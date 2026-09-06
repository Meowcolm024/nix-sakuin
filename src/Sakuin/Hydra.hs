module Sakuin.Hydra where

import Codec.Compression.Brotli qualified as Brotli
import Codec.Compression.Lzma qualified as Lzma
import Codec.Compression.Zstd.Lazy qualified as Zstd
import Control.Monad (forM_)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Exception (displayException, try)
import Effectful.Fail
import Effectful.Reader.Static
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.URI (URI, parseURI)
import Sakuin.Log (Log, logErr, logWarn)
import Sakuin.Types
import System.Random (randomRIO)

data Cached a
  = NotFetched
  | Missing
  | Found a
  deriving stock (Show, Eq, Generic)

instance (ToJSON a) => ToJSON (Cached a)

instance (FromJSON a) => FromJSON (Cached a)

data FetchCacheEntry = FetchCacheEntry
  { cachedNarInfo :: Cached NarInfo,
    cachedListing :: Cached FileNode
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON FetchCacheEntry

instance FromJSON FetchCacheEntry

newtype FetchCache = FetchCache
  { fetchCacheEntries :: TVar (Map StoreHash FetchCacheEntry)
  }

emptyFetchCacheEntry :: FetchCacheEntry
emptyFetchCacheEntry = FetchCacheEntry NotFetched NotFetched

newFetchCache :: forall es. (Concurrent :> es) => Map StoreHash FetchCacheEntry -> Eff es FetchCache
newFetchCache entries = FetchCache <$> newTVarIO entries

readFetchCache :: forall es. (Concurrent :> es) => FetchCache -> Eff es (Map StoreHash FetchCacheEntry)
readFetchCache = readTVarIO . fetchCacheEntries

encodeFetchCache :: Map StoreHash FetchCacheEntry -> LBS.ByteString
encodeFetchCache = Zstd.compress 3 . encode

decodeFetchCache :: LBS.ByteString -> Either String (Map StoreHash FetchCacheEntry)
decodeFetchCache = eitherDecode . Zstd.decompress

runHydra ::
  forall es a.
  (Concurrent :> es, Reader Manager :> es, IOE :> es, Fail :> es, Log :> es) =>
  Maybe FetchCache -> Eff (Fetch : es) a -> Eff es a
runHydra fetchCache = interpret $ \_ -> \case
  FetchNarInfo storePath -> do
    cached <- traverse (\cache -> lookupNarInfo cache (spHash storePath)) fetchCache
    case cached of
      Just (Found narinfo) -> pure (Just narinfo)
      Just Missing -> pure Nothing
      _ -> do
        mgr <- ask
        let uri = "https://cache.nixos.org/" <> spHash storePath <> ".narinfo"
        result <- (>>= parseNarInfo . LBS.toStrict) <$> fetchUri mgr uri
        forM_ fetchCache $ \cache -> storeNarInfo cache (spHash storePath) result
        pure result
  FetchListing storePath -> do
    cached <- traverse (\cache -> lookupListing cache (spHash storePath)) fetchCache
    case cached of
      Just (Found listing) -> pure (Just listing)
      Just Missing -> pure Nothing
      _ -> do
        mgr <- ask
        let base = "https://cache.nixos.org/" <> spHash storePath
        generic <- fetchUri mgr (base <> ".ls")
        body <- case generic of
          Just bytes -> pure (Just bytes)
          Nothing -> fetchUri mgr (base <> ".ls.xz")
        case traverse (parseListing . decodeListing) body of
          Left _ -> do
            logErr $ "fail to process listing for hash: " <> spHash storePath
            pure Nothing
          Right listing -> do
            forM_ fetchCache $ \cache -> storeListing cache (spHash storePath) listing
            pure listing

lookupNarInfo :: forall es. (Concurrent :> es) => FetchCache -> StoreHash -> Eff es (Cached NarInfo)
lookupNarInfo cache storeHash =
  cachedNarInfo . Map.findWithDefault emptyFetchCacheEntry storeHash <$> readTVarIO (fetchCacheEntries cache)

lookupListing :: forall es. (Concurrent :> es) => FetchCache -> StoreHash -> Eff es (Cached FileNode)
lookupListing cache storeHash =
  cachedListing . Map.findWithDefault emptyFetchCacheEntry storeHash <$> readTVarIO (fetchCacheEntries cache)

storeNarInfo :: forall es. (Concurrent :> es) => FetchCache -> StoreHash -> Maybe NarInfo -> Eff es ()
storeNarInfo cache storeHash result =
  forM_ result $ \narinfo ->
    atomically . modifyTVar' (fetchCacheEntries cache) $
      Map.alter (Just . setNarInfo narinfo . maybe emptyFetchCacheEntry id) storeHash
  where
    setNarInfo narinfo entry = entry {cachedNarInfo = Found narinfo}

storeListing :: forall es. (Concurrent :> es) => FetchCache -> StoreHash -> Maybe FileNode -> Eff es ()
storeListing cache storeHash result =
  forM_ result $ \listing ->
    atomically . modifyTVar' (fetchCacheEntries cache) $
      Map.alter (Just . setListing listing . maybe emptyFetchCacheEntry id) storeHash
  where
    setListing listing entry = entry {cachedListing = Found listing}

fetchUri ::
  forall es.
  (Concurrent :> es, IOE :> es, Fail :> es, Log :> es) =>
  Manager -> Text -> Eff es (Maybe LBS.ByteString)
fetchUri mgr uri = parseURI' uri >>= (`fetch` mgr)
  where
    -- TODO actual error handling
    parseURI' plain = case parseURI (T.unpack plain) of
      Nothing -> fail "invalid uri"
      Just uri' -> pure uri'

decodeListing :: LBS.ByteString -> LBS.ByteString
decodeListing bytes
  | zstdMagic `LBS.isPrefixOf` bytes = Zstd.decompress bytes
  | xzMagic `LBS.isPrefixOf` bytes = Lzma.decompress bytes
  | otherwise = bytes
  where
    zstdMagic = LBS.pack [0x28, 0xB5, 0x2F, 0xFD]
    xzMagic = LBS.pack [0xFD, 0x37, 0x7A, 0x58, 0x5A, 0x00]

parseListing :: LBS.ByteString -> Either String FileNode
parseListing bytes = root <$> eitherDecode bytes

-- simple fetch without retry
fetchNoRetry ::
  forall es. (Concurrent :> es, IOE :> es) => URI -> Manager -> Eff es (Status, ResponseHeaders, LBS.ByteString)
fetchNoRetry uri mgr = do
  req <- requestFromURI uri
  response <- liftIO $ httpLbs (req {checkResponse = \_ _ -> pure ()}) mgr
  pure (responseStatus response, responseHeaders response, responseBody response)

-- workaround for brotli compression from cache.nixos.org
decodeResponseBody :: ResponseHeaders -> LBS.ByteString -> LBS.ByteString
decodeResponseBody headers body
  | lookup hContentEncoding headers == Just "br" = Brotli.decompress body
  | otherwise = body

fetch :: forall es. (Concurrent :> es, IOE :> es, Log :> es) => URI -> Manager -> Eff es (Maybe LBS.ByteString)
fetch uri mgr = go 0
  where
    maxAttempts = 5 :: Int
    -- simple retry logic
    retry attempt failure
      | attempt + 1 >= maxAttempts = do
          logWarn $ "giving up fetching " <> T.pack (show uri) <> ": " <> failure
          pure Nothing
      | otherwise = do
          let maximumDelay = min 5000000 (50000 * (2 ^ attempt))
          delay <- liftIO $ randomRIO (0, maximumDelay)
          threadDelay delay
          go (attempt + 1)
    go attempt = do
      result <- try @HttpException (fetchNoRetry uri mgr)
      case result of
        Left err -> retry attempt (T.pack $ displayException err)
        Right (status, headers, body)
          | statusIsSuccessful status -> pure . Just $ decodeResponseBody headers body
          | status == status404 ->
              -- not cached/available in hydra
              pure Nothing
          | status == status408 || status == status429 || statusIsServerError status ->
              retry attempt ("HTTP " <> T.pack (show $ statusCode status))
          | otherwise -> do
              logWarn $ "failed fetching " <> T.pack (show uri) <> ": HTTP " <> T.pack (show $ statusCode status)
              pure Nothing
