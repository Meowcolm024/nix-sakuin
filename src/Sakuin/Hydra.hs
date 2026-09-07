module Sakuin.Hydra where

import Codec.Compression.Brotli qualified as Brotli
import Codec.Compression.Lzma qualified as Lzma
import Codec.Compression.Zstd.Lazy qualified as Zstd
import Control.Monad (forM_)
import Control.Retry qualified as Retry
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Either (isLeft)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Exception (displayException, try)
import Effectful.Reader.Static
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, Response, responseHeaders, responseStatus)
import Network.HTTP.Req qualified as Req
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Sakuin.Log (Log, logErr, logWarn)
import Sakuin.Types

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

cacheUri :: Req.Url Req.Https
cacheUri = Req.https "cache.nixos.org"

runHydra ::
  forall es a.
  (Concurrent :> es, Reader Manager :> es, IOE :> es, Log :> es) =>
  Maybe FetchCache -> Eff (Fetch : es) a -> Eff es a
runHydra fetchCache = interpret $ \_ -> \case
  FetchNarInfo storePath -> do
    cached <- traverse (\cache -> lookupNarInfo cache (spHash storePath)) fetchCache
    case cached of
      Just (Found narinfo) -> pure (Just narinfo)
      Just Missing -> pure Nothing
      _ -> do
        mgr <- ask
        result <- (>>= parseNarInfo . LBS.toStrict) <$> fetch mgr (spHash storePath <> ".narinfo")
        forM_ fetchCache $ \cache -> storeNarInfo cache (spHash storePath) result
        pure result
  FetchListing storePath -> do
    cached <- traverse (\cache -> lookupListing cache (spHash storePath)) fetchCache
    case cached of
      Just (Found listing) -> pure (Just listing)
      Just Missing -> pure Nothing
      _ -> do
        mgr <- ask
        let base = spHash storePath
        generic <- fetch mgr (base <> ".ls")
        body <- case generic of
          Just bytes -> pure (Just bytes)
          Nothing -> fetch mgr (base <> ".ls.xz")
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
  forall es. (Concurrent :> es, IOE :> es) => Text -> Manager -> Eff es (Status, ResponseHeaders, LBS.ByteString)
fetchNoRetry path mgr = liftIO . Req.runReq config $ do
  response <- Req.req Req.GET (cacheUri Req./: path) Req.NoReqBody Req.lbsResponse mempty
  let vanillaResponse = Req.toVanillaResponse response :: Response LBS.ByteString
  pure (responseStatus vanillaResponse, responseHeaders vanillaResponse, Req.responseBody response)
  where
    config =
      Req.defaultHttpConfig
        { Req.httpConfigAltManager = Just mgr,
          Req.httpConfigCheckResponse = \_ _ _ -> Nothing,
          Req.httpConfigRetryJudge = \_ _ -> False,
          Req.httpConfigRetryJudgeException = \_ _ -> False
        }

-- workaround for brotli compression from cache.nixos.org
decodeResponseBody :: ResponseHeaders -> LBS.ByteString -> LBS.ByteString
decodeResponseBody headers body
  | lookup hContentEncoding headers == Just "br" = Brotli.decompress body
  | otherwise = body

fetch :: forall es. (Concurrent :> es, IOE :> es, Log :> es) => Manager -> Text -> Eff es (Maybe LBS.ByteString)
fetch mgr path = do
  result <- Retry.retrying policy (\_ -> pure . isLeft) (const attempt)
  case result of
    Left failure -> do
      logWarn $ "giving up fetching " <> uri <> ": " <> failure
      pure Nothing
    Right body -> pure body
  where
    uri = Req.renderUrl (cacheUri Req./: path)
    policy = Retry.capDelay 5000000 (Retry.fullJitterBackoff 50000) <> Retry.limitRetries 4
    attempt = do
      result <- try @Req.HttpException (fetchNoRetry path mgr)
      case result of
        Left err -> pure . Left . T.pack $ displayException err
        Right (status, headers, body)
          | statusIsSuccessful status -> pure . Right . Just $ decodeResponseBody headers body
          | status == status404 ->
              -- not cached/available in hydra
              pure $ Right Nothing
          | status == status408 || status == status429 || statusIsServerError status ->
              pure . Left $ "HTTP " <> T.pack (show $ statusCode status)
          | otherwise -> do
              logWarn $ "failed fetching " <> uri <> ": HTTP " <> T.pack (show $ statusCode status)
              pure $ Right Nothing
