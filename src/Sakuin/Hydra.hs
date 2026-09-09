module Sakuin.Hydra where

import Codec.Compression.Brotli qualified as Brotli
import Codec.Compression.Lzma qualified as Lzma
import Codec.Compression.Zstd.Lazy qualified as Zstd
import Control.Arrow (ArrowChoice (left))
import Control.Retry qualified as Retry
import Data.Aeson (throwDecode)
import Data.ByteString.Lazy qualified as LBS
import Data.Either (isLeft)
import Data.Map (Map)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Exception
import Network.HTTP.Client (Manager, Response, responseHeaders, responseStatus)
import Network.HTTP.Req qualified as Req
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Sakuin.FetchCache
import Sakuin.Log
import Sakuin.Types

cacheUri :: Req.Url Req.Https
cacheUri = Req.https "cache.nixos.org"

runHydra ::
  forall es a.
  (Concurrent :> es, IOE :> es, Log :> es) =>
  Manager -> Eff (Fetch : es) a -> Eff es a
runHydra manager = interpret $ \_ -> \case
  FetchNarInfo storePath -> fetchNarInfoFromHydra manager storePath
  FetchListing storePath -> fetchListingFromHydra manager storePath

runHydraFetchCache ::
  forall es a.
  (Concurrent :> es, IOE :> es, Log :> es) =>
  Map StoreHash FetchCacheEntry ->
  Manager ->
  Eff (Fetch : es) a ->
  Eff es (a, Map StoreHash FetchCacheEntry)
runHydraFetchCache initial manager action = do
  cache <- newFetchCacheState initial
  result <-
    interpret
      ( \_ -> \case
          FetchNarInfo storePath -> do
            cached <- lookupCachedNarInfo cache (spHash storePath)
            case cached of
              Found bytes -> case parseNarInfo bytes of
                Just narinfo -> pure (Just narinfo)
                Nothing -> fetchAndCacheNarInfo cache manager storePath
              Missing -> pure Nothing
              NotFetched -> fetchAndCacheNarInfo cache manager storePath
          FetchListing storePath -> do
            cached <- lookupCachedListing cache (spHash storePath)
            case cached of
              Found bytes -> do
                decodeListing (LBS.fromStrict bytes) >>= \case
                  Right listing -> pure $ Just listing
                  Left err -> do
                    logWarn $ "discarding invalid cached listing for hash " <> spHash storePath <> ": " <> T.pack err
                    fetchAndCacheListing cache manager storePath
              Missing -> pure Nothing
              NotFetched -> fetchAndCacheListing cache manager storePath
      )
      action
  finalCache <- readFetchCacheState cache
  pure (result, finalCache)

fetchNarInfoFromHydra ::
  forall es.
  (Concurrent :> es, IOE :> es, Log :> es) => Manager -> StorePath -> Eff es (Maybe NarInfo)
fetchNarInfoFromHydra manager storePath = do
  raw <- fetch manager (spHash storePath <> ".narinfo")
  pure $ LBS.toStrict <$> raw >>= parseNarInfo

fetchAndCacheNarInfo ::
  forall es.
  (Concurrent :> es, IOE :> es, Log :> es) => FetchCacheState -> Manager -> StorePath -> Eff es (Maybe NarInfo)
fetchAndCacheNarInfo cache manager storePath = do
  raw <- fetch manager (spHash storePath <> ".narinfo")
  case raw of
    Nothing -> pure Nothing
    Just bytes -> case parseNarInfo (LBS.toStrict bytes) of
      Nothing -> pure Nothing
      Just narinfo -> do
        storeCachedNarInfo cache (spHash storePath) (Found $ LBS.toStrict bytes)
        pure $ Just narinfo

fetchListingFromHydra ::
  forall es.
  (Concurrent :> es, IOE :> es, Log :> es) => Manager -> StorePath -> Eff es (Maybe FileNode)
fetchListingFromHydra manager storePath = do
  body <- fetchListingBytes manager storePath
  decodeFetchedListing storePath body

fetchListingBytes ::
  forall es.
  (Concurrent :> es, IOE :> es, Log :> es) => Manager -> StorePath -> Eff es (Maybe LBS.ByteString)
fetchListingBytes manager storePath = do
  let base = spHash storePath
  fetch manager (base <> ".ls") >>= \case
    Just bytes -> pure (Just bytes)
    Nothing -> fetch manager (base <> ".ls.xz")

decodeFetchedListing ::
  forall es.
  (Concurrent :> es, Log :> es) => StorePath -> Maybe LBS.ByteString -> Eff es (Maybe FileNode)
decodeFetchedListing storePath body = do
  result <- case body of
    Nothing -> pure $ Right Nothing
    Just bytes -> fmap Just <$> decodeListing bytes
  case result of
    Left err -> do
      logErr $ "failed to process listing for hash " <> spHash storePath <> ": " <> T.pack err
      pure Nothing
    Right listing -> pure listing

fetchAndCacheListing ::
  forall es.
  (Concurrent :> es, IOE :> es, Log :> es) => FetchCacheState -> Manager -> StorePath -> Eff es (Maybe FileNode)
fetchAndCacheListing cache manager storePath = do
  body <- fetchListingBytes manager storePath
  listing <- decodeFetchedListing storePath body
  case (body, listing) of
    (Just bytes, Just files) -> do
      storeCachedListing cache (spHash storePath) (Found $ LBS.toStrict bytes)
      pure $ Just files
    _ -> pure Nothing

decodeListing :: forall es. LBS.ByteString -> Eff es (Either String FileNode)
decodeListing bytes =
  left displayException <$> tryJust synchronous (parseListing decoded)
  where
    decoded
      | zstdMagic `LBS.isPrefixOf` bytes = Zstd.decompress bytes
      | xzMagic `LBS.isPrefixOf` bytes = Lzma.decompress bytes
      | otherwise = bytes
    synchronous err = case fromException @SomeAsyncException err of
      Just _ -> Nothing
      Nothing -> Just err
    zstdMagic = LBS.pack [0x28, 0xB5, 0x2F, 0xFD]
    xzMagic = LBS.pack [0xFD, 0x37, 0x7A, 0x58, 0x5A, 0x00]

parseListing :: forall es. LBS.ByteString -> Eff es FileNode
parseListing bytes = root <$> throwDecode bytes

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
              pure $ Right Nothing -- not cached/available in hydra
          | status == status408 || status == status429 || statusIsServerError status ->
              pure . Left $ "HTTP " <> T.pack (show $ statusCode status)
          | otherwise -> do
              logWarn $ "failed fetching " <> uri <> ": HTTP " <> T.pack (show $ statusCode status)
              pure $ Right Nothing
