module Sakuin.Hydra where

import Codec.Compression.Brotli qualified as Brotli
import Codec.Compression.Lzma qualified as Lzma
import Codec.Compression.Zstd.Lazy qualified as Zstd
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Exception (displayException, try)
import Effectful.Fail
import Effectful.Reader.Static
import Network.HTTP.Client
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.URI (URI, parseURI)
import Sakuin.Log (Log, logErr, logWarn)
import Sakuin.Types
import System.Random (randomRIO)

runHydra ::
  forall es a.
  (Concurrent :> es, Reader Manager :> es, IOE :> es, Fail :> es, Log :> es) =>
  Eff (Fetch : es) a -> Eff es a
runHydra = interpret $ \_ -> \case
  FetchNarInfo storePath -> do
    mgr <- ask
    let uri = "https://cache.nixos.org/" <> spHash storePath <> ".narinfo"
    bs <- fetchUri mgr uri
    pure (bs >>= parseNarInfo . LBS.toStrict)
  FetchListing storePath -> do
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
      Right listing -> pure listing

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
          | status == status404 -> pure Nothing
          | status == status408 || status == status429 || statusIsServerError status ->
              retry attempt ("HTTP " <> T.pack (show $ statusCode status))
          | otherwise -> do
              logWarn $ "failed fetching " <> T.pack (show uri) <> ": HTTP " <> T.pack (show $ statusCode status)
              pure Nothing
