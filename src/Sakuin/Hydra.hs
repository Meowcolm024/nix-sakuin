module Sakuin.Hydra where

import Codec.Compression.Lzma qualified as Lzma
import Codec.Compression.Zstd.Lazy qualified as Zstd
import Control.Concurrent
import Control.Exception
import Control.Monad.Catch qualified as Catch
import Control.Monad.IO.Unlift
import Data.Aeson (eitherDecode')
import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Client
import Network.HTTP.Types.Status
import Network.URI (URI, parseURI)
import Sakuin.Types
import System.Random (randomRIO)

newtype CacheIO a = CacheIO {runCacheIO :: ReaderT Manager IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader Manager,
      MonadUnliftIO,
      Catch.MonadThrow,
      Catch.MonadCatch,
      Catch.MonadMask
    )

instance MonadCache CacheIO where
  fetchNarinfo storePath = do
    mgr <- ask
    let uri = "https://cache.nixos.org/" <> toString (spHash storePath) <> ".narinfo"
    bs <- liftIO $ fetchUri mgr uri
    pure (bs >>= parseNarInfo . toStrict)

  fetchListing storePath = do
    mgr <- ask
    let base = "https://cache.nixos.org/" <> toString (spHash storePath)
    generic <- liftIO $ fetchUri mgr (base <> ".ls")
    body <- case generic of
      Just bytes -> pure (Just bytes)
      Nothing -> liftIO $ fetchUri mgr (base <> ".ls.xz")
    case traverse (parseListing . decodeListing) body of
      Left err -> liftIO $ fail err
      Right listing -> pure listing

fetchUri :: Manager -> String -> IO (Maybe LByteString)
fetchUri mgr uri = parseURI' uri >>= (`fetch` mgr)
  where
    -- TODO actual error handling
    parseURI' :: String -> IO URI
    parseURI' plain = case parseURI plain of
      Nothing -> liftIO $ fail "invalid uri"
      Just uri' -> pure uri'

decodeListing :: LByteString -> LByteString
decodeListing bytes
  | zstdMagic `LBS.isPrefixOf` bytes = Zstd.decompress bytes
  | xzMagic `LBS.isPrefixOf` bytes = Lzma.decompress bytes
  | otherwise = bytes
  where
    zstdMagic = LBS.pack [0x28, 0xB5, 0x2F, 0xFD]
    xzMagic = LBS.pack [0xFD, 0x37, 0x7A, 0x58, 0x5A, 0x00]

parseListing :: LByteString -> Either String FileNode
parseListing bytes = root <$> eitherDecode' bytes

-- simple fetch without retry
fetchNoRetry :: URI -> Manager -> IO (Status, LByteString)
fetchNoRetry uri mgr = do
  req <- requestFromURI uri
  response <- httpLbs (req {checkResponse = \_ _ -> pure ()}) mgr
  pure (responseStatus response, responseBody response)

fetch :: URI -> Manager -> IO (Maybe LByteString)
fetch uri mgr = go 0
  where
    maxAttempts = 5 :: Int
    -- simple retry logic
    retry attempt
      | attempt + 1 >= maxAttempts = pure Nothing
      | otherwise = do
          let maximumDelay = min 5000000 (50000 * (2 ^ attempt))
          delay <- randomRIO (0, maximumDelay)
          threadDelay delay
          go (attempt + 1)
    go attempt = do
      result <- try @HttpException (fetchNoRetry uri mgr)
      case result of
        Left _ -> retry attempt
        Right (status, body)
          | statusIsSuccessful status -> pure (Just body)
          | status == status404 -> pure Nothing
          | status == status408 || status == status429 || statusIsServerError status -> retry attempt
          | otherwise -> pure Nothing
