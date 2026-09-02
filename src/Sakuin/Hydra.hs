module Sakuin.Hydra where

import Control.Concurrent
import Control.Exception
import Network.HTTP.Client
import Network.HTTP.Types.Status
import Network.URI (URI, parseURI)
import Sakuin.Types
import System.Random (randomRIO)

newtype CacheIO a = CacheIO {runCacheIO :: ReaderT Manager IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Manager)

instance MonadCache CacheIO where
  fetchNarinfo storePath = do
    mgr <- ask
    case parseURI $ "https://cache.nixos.org/" <> toString (spHash storePath) <> ".narinfo" of
      Nothing -> liftIO $ fail "invalid uri"
      Just uri -> do
        bs <- liftIO $ fetch uri mgr
        pure (bs >>= parseNarInfo)

  fetchListing _ = pure Nothing

-- simple fetch without retry
fetchNoRetry :: URI -> Manager -> IO (Status, ByteString)
fetchNoRetry uri mgr = do
  req <- requestFromURI uri
  response <- httpLbs (req {checkResponse = \_ _ -> pure ()}) mgr
  pure (responseStatus response, toStrict (responseBody response))

fetch :: URI -> Manager -> IO (Maybe ByteString)
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
