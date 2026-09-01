module Sakuin.Hydra where

import Control.Concurrent
import Control.Exception
import Network.HTTP.Client (Manager, Response (responseBody), httpLbs, requestFromURI)
import Network.URI (URI, parseURI)
import Sakuin.Types
import System.Random (randomRIO)

newtype CacheIO a = CacheIO {runCacheIO :: ReaderT Manager IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Manager)

instance MonadCache CacheIO where
  fetchNarinfo se = do
    mgr <- ask
    case parseURI $ "https://cache.nixos.org/" <> toString (spHash (storePath se)) <> ".narinfo" of
      Nothing -> liftIO $ fail "invalid uri"
      Just uri -> do
        bs <- liftIO $ fetch uri mgr
        pure (bs >>= parseNarInfoWith se)

  fetchListing _ = pure Nothing

-- simple fetch without retry
fetchNoRetry :: URI -> Manager -> IO ByteString
fetchNoRetry uri mgr = do
  req <- requestFromURI uri
  response <- httpLbs req mgr
  pure $ toStrict (responseBody response)

fetch :: URI -> Manager -> IO (Maybe ByteString)
fetch uri mgr = go 0
  where
    -- simple retry logic
    backoff n = min 5000000 (50000 * (2 ^ n))
    jitter delay = randomRIO (0, delay)
    go (n :: Int) = do
      result <- try @SomeException (fetchNoRetry uri mgr)
      case result of
        Right value -> pure (Just value)
        Left _ ->
          if n + 1 >= 20
            then pure Nothing
            else do
              delay <- jitter (backoff n)
              threadDelay $ delay + 5000000
              go (n + 1)
