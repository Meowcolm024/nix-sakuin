{-# LANGUAGE QuasiQuotes #-}

module Index where

import Cli
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.ByteString.Lazy qualified as LBS
import Data.List (nub)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Effectful
import Effectful.Concurrent.Async
import Effectful.Fail
import Effectful.Reader.Static (runReader)
import Network.HTTP.Client.TLS
import Path
import Path.IO
import Sakuin
import Sakuin.Database
import Sakuin.Hydra
import Storage
import System.IO (hClose, hFlush, stdout)

fetchCachePath :: IO (Path Abs File)
fetchCachePath = do
  tmpDir <- getTempDir
  pure $ tmpDir </> [relfile|nix-sakuin-fetch-cache.json.zst|]

loadFetchCache :: IO (Map.Map StoreHash FetchCacheEntry)
loadFetchCache = do
  cachePath <- fetchCachePath
  exists <- doesFileExist cachePath
  if exists
    then either fail pure . decodeFetchCache =<< LBS.readFile (toFilePath cachePath)
    else pure Map.empty

writeFetchCache :: Map.Map StoreHash FetchCacheEntry -> IO ()
writeFetchCache entries = do
  cachePath <- fetchCachePath
  tmpDir <- getTempDir
  (temporaryPath, handle) <- openBinaryTempFile tmpDir "nix-sakuin-fetch-cache.tmp"
  LBS.hPut handle (encodeFetchCache entries)
  hClose handle
  renameFile temporaryPath cachePath

runIndex :: IndexOptions -> IO ()
runIndex opts = do
  mgr <- newTlsManager
  databaseDir <- resolveDatabaseDir (indexDatabase opts)

  let writeQueueCapacity = max 1 (indexWorker opts * 2)
  size <- bracket (setupLogger (indexVerbose opts)) (const cleanupLogger) $ \logger ->
    runEff
      . runFailIO
      . runConcurrent
      . runReader mgr
      . runLog logger
      $ withTsvDatabase writeQueueCapacity (toFilePath $ databasePath databaseDir)
      $ \database -> do
        let loadCache = do
              logInfo "loading fetch cache"
              initialFetchCache <- liftIO $ if indexFetchCache opts then Just <$> loadFetchCache else pure Nothing
              traverse newFetchCache initialFetchCache
            -- NOTE: Nothing represents the default scope
            scopes = nub $ (if indexNoDefaultScope opts then [] else [Nothing]) <> map Just (indexExtraScopes opts)
            queryScopes = do
              logInfo "querying root packages"
              queryAllScopes "<nixpkgs>" (indexSystem opts) scopes
        (fetchCache, pkgs@(Packages pkgs')) <- concurrently loadCache queryScopes
        logInfo $ "root package count: " <> T.show (length pkgs')
        runTsvDatabase database . runHydra fetchCache $ do
          runPipelineWithProgress (indexWorker opts) (readTsvEntryCount database) pkgs
        finalFetchCache <- traverse readFetchCache fetchCache
        forM_ finalFetchCache $ \cache -> do
          logInfo "writing fetch cache"
          liftIO $ writeFetchCache cache
        readTsvEntryCount database
  T.putStrLn $ "summary: " <> T.show size <> " paths indexed"
  hFlush stdout
