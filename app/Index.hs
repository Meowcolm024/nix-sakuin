{-# LANGUAGE QuasiQuotes #-}

module Index where

import Cli
import Control.Exception (bracket)
import Data.ByteString.Lazy qualified as LBS
import Data.List (nub)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Effectful
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Error.Static (runErrorNoCallStackWith)
import Effectful.Reader.Static (Reader, runReader)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Path
import Path.IO
import Sakuin
import Sakuin.Database
import Sakuin.FetchCache
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

withFetchCache ::
  forall es a.
  (Concurrent :> es, Reader Manager :> es, IOE :> es, Log :> es) =>
  Bool -> Eff (Fetch : es) a -> Eff es a
withFetchCache enabled action
  | not enabled = runHydra action
  | otherwise = do
      logInfo "loading fetch cache"
      initial <- liftIO loadFetchCache
      (result, finalCache) <- runHydraFetchCache initial action
      logInfo "writing fetch cache"
      liftIO $ writeFetchCache finalCache
      pure result

runIndex :: IndexOptions -> IO ()
runIndex opts = do
  mgr <- newTlsManager
  databaseDir <- resolveDatabaseDir (indexDatabase opts)
  let writeQueueCapacity = max 1 (indexWorker opts * 2)

  size <- bracket (setupLogger (indexVerbose opts)) (const cleanupLogger) $ \logger ->
    runEff
      . runErrorNoCallStackWith @NixEnvError (liftIO . exitErrorIO)
      . runErrorNoCallStackWith @PipelineError (liftIO . exitErrorIO)
      . runConcurrent
      . runReader mgr
      . runLog logger
      . withTsvDatabase writeQueueCapacity (toFilePath $ databasePath databaseDir)
      $ \database -> do
        -- Nothing represents the default scope
        let scopes = nub $ (if indexNoDefaultScope opts then [] else [Nothing]) <> map Just (indexExtraScopes opts)
        logInfo "querying root packages"
        pkgs@(Packages pkgs') <- queryAllScopes (indexNixpkgsPath opts) (indexSystem opts) scopes
        logInfo $ "root package count: " <> T.show (length pkgs')
        withFetchCache (indexFetchCache opts) . runTsvDatabase database $
          runPipeline
            defaultPipelineConfig
              { pipelineWorkerCount = indexWorker opts,
                pipelineFilterPrefix = indexFilterPrefix opts,
                pipelineIndexedCount = Just $ readTsvEntryCount database
              }
            pkgs
        readTsvEntryCount database

  T.putStrLn $ "summary: " <> T.show size <> " paths indexed"
  hFlush stdout
