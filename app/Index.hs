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
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Path
import Path.IO
import Sakuin
import Sakuin.FetchCache
import Storage
import System.IO (hClose, hFlush, stdout)

loadFetchCache ::
  forall es. (IOE :> es, Log :> es) => Path Abs File -> Eff es (Map.Map StoreHash FetchCacheEntry)
loadFetchCache cachePath = do
  exists <- doesFileExist cachePath
  if exists
    then
      decodeFetchCache <$> liftIO (LBS.readFile (toFilePath cachePath)) >>= \case
        Left err -> logWarn err *> pure Map.empty
        Right c -> pure c
    else pure Map.empty

writeFetchCache ::
  forall es. (IOE :> es) => Path Abs File -> Map.Map StoreHash FetchCacheEntry -> Eff es ()
writeFetchCache cachePath entries = do
  tmpDir <- getTempDir
  (temporaryPath, handle) <- openBinaryTempFile tmpDir "nix-sakuin-fetch-cache.tmp"
  liftIO $ LBS.hPut handle (encodeFetchCache entries) *> hClose handle
  renameFile temporaryPath cachePath

withFetchCache ::
  forall es a.
  (Concurrent :> es, IOE :> es, Log :> es) => Bool -> Manager -> Eff (Fetch : es) a -> Eff es a
withFetchCache enabled mgr action
  | not enabled = runHydra mgr action
  | otherwise = do
      cachePath <- liftIO fetchCachePath
      logInfo "loading fetch cache"
      initial <- loadFetchCache cachePath
      (result, finalCache) <- runHydraFetchCache initial mgr action
      logInfo "writing fetch cache"
      writeFetchCache cachePath finalCache
      pure result

runIndex :: IndexOptions -> IO ()
runIndex opts = do
  manager <- newTlsManager
  databaseDir <- resolveDatabaseDir (indexDatabase opts)
  let writeQueueCapacity = max 1 (indexWorker opts * 2)
  -- Nothing represents the default scope
  let scopes = nub $ (if indexNoDefaultScope opts then [] else [Nothing]) <> map Just (indexExtraScopes opts)

  size <- bracket (setupLogger (indexVerbose opts)) (const cleanupLogger) $ \logger ->
    runEff
      . runErrorNoCallStackWith @NixEnvError (liftIO . exitErrorIO)
      . runErrorNoCallStackWith @PipelineError (liftIO . exitErrorIO)
      . runConcurrent
      . runLog logger
      . withTsvDatabase writeQueueCapacity (toFilePath $ databasePath databaseDir)
      $ \database -> do
        liftIO $ T.putStrLn "querying root packages"
        logInfo $ "root packages scopes: " <> T.intercalate ", " (map (maybe "(default)" id) scopes)
        pkgs@(Packages pkgs') <- queryAllScopes (indexNixpkgsPath opts) (indexSystem opts) scopes
        logInfo $ "root packages count: " <> T.show (length pkgs')
        withFetchCache (indexFetchCache opts) manager . runTsvDatabase database $
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
