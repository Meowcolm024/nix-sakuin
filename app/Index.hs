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
import Sakuin.Storage
import System.IO (hFlush, stdout)

withFetchCache ::
  forall es a.
  (Concurrent :> es, IOE :> es, Log :> es) => Bool -> Manager -> Eff (Fetch : es) a -> Eff es a
withFetchCache enabled mgr action
  | not enabled = runHydra mgr action
  | otherwise = do
      logInfo "loading fetch cache"
      cachePath <- fetchCachePath
      exists <- doesFileExist cachePath
      initial <-
        if exists
          then
            decodeFetchCache <$> liftIO (LBS.readFile (toFilePath cachePath)) >>= \case
              Left err -> logWarn err *> pure Map.empty
              Right c -> pure c
          else pure Map.empty
      (result, finalCache) <- runHydraFetchCache initial mgr action
      logInfo "writing fetch cache"
      withAtomicFile cachePath $ \handle ->
        liftIO $ LBS.hPut handle (encodeFetchCache finalCache)
      pure result

runIndex :: IndexOptions -> IO ()
runIndex opts = do
  let writeQueueCapacity = max 1 (indexWorker opts * 2)
  -- Nothing represents the default scope
  let scopes = nub $ (if indexNoDefaultScope opts then [] else [Nothing]) <> map Just (indexExtraScopes opts)
  manager <- newTlsManager
  size <- bracket (setupLogger (indexVerbose opts)) (const cleanupLogger) $ \logger ->
    runEff
      . runErrorNoCallStackWith @NixEnvError (liftIO . exitErrorIO)
      . runErrorNoCallStackWith @PipelineError (liftIO . exitErrorIO)
      . runConcurrent
      . runLog logger
      $ do
        databaseDir <- resolveDatabaseDir (indexDatabase opts)
        withAtomicFile (databasePath databaseDir) $ \handle ->
          withTsvDatabase writeQueueCapacity handle $ \database -> do
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
