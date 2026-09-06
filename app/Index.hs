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

getCacheDir :: IO (Path Abs Dir)
getCacheDir = do
  xdgCache <- getXdgDir XdgCache (parseRelDir "nix-sakuin")
  createDirIfMissing False xdgCache
  pure xdgCache

runIndex :: IndexOptions -> IO ()
runIndex opts = do
  mgr <- newTlsManager
  cacheDir <- maybe getCacheDir pure (indexDatabase opts)
  initialFetchCache <- if indexFetchCache opts then Just <$> loadFetchCache else pure Nothing
  size <- bracket (setupLogger (indexVerbose opts)) (const cleanupLogger) $ \logger ->
    runEff
      . runFailIO
      . runConcurrent
      . runReader mgr
      . runLog logger
      $ do
        database <- newMemoryDatabase
        fetchCache <- traverse newFetchCache initialFetchCache
        runMemoryDatabase database . runHydra fetchCache $ do
          -- NOTE: Nothing represents the default scope
          let scopes = nub $ (if indexNoDefaultScope opts then [] else [Nothing]) <> map Just (indexExtraScopes opts)
          pkgs@(Packages pkgs') <- queryAllScopes "<nixpkgs>" (indexSystem opts) scopes
          logInfo $ "root package count: " <> T.show (length pkgs')
          runPipelineWithProgress (indexWorker opts) (Map.size <$> readMemoryDatabase database) pkgs
        finalDb <- readMemoryDatabase database
        finalFetchCache <- traverse readFetchCache fetchCache
        forM_ finalFetchCache $ \c -> do
          logInfo "writing cache"
          liftIO $ writeFetchCache c
        logInfo "writing database"
        liftIO $
          LBS.writeFile
            (fromAbsFile $ cacheDir </> [relfile|database.jsonl.zst|])
            (encodeDatabase finalDb)
        pure $ Map.size finalDb
  T.putStrLn $ "summary: " <> T.show size <> " paths indexed"
  hFlush stdout
