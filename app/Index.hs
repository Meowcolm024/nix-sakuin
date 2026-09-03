{-# LANGUAGE TemplateHaskell #-}

module Index where

import Cli
import Control.Exception (bracket)
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
import Sakuin
import Sakuin.Database
import Sakuin.Hydra
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, getXdgDirectory)
import System.IO

getCacheDir :: IO (Path Abs Dir)
getCacheDir = do
  xdgCache <- parseAbsDir =<< getXdgDirectory XdgCache "nix-sakuin"
  createDirectoryIfMissing False (fromAbsDir xdgCache)
  pure xdgCache

runIndex :: IndexOptions -> IO ()
runIndex opts = do
  mgr <- newTlsManager
  cacheDir <- maybe getCacheDir pure (indexDatabase opts)
  result <- bracket (setupLogger (indexVerbose opts)) (const cleanupLogger) $ \logger ->
    runEff
      . runFailIO
      . runConcurrent
      . runReader mgr
      . runLog logger
      $ do
        database <- newMemoryDatabase
        runMemoryDatabase database . runHydra $ do
          -- NOTE: Nothing represents the default scope
          let scopes = nub $ (if indexNoDefaultScope opts then [] else [Nothing]) <> map Just (indexExtraScopes opts)
          pkgs@(Packages pkgs') <- queryAllScopes "<nixpkgs>" (indexSystem opts) scopes
          logInfo $ "root package count: " <> T.show (length pkgs')
          runPipelineWithProgress (indexWorker opts) (Map.size <$> readMemoryDatabase database) pkgs
        readMemoryDatabase database
  withFile (fromAbsFile $ cacheDir </> $(mkRelFile "database.tsv")) WriteMode $
    \h -> T.hPutStr h (formatDatabase result)
  T.putStrLn $ "summary: " <> T.show (Map.size result) <> " paths indexed"
  hFlush stdout
