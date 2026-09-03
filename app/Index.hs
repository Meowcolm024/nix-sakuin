module Index where

import Cli
import Data.List (nub)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Effectful
import Effectful.Concurrent.Async
import Effectful.Fail
import Effectful.Reader.Static (runReader)
import Network.HTTP.Client.TLS
import Sakuin
import Sakuin.Database
import Sakuin.Hydra
import System.IO

runIndex :: IndexOptions -> IO ()
runIndex opts = do
  print opts
  mgr <- newTlsManager
  result <- runEff
    . runFailIO
    . runConcurrent
    . runReader mgr
    $ do
      database <- newMemoryDatabase
      runMemoryDatabase database . runHydra $ do
        -- NOTE: Nothing represents the default scope
        let scopes = nub $ (if indexNoDefaultScope opts then [] else [Nothing]) <> map Just (indexExtraScopes opts)
        pkgs@(Packages pkgs') <- queryAllScopes "<nixpkgs>" (indexSystem opts) scopes
        liftIO $ do
          T.putStrLn $ "package count: " <> T.show (length pkgs')
          hFlush stdout
        if indexVerbose opts
          then runPipelineWithProgress (indexWorker opts) (Map.size <$> readMemoryDatabase database) pkgs
          else runPipeline (indexWorker opts) pkgs
      readMemoryDatabase database
  withFile "out.txt" WriteMode $ \h -> T.hPutStr h (formatDatabase result)
  T.putStrLn $ "done: " <> T.show (Map.size result) <> " paths indexed"
  hFlush stdout
