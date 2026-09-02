module Index where

import Cli
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
import Sakuin.NixEnv (queryPackages)
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
        pkgs <- queryPackages "<nixpkgs>" (indexSystem opts) (Just "coqPackages")
        let Packages pkgs' = pkgs
        liftIO $ do
          T.putStrLn $ "package count: " <> T.show (length pkgs')
          hFlush stdout
        runPipeline 100 pkgs
      formatMemoryDatabase database
  withFile "out.txt" WriteMode $ \h -> T.hPutStr h result
  T.putStrLn "done"
  hFlush stdout
