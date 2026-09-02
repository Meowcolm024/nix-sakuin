module Index where

import Cli
import Data.Text.IO qualified as T
import Network.HTTP.Client.TLS (newTlsManager)
import Sakuin
import Sakuin.Database
import Sakuin.Hydra (CacheIO (..))
import Sakuin.NixEnv (queryPackages)

type Runtime a = DatabaseT MemoryDatabase CacheIO a

runIndex :: IndexOptions -> IO ()
runIndex opts = do
  print opts
  pkgs <- queryPackages "<nixpkgs>" (indexSystem opts) (Just "coqPackages")
  let Packages pkgs' = pkgs
  putTextLn $ "package count: " <> show (length pkgs')
  hFlush stdout
  mgr <- newTlsManager
  db <- newMemoryDatabase
  runReaderT (runCacheIO (runDatabaseT db (runDatabaseT db $ runPipeline 100 pkgs))) mgr
  withFile "out.txt" WriteMode $ \h -> do
    T.hPutStr h =<< formatMemoryDatabase db
  putTextLn "done"
  hFlush stdout
