module Index where

import Cli
import Data.Map qualified as Map
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
  let Packages p = pkgs
  putTextLn $ "package count: " <> show (length p)
  hFlush stdout
  mgr <- newTlsManager
  db <- newMemoryDatabase
  runReaderT (runCacheIO (runDatabaseT db (runDatabaseT db $ runPipeline 100 pkgs))) mgr
  indexed <- readMemoryDatabase db
  withFile "out.txt" WriteMode $ \h -> do
    forM_ (Map.toList indexed) $ \(k, v) -> do
      T.hPutStrLn h $ k <> "\t" <> show (indexedFiles v)
  putTextLn "done"
  hFlush stdout
