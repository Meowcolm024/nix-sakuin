module Locate where

import Cli
import Effectful
import Effectful.Error.Static (runErrorNoCallStackWith)
import Path (toFilePath)
import Sakuin
import Sakuin.Storage
import System.IO (hFlush, hPutStrLn, stderr, stdout)

runLocate :: LocateOptions -> IO ()
runLocate opts = do
  runEff . runErrorNoCallStackWith @SearchError (liftIO . exitErrorIO) $ do
    let format = locateDatabaseFormat opts
    runSearch <- case (format, locateBackend opts) of
      (_, DefaultBackend) -> pure $ runDatabaseSearch format
      (FrCode, ParallelBackend) -> pure runFrCodeParallelSearch
      (FrCode, PureBackend) -> pure runFrCodePureSearch
      (FrCode, CheatBackend) -> pure runFrCodeNativeSearch
      (Tsv, backend) -> do
        liftIO $ hPutStrLn stderr $ "Warning: ignoring --backend " <> locateBackendName backend <> " for TSV databases"
        pure $ runDatabaseSearch Tsv
    databaseDir <- resolveDatabaseDir (locateDatabase opts)
    runSearch (toFilePath $ databasePathFor format databaseDir) (locateMinimal opts) $
      searchPaths
        (locatePattern opts)
        (locateRegex opts)
        SearchFilter
          { filterPackage = locatePackage opts,
            filterHash = locateHash opts,
            filterTypes = locateTypes opts,
            filterWholeName = locateWholeName opts,
            filterAtRoot = locateAtRoot opts
          }
  hFlush stdout
