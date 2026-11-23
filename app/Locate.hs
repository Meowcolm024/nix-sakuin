module Locate where

import Cli
import Effectful
import Effectful.Error.Static (runErrorNoCallStackWith, throwError)
import Path (toFilePath)
import Sakuin
import Sakuin.Storage
import System.IO (hFlush, stdout)

runLocate :: LocateOptions -> IO ()
runLocate opts = do
  runEff . runErrorNoCallStackWith @SearchError (liftIO . exitErrorIO) $ do
    let format = locateDatabaseFormat opts
    runSearch <- case (format, locateParallel opts, locatePure opts, locateCheat opts) of
      (FrCode, False, False, True) -> pure runFrCodeNativeSearch
      (FrCode, False, True, False) -> pure runFrCodePureSearch
      (FrCode, True, False, False) -> pure runFrCodeParallelSearch
      (Tsv, _, _, True) -> throwError $ SearchProcessError "--cheat requires --db-format frcode"
      (Tsv, True, _, False) -> throwError $ SearchProcessError "--parallel requires --db-format frcode"
      (Tsv, False, True, False) -> throwError $ SearchProcessError "--pure requires --db-format frcode"
      (_, False, False, False) -> pure $ runDatabaseSearch format
      (FrCode, _, _, _) -> throwError $ SearchProcessError "--pure, --parallel, and --cheat are mutually exclusive"
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
