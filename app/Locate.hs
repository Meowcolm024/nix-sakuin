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
    runSearch <- case (format, locateParallel opts, locateRipgrep opts, locateCheat opts) of
      (FrCode, False, False, True) -> pure runFrCodeNativeSearch
      (Tsv, _, _, True) -> throwError $ SearchProcessError "--cheat requires --db-format frcode"
      (_, _, _, True) -> throwError $ SearchProcessError "--cheat cannot be combined with --parallel or --ripgrep"
      (FrCode, True, True, False) -> pure runFrCodeParallelRipgrepSearch
      (FrCode, True, False, False) -> pure runFrCodeParallelSearch
      (FrCode, False, True, False) -> pure runFrCodeRipgrepSearch
      (Tsv, True, _, False) -> throwError $ SearchProcessError "--parallel requires --db-format frcode"
      (Tsv, False, True, False) -> throwError $ SearchProcessError "--ripgrep requires --db-format frcode"
      (_, False, False, False) -> pure $ runDatabaseSearch format
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
