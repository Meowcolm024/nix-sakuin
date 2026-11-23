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
    runSearch <- case (format, locateParallel opts) of
      (FrCode, True) -> pure runFrCodeParallelSearch
      (Tsv, True) -> throwError $ SearchProcessError "--parallel requires --db-format frcode"
      (_, False) -> pure $ runDatabaseSearch format
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
