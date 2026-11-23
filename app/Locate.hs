module Locate where

import Cli
import Effectful
import Effectful.Error.Static (runErrorNoCallStackWith)
import Path (toFilePath)
import Sakuin
import Sakuin.Storage
import System.IO (hFlush, stdout)

runLocate :: LocateOptions -> IO ()
runLocate opts = do
  runEff . runErrorNoCallStackWith @SearchError (liftIO . exitErrorIO) $ do
    databaseDir <- resolveDatabaseDir (locateDatabase opts)
    let format = locateDatabaseFormat opts
    runDatabaseSearch format (toFilePath $ databasePathFor format databaseDir) (locateMinimal opts) $
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
