module Locate where

import Cli
import Effectful
import Effectful.Error.Static (runErrorNoCallStackWith)
import Path (toFilePath)
import Sakuin
import Sakuin.Storage

runLocate :: LocateOptions -> IO ()
runLocate opts = do
  runEff . runErrorNoCallStackWith @SearchError (liftIO . exitErrorIO) $ do
    databaseDir <- resolveDatabaseDir (locateDatabase opts)
    runTsvSearch (toFilePath $ databasePath databaseDir) (locateMinimal opts) $
      searchPaths
        (locatePattern opts)
        (locateRegex opts)
        TsvSearchFilter
          { filterPackage = locatePackage opts,
            filterHash = locateHash opts,
            filterTypes = locateTypes opts,
            filterWholeName = locateWholeName opts,
            filterAtRoot = locateAtRoot opts
          }
