module Locate where

import Cli
import Effectful
import Effectful.Error.Static (runErrorNoCallStackWith)
import Path (toFilePath)
import Sakuin.Search
import Sakuin.Types
import Storage

runLocate :: LocateOptions -> IO ()
runLocate opts = do
  databaseDir <- resolveDatabaseDir (locateDatabase opts)
  runEff
    . runErrorNoCallStackWith
      (\(err :: SearchError) -> liftIO (exitErrorIO err))
    $ runTsvSearch (toFilePath $ databasePath databaseDir) (locateMinimal opts)
    $ searchPaths
      (locatePattern opts)
      (locateRegex opts)
      TsvSearchFilter
        { filterPackage = locatePackage opts,
          filterHash = locateHash opts,
          filterTypes = locateTypes opts,
          filterWholeName = locateWholeName opts,
          filterAtRoot = locateAtRoot opts
        }
