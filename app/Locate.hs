module Locate where

import Cli
import Path (toFilePath)
import Sakuin.Database
import Storage

runLocate :: LocateOptions -> IO ()
runLocate opts = do
  databaseDir <- resolveDatabaseDir (locateDatabase opts)
  searchTsvDatabase
    (toFilePath $ databasePath databaseDir)
    (locatePattern opts)
    (locateRegex opts)
