module Sakuin.Database
  ( DatabaseFormat (..),
    DatabaseWriter,
    withDatabase,
    runDatabaseWriter,
    readDatabaseEntryCount,
    runDatabaseSearch,
    runTsvDatabase,
    runTsvSearch,
    withTsvDatabase,
    readTsvEntryCount,
  )
where

import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Error.Static (Error)
import Sakuin.Database.FrCode (runFrCodeSearch, withFrCodeDatabase)
import Sakuin.Database.TSV
import Sakuin.Search (SearchError)
import Sakuin.Types (Search)
import System.IO (Handle)

data DatabaseFormat = Tsv | FrCode
  deriving stock (Show)

withDatabase ::
  forall es a. (Concurrent :> es, IOE :> es) => DatabaseFormat -> Int -> Handle -> (DatabaseWriter -> Eff es a) -> Eff es a
withDatabase Tsv = withTsvDatabase
withDatabase FrCode = withFrCodeDatabase

runDatabaseSearch ::
  forall es a. (IOE :> es, Error SearchError :> es) => DatabaseFormat -> FilePath -> Bool -> Eff (Search : es) a -> Eff es a
runDatabaseSearch Tsv = runTsvSearch
runDatabaseSearch FrCode = runFrCodeSearch
