module Sakuin.Database
  ( DatabaseFormat (..),
    defaultDatabaseFormat,
    DatabaseWriter,
    withDatabase,
    runDatabaseWriter,
    readDatabaseEntryCount,
    runDatabaseSearch,
    runFrCodeNativeSearch,
    runFrCodeParallelSearch,
    runFrCodeParallelRipgrepSearch,
    runFrCodeRipgrepSearch,
    runTsvDatabase,
    runTsvSearch,
    withTsvDatabase,
    readTsvEntryCount,
  )
where

import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Error.Static (Error)
import Sakuin.Database.Common (DatabaseWriter, readDatabaseEntryCount, runDatabaseWriter)
import Sakuin.Database.FrCode (runFrCodeParallelRipgrepSearch, runFrCodeParallelSearch, runFrCodeRipgrepSearch, runFrCodeSearch, withFrCodeDatabase)
import Sakuin.Database.Native (runFrCodeNativeSearch)
import Sakuin.Database.TSV (readTsvEntryCount, runTsvDatabase, runTsvSearch, withTsvDatabase)
import Sakuin.Search (SearchError)
import Sakuin.Types (Search)
import System.IO (Handle)

data DatabaseFormat = Tsv | FrCode
  deriving stock (Show)

defaultDatabaseFormat :: DatabaseFormat
defaultDatabaseFormat = FrCode

withDatabase ::
  forall es a. (Concurrent :> es, IOE :> es) => DatabaseFormat -> Int -> Handle -> (DatabaseWriter -> Eff es a) -> Eff es a
withDatabase Tsv = withTsvDatabase
withDatabase FrCode = withFrCodeDatabase

runDatabaseSearch ::
  forall es a. (IOE :> es, Error SearchError :> es) => DatabaseFormat -> FilePath -> Bool -> Eff (Search : es) a -> Eff es a
runDatabaseSearch Tsv = runTsvSearch
runDatabaseSearch FrCode = runFrCodeSearch
