module Sakuin.Database.MemoryDatabase where

import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import Sakuin.Types

newtype MemoryDatabase = MemoryDatabase
  { databaseEntries :: TVar (Map StoreHash IndexedStorePath)
  }

runMemoryDatabase :: forall es a. (Concurrent :> es) => MemoryDatabase -> Eff (Database : es) a -> Eff es a
runMemoryDatabase database = interpret $ \_ -> \case
  AddToDatabase indexed -> insertMemoryDatabase database indexed

newMemoryDatabase :: forall es. (Concurrent :> es) => Eff es MemoryDatabase
newMemoryDatabase = MemoryDatabase <$> newTVarIO Map.empty

readMemoryDatabase ::
  forall es. (Concurrent :> es) => MemoryDatabase -> Eff es (Map StoreHash IndexedStorePath)
readMemoryDatabase = readTVarIO . databaseEntries

insertMemoryDatabase :: forall es. (Concurrent :> es) => MemoryDatabase -> IndexedStorePath -> Eff es ()
insertMemoryDatabase database entry =
  atomically $
    modifyTVar'
      (databaseEntries database)
      (Map.insert (entryHash entry) entry)
  where
    entryHash = spHash . value . indexedPath

formatDatabase :: Map StoreHash IndexedStorePath -> Text
formatDatabase = T.unlines . foldMap formatEntry . Map.toAscList
  where
    formatEntry (storeHash, indexed) =
      map (formatNode storeHash) . toFileList . indexedFiles $ indexed
    formatNode storeHash (path, node) =
      storeHash <> "\t" <> path <> "\t" <> T.show node

formatMemoryDatabase :: forall es. (Concurrent :> es) => MemoryDatabase -> Eff es Text
formatMemoryDatabase database = formatDatabase <$> readMemoryDatabase database
