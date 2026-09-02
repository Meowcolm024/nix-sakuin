module Sakuin.Database.MemoryDatabase where

import Control.Monad.Trans.Class qualified as Trans
import Data.Map.Strict qualified as Map
import Sakuin.Types

newtype MemoryDatabase = MemoryDatabase
  { databaseEntries :: TVar (Map StoreHash IndexedStorePath)
  }

newMemoryDatabase :: IO MemoryDatabase
newMemoryDatabase = MemoryDatabase <$> newTVarIO Map.empty

readMemoryDatabase :: MemoryDatabase -> IO (Map StoreHash IndexedStorePath)
readMemoryDatabase = readTVarIO . databaseEntries

insertMemoryDatabase :: MemoryDatabase -> IndexedStorePath -> IO ()
insertMemoryDatabase database entry =
  atomically $
    modifyTVar'
      (databaseEntries database)
      (Map.insert (entryHash entry) entry)
  where
    entryHash = spHash . value . indexedPath

instance (MonadIO m) => MonadDatabase (DatabaseT MemoryDatabase m) where
  addToDatabase entry = DatabaseT $ do
    database <- ask
    liftIO $ insertMemoryDatabase database entry

instance (MonadCache m) => MonadCache (DatabaseT MemoryDatabase m) where
  fetchNarinfo = DatabaseT . Trans.lift . fetchNarinfo
  fetchListing = DatabaseT . Trans.lift . fetchListing
