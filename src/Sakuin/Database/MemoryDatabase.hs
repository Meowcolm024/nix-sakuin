module Sakuin.Database.MemoryDatabase where

import Codec.Compression.Zstd.Lazy qualified as Zstd
import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Map (Map)
import Data.Map.Strict qualified as Map
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

encodeDatabase :: Map StoreHash IndexedStorePath -> ByteString
encodeDatabase =
  Zstd.compress 3
    . foldMap (\entry -> encode entry <> "\n")
    . Map.toAscList

decodeDatabase :: ByteString -> Either String (Map StoreHash IndexedStorePath)
decodeDatabase bytes =
  Map.fromList <$> traverse eitherDecode (LBS8.lines $ Zstd.decompress bytes)

encodeMemoryDatabase :: forall es. (Concurrent :> es) => MemoryDatabase -> Eff es ByteString
encodeMemoryDatabase database = encodeDatabase <$> readMemoryDatabase database
