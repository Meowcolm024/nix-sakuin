module Sakuin.MemoryDatabase where

import Codec.Compression.Zstd.Lazy qualified as Zstd
import Control.Monad (forM_)
import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async (mapConcurrently_)
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic (interpret)
import Sakuin.Types

newtype MemoryDatabase = MemoryDatabase
  { databaseEntries :: TVar (Map StoreHash IndexedStorePath)
  }

runMemoryDatabase :: forall es a. (Concurrent :> es) => MemoryDatabase -> Eff (Database : es) a -> Eff es a
runMemoryDatabase database = interpret $ \_ -> \case
  AddToDatabase indexed -> insertMemoryDatabase database indexed

newMemoryDatabase :: forall es. (Concurrent :> es) => Eff es MemoryDatabase
newMemoryDatabase = newMemoryDatabaseFrom Map.empty

newMemoryDatabaseFrom :: forall es. (Concurrent :> es) => Map StoreHash IndexedStorePath -> Eff es MemoryDatabase
newMemoryDatabaseFrom entries = MemoryDatabase <$> newTVarIO entries

readMemoryDatabase :: forall es. (Concurrent :> es) => MemoryDatabase -> Eff es (Map StoreHash IndexedStorePath)
readMemoryDatabase = readTVarIO . databaseEntries

insertMemoryDatabase :: forall es. (Concurrent :> es) => MemoryDatabase -> IndexedStorePath -> Eff es ()
insertMemoryDatabase database entry =
  atomically $ modifyTVar' (databaseEntries database) (Map.insert (spHash . value . indexedPath $ entry) entry)

encodeDatabase :: Map StoreHash IndexedStorePath -> ByteString
encodeDatabase = Zstd.compress 3 . foldMap (\entry -> encode entry <> "\n") . Map.toAscList

decodeDatabase :: ByteString -> Either String (Map StoreHash IndexedStorePath)
decodeDatabase bytes = Map.fromList <$> traverse eitherDecode (LBS8.lines $ Zstd.decompress bytes)

encodeMemoryDatabase :: forall es. (Concurrent :> es) => MemoryDatabase -> Eff es ByteString
encodeMemoryDatabase database = encodeDatabase <$> readMemoryDatabase database

searchMemoryDatabase ::
  forall es.
  (Concurrent :> es) =>
  Int ->
  MemoryDatabase ->
  PathMatcher ->
  (SearchResult -> Eff es ()) ->
  Eff es ()
searchMemoryDatabase workerCount database matches emit = do
  entries <- readMemoryDatabase database
  mapConcurrently_ searchChunk (splitEvenly workerCount $ Map.elems entries)
  where
    searchChunk indexedPaths =
      forM_ indexedPaths $ \indexed ->
        forM_ (toFileList $ indexedFiles indexed) $ \fileLine ->
          if matches (fileLinePath fileLine)
            then emit (indexedPath indexed, fileLine)
            else pure ()

splitEvenly :: Int -> [a] -> [[a]]
splitEvenly _ [] = []
splitEvenly requestedWorkers entries = chunksOf chunkSize entries
  where
    workerCount = max 1 requestedWorkers
    chunkSize = (length entries + workerCount - 1) `div` workerCount
    chunksOf _ [] = []
    chunksOf size xs =
      let (chunk, rest) = splitAt size xs
       in chunk : chunksOf size rest
