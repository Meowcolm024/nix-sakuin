module Sakuin.Database.Common
  ( DatabaseWriter,
    TsvDatabase (..),
    withDatabaseWriter,
    runDatabaseWriter,
    readDatabaseEntryCount,
    enqueue,
    writerLoop,
    indexedPathFields,
    foldIndexedFiles,
    decompressDatabase,
    matchesSearchFields,
  )
where

import Codec.Compression.Zstd.Streaming qualified as Zstd
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, byteString, char8, word64Dec)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Effectful
import Effectful.Concurrent.Async (wait, withAsync)
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Exception
import Sakuin.Search (hasStoreHash)
import Sakuin.Types
import System.IO (Handle)
import System.Process.Typed (ProcessConfig, createPipe, proc, setStdout)

-- Keep the original TSV writer type available to existing callers.
data TsvDatabase = TsvDatabase
  { tsvWriteQueue :: TBQueue (Maybe IndexedStorePath),
    tsvWriterDone :: TMVar (Either SomeException ()),
    tsvEntryCount :: TVar Int
  }

type DatabaseWriter = TsvDatabase

withDatabaseWriter ::
  forall es a.
  (Concurrent :> es, IOE :> es) =>
  LBS.ByteString ->
  (IndexedStorePath -> LBS.ByteString) ->
  Int ->
  Handle ->
  (DatabaseWriter -> Eff es a) ->
  Eff es a
withDatabaseWriter header encodeStorePath queueCapacity output action = do
  queue <- newTBQueueIO (fromIntegral $ max 1 queueCapacity)
  done <- newEmptyTMVarIO
  count <- newTVarIO 0
  let database = TsvDatabase queue done count
      writerAction = do
        outcome <- try @SomeException $ writerLoop header encodeStorePath output queue count
        atomically $ putTMVar done outcome
        either throwIO pure outcome
  withAsync writerAction $ \writer -> do
    result <- action database
    enqueue database Nothing
    wait writer
    pure result

runDatabaseWriter :: forall es a. (Concurrent :> es) => DatabaseWriter -> Eff (Database : es) a -> Eff es a
runDatabaseWriter database = interpret $ \_ -> \case
  AddToDatabase indexed -> enqueue database (Just indexed)

enqueue :: forall es. (Concurrent :> es) => DatabaseWriter -> Maybe IndexedStorePath -> Eff es ()
enqueue database item =
  atomically $ do
    ( do
        writerStatus <- readTMVar (tsvWriterDone database)
        case writerStatus of
          Left err -> throwSTM err
          Right () -> throwSTM $ userError "database writer stopped unexpectedly"
      )
      `orElse` writeTBQueue (tsvWriteQueue database) item

writerLoop ::
  forall es.
  (Concurrent :> es, IOE :> es) =>
  LBS.ByteString ->
  (IndexedStorePath -> LBS.ByteString) ->
  Handle ->
  TBQueue (Maybe IndexedStorePath) ->
  TVar Int ->
  Eff es ()
writerLoop header encodeStorePath output queue count = liftIO (Zstd.compress 3) >>= drive False (LBS.toChunks header)
  where
    drive ending pending = \case
      Zstd.Produce bytes next -> do
        liftIO $ BS.hPut output bytes
        liftIO next >>= drive ending pending
      Zstd.Consume consume
        | ending -> throwIO . userError $ "zstd requested input after end of stream"
        | bytes : rest <- pending -> liftIO (consume bytes) >>= drive False rest
        | otherwise ->
            atomically (readTBQueue queue) >>= \case
              Nothing -> liftIO (consume BS.empty) >>= drive True []
              Just indexed -> do
                let chunks = LBS.toChunks $ encodeStorePath indexed
                atomically $ modifyTVar' count (+ 1)
                case chunks of
                  [] -> drive False [] (Zstd.Consume consume)
                  bytes : rest -> liftIO (consume bytes) >>= drive False rest
      Zstd.Error code message -> throwIO . userError $ "zstd compression failed (" <> code <> "): " <> message
      Zstd.Done bytes -> liftIO $ BS.hPut output bytes

readDatabaseEntryCount :: forall es. (Concurrent :> es) => DatabaseWriter -> Eff es Int
readDatabaseEntryCount = readTVarIO . tsvEntryCount

indexedPathFields :: WithOrigin StorePath -> (BS.ByteString, BS.ByteString)
indexedPathFields indexed = (package, storePrefix)
  where
    entryOrigin = origin indexed
    storePath = value indexed
    package = encodeUtf8 $ orAttr entryOrigin <> "." <> orOutput entryOrigin
    storePrefix = encodeUtf8 $ spDir storePath <> "/" <> spHash storePath <> "-" <> spName storePath

foldIndexedFiles :: (Builder -> Builder -> a -> a) -> a -> FileNode -> a
foldIndexedFiles line = go True mempty
  where
    go isRoot path rest (FileNode node) = case node of
      Regular fileSize isExecutable -> line path (word64Dec fileSize <> if isExecutable then " x" else " r") rest
      Symlink _ -> line path "0 s" rest
      Directory entries ->
        let files = Map.foldrWithKey (\name child next -> go False (path <> char8 '/' <> byteString (encodeUtf8 name)) next child) rest entries
         in if isRoot then files else line path "0 d" files

decompressDatabase :: FilePath -> ProcessConfig () Handle ()
decompressDatabase databasePath = setStdout createPipe $ proc "zstd" ["--decompress", "--stdout", databasePath]

matchesSearchFields :: SearchFilter -> PathMatcher -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Bool
matchesSearchFields filters matchesPath package metadata fullPath =
  maybe True (`T.isPrefixOf` decodeUtf8 package) (filterPackage filters)
    && maybe True (hasStoreHash path) (filterHash filters)
    && (null (filterTypes filters) || maybe False (`elem` filterTypes filters) fileType)
    && matchesPath path
  where
    path = decodeUtf8 fullPath
    fileType = snd <$> BS8.unsnoc metadata
