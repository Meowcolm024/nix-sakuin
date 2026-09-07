module Sakuin.Database where

import Codec.Compression.Zstd.Streaming qualified as Zstd
import Control.Exception (SomeException)
import Control.Exception qualified as Exception
import Data.ByteString qualified as BS
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Effectful
import Effectful.Concurrent.Async (wait, withAsync)
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Exception (bracket, throwIO, try)
import Sakuin.Types
import System.IO

data TsvDatabase = TsvDatabase
  { tsvWriteQueue :: TBQueue (Maybe IndexedStorePath),
    tsvWriterDone :: TMVar (Either SomeException ()),
    tsvEntryCount :: TVar Int
  }

withTsvDatabase ::
  forall es a.
  (Concurrent :> es, IOE :> es) =>
  Int ->
  FilePath ->
  (TsvDatabase -> Eff es a) ->
  Eff es a
withTsvDatabase queueCapacity databasePath action =
  bracket
    (liftIO $ openBinaryFile databasePath WriteMode)
    (liftIO . hClose)
    $ \output -> do
      queue <- newTBQueueIO (fromIntegral $ max 1 queueCapacity)
      done <- newEmptyTMVarIO
      count <- newTVarIO 0
      let database = TsvDatabase queue done count
          writerAction = do
            outcome <- try @SomeException $ writerLoop output queue count
            atomically $ putTMVar done outcome
            either throwIO pure outcome
      withAsync writerAction $ \writer -> do
        result <- action database
        enqueue database Nothing
        wait writer
        pure result

runTsvDatabase ::
  forall es a.
  (Concurrent :> es) =>
  TsvDatabase ->
  Eff (Database : es) a ->
  Eff es a
runTsvDatabase database = interpret $ \_ -> \case
  AddToDatabase indexed -> insertTsvDatabase database indexed

insertTsvDatabase :: forall es. (Concurrent :> es) => TsvDatabase -> IndexedStorePath -> Eff es ()
insertTsvDatabase database = enqueue database . Just

enqueue :: forall es. (Concurrent :> es) => TsvDatabase -> Maybe IndexedStorePath -> Eff es ()
enqueue database item =
  atomically $ do
    ( do
        writerStatus <- readTMVar (tsvWriterDone database)
        case writerStatus of
          Left err -> throwSTM err
          Right () -> throwSTM $ userError "TSV database writer stopped unexpectedly"
      )
      `orElse` writeTBQueue (tsvWriteQueue database) item

writerLoop ::
  forall es. (Concurrent :> es, IOE :> es) => Handle -> TBQueue (Maybe IndexedStorePath) -> TVar Int -> Eff es ()
writerLoop output queue count = liftIO (Zstd.compress 3) >>= drive False
  where
    drive ending = \case
      Zstd.Produce bytes next -> do
        liftIO $ BS.hPut output bytes
        liftIO next >>= drive ending
      Zstd.Consume consume
        | ending -> liftIO . Exception.throwIO . userError $ "zstd requested input after end of stream"
        | otherwise ->
            atomically (readTBQueue queue) >>= \case
              Nothing -> liftIO (consume BS.empty) >>= drive True
              Just indexed -> do
                let bytes = LBS.toStrict $ formatIndexedStorePath indexed
                atomically $ modifyTVar' count (+ 1)
                -- skip empty store listing line
                if BS.null bytes
                  then drive False (Zstd.Consume consume)
                  else liftIO (consume bytes) >>= drive False
      Zstd.Error code message ->
        liftIO . Exception.throwIO . userError $ "zstd compression failed (" <> code <> "): " <> message
      Zstd.Done bytes -> liftIO $ BS.hPut output bytes

readTsvEntryCount :: forall es. (Concurrent :> es) => TsvDatabase -> Eff es Int
readTsvEntryCount = readTVarIO . tsvEntryCount

formatIndexedStorePath :: IndexedStorePath -> ByteString
formatIndexedStorePath indexed =
  foldMap (formatFileLine $ indexedPath indexed) (toFileList $ indexedFiles indexed)

formatFileLine :: WithOrigin StorePath -> FileLine -> ByteString
formatFileLine indexed (FileLine (path, node)) =
  LBS8.intercalate "\t" [text package, text metadata, text fullPath] <> "\n"
  where
    entryOrigin = origin indexed
    storePath = value indexed
    package = orAttr entryOrigin <> "." <> orOutput entryOrigin
    metadata = case node of
      Regular fileSize True -> T.pack (show fileSize) <> " x"
      Regular fileSize False -> T.pack (show fileSize) <> " r"
      Symlink _ -> "0 s"
      Directory () -> "0 d"
    fullPath =
      spDir storePath <> "/" <> spHash storePath <> "-" <> spName storePath <> path
    text = LBS.fromStrict . encodeUtf8
