module Sakuin.Database where

import Codec.Compression.Zstd.Streaming qualified as Zstd
import Control.Exception (SomeException)
import Control.Exception qualified as Exception
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, byteString, char8, toLazyByteString, word64Dec)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Map qualified as Map
import Data.Text.Encoding (encodeUtf8)
import Effectful
import Effectful.Concurrent.Async (wait, withAsync)
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Exception (bracket, throwIO, try)
import Sakuin.Types
import System.IO (Handle, IOMode (WriteMode), hClose, openBinaryFile)

data TsvDatabase = TsvDatabase
  { tsvWriteQueue :: TBQueue (Maybe IndexedStorePath),
    tsvWriterDone :: TMVar (Either SomeException ()),
    tsvEntryCount :: TVar Int
  }

withTsvDatabase ::
  forall es a.
  (Concurrent :> es, IOE :> es) =>
  Int -> FilePath -> (TsvDatabase -> Eff es a) -> Eff es a
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
  TsvDatabase -> Eff (Database : es) a -> Eff es a
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
writerLoop output queue count = liftIO (Zstd.compress 3) >>= drive False []
  where
    drive ending pending = \case
      Zstd.Produce bytes next -> do
        liftIO $ BS.hPut output bytes
        liftIO next >>= drive ending pending
      Zstd.Consume consume
        | ending -> liftIO . Exception.throwIO . userError $ "zstd requested input after end of stream"
        | bytes : rest <- pending -> liftIO (consume bytes) >>= drive False rest
        | otherwise ->
            atomically (readTBQueue queue) >>= \case
              Nothing -> liftIO (consume BS.empty) >>= drive True []
              Just indexed -> do
                let chunks = LBS.toChunks $ formatIndexedStorePath indexed
                atomically $ modifyTVar' count (+ 1)
                -- skip empty store listing line
                case chunks of
                  [] -> drive False [] (Zstd.Consume consume)
                  bytes : rest -> liftIO (consume bytes) >>= drive False rest
      Zstd.Error code message ->
        liftIO . Exception.throwIO . userError $ "zstd compression failed (" <> code <> "): " <> message
      Zstd.Done bytes -> liftIO $ BS.hPut output bytes

readTsvEntryCount :: forall es. (Concurrent :> es) => TsvDatabase -> Eff es Int
readTsvEntryCount = readTVarIO . tsvEntryCount

formatIndexedStorePath :: IndexedStorePath -> ByteString
formatIndexedStorePath = toLazyByteString . formatIndexedStorePathBuilder

formatIndexedStorePathBuilder :: IndexedStorePath -> Builder
formatIndexedStorePathBuilder indexed = go True mempty (indexedFiles indexed)
  where
    indexedPath' = indexedPath indexed
    entryOrigin = origin indexedPath'
    storePath = value indexedPath'
    package = byteString . encodeUtf8 $ orAttr entryOrigin <> "." <> orOutput entryOrigin
    storePrefix =
      byteString . encodeUtf8 $
        spDir storePath <> "/" <> spHash storePath <> "-" <> spName storePath

    go isRoot path (FileNode node) = case node of
      Regular fileSize isExecutable ->
        line path $ word64Dec fileSize <> if isExecutable then " x" else " r"
      Symlink _ -> line path "0 s"
      Directory entries ->
        (if isRoot then mempty else line path "0 d")
          <> Map.foldMapWithKey
            (\name child -> go False (path <> char8 '/' <> byteString (encodeUtf8 name)) child)
            entries

    line path metadata =
      package <> char8 '\t' <> metadata <> char8 '\t' <> storePrefix <> path <> char8 '\n'
