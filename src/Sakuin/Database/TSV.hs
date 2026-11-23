module Sakuin.Database.TSV where

import Codec.Compression.Zstd.Streaming qualified as Zstd
import Control.Monad (void, when)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, byteString, char8, toLazyByteString, word64Dec)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Effectful
import Effectful.Concurrent.Async (wait, withAsync)
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static
import Effectful.Exception
import Sakuin.Search
import Sakuin.Types
import System.IO (Handle, hIsEOF)
import System.Process.Typed

data TsvDatabase = TsvDatabase
  { tsvWriteQueue :: TBQueue (Maybe IndexedStorePath),
    tsvWriterDone :: TMVar (Either SomeException ()),
    tsvEntryCount :: TVar Int
  }

type DatabaseWriter = TsvDatabase

withTsvDatabase ::
  forall es a. (Concurrent :> es, IOE :> es) => Int -> Handle -> (TsvDatabase -> Eff es a) -> Eff es a
withTsvDatabase = withDatabaseWriter LBS.empty formatIndexedStorePath

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

runTsvDatabase ::
  forall es a. (Concurrent :> es) => TsvDatabase -> Eff (Database : es) a -> Eff es a
runTsvDatabase = runDatabaseWriter

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

readTsvEntryCount :: forall es. (Concurrent :> es) => TsvDatabase -> Eff es Int
readTsvEntryCount = readDatabaseEntryCount

readDatabaseEntryCount :: forall es. (Concurrent :> es) => DatabaseWriter -> Eff es Int
readDatabaseEntryCount = readTVarIO . tsvEntryCount

formatIndexedStorePath :: IndexedStorePath -> LBS.ByteString
formatIndexedStorePath = toLazyByteString . formatIndexedStorePathBuilder

formatIndexedStorePathBuilder :: IndexedStorePath -> Builder
formatIndexedStorePathBuilder indexed = foldIndexedFiles line mempty (indexedFiles indexed)
  where
    (package, storePrefix) = indexedPathFields $ indexedPath indexed
    line path metadata rest =
      byteString package <> char8 '\t' <> metadata <> char8 '\t' <> byteString storePrefix <> path <> char8 '\n' <> rest

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

runTsvSearch ::
  forall es a. (IOE :> es, Error SearchError :> es) => FilePath -> Bool -> Eff (Search : es) a -> Eff es a
runTsvSearch databasePath isMinimal = interpret $ \_ -> \case
  SearchPaths pattern isRegex filters -> searchTsvDatabase databasePath pattern isRegex filters isMinimal

searchTsvDatabase ::
  forall es. (IOE :> es, Error SearchError :> es) => FilePath -> Text -> Bool -> SearchFilter -> Bool -> Eff es ()
searchTsvDatabase databasePath pattern isRegex filters isMinimal =
  either (throwError . InvalidSearchRegex . T.pack) runSearch (pathMatcher pattern isRegex filters)
  where
    runSearch matchesPath = do
      result <- try @SomeException $
        withProcessWait zstdConfig $ \zstdProcess ->
          withProcessWait (setStdout createPipe . rgConfig $ getStdout zstdProcess) $ \rgProcess -> do
            if isMinimal
              then void $ drainSearchResults (getStdout rgProcess) Set.empty $ \line seen ->
                let outputName = LBS8.takeWhile (/= '\t') line
                 in if
                      | Set.member outputName seen -> pure seen
                      | matchesTsvSearchFilter filters matchesPath line ->
                          liftIO (LBS8.putStrLn outputName) *> pure (Set.insert outputName seen)
                      | otherwise -> pure seen
              else void $ drainSearchResults (getStdout rgProcess) () $ \line () ->
                when (matchesTsvSearchFilter filters matchesPath line) $ liftIO (LBS8.putStrLn line)
            rgExit <- waitExitCode rgProcess
            case rgExit of
              ExitSuccess -> pure ()
              ExitFailure 1 -> pure ()
              ExitFailure code -> throwIO . userError $ "rg failed with exit code " <> show code
            checkExitCode zstdProcess
      either (throwError . SearchProcessError . T.pack . displayException) pure result
    rgConfig input = setStdin (useHandleOpen input) $ proc "rg" (rgArguments pattern isRegex)
    zstdConfig = decompressDatabase databasePath

decompressDatabase :: FilePath -> ProcessConfig () Handle ()
decompressDatabase databasePath = setStdout createPipe $ proc "zstd" ["--decompress", "--stdout", databasePath]

drainSearchResults :: forall es a. (IOE :> es) => Handle -> a -> (LBS.ByteString -> a -> Eff es a) -> Eff es a
drainSearchResults input acc sink = do
  atEnd <- liftIO $ hIsEOF input
  if atEnd
    then pure acc
    else do
      line <- liftIO $ LBS.fromStrict <$> BS8.hGetLine input
      acc' <- sink line acc
      drainSearchResults input acc' sink

rgArguments :: Text -> Bool -> [String]
rgArguments pattern isRegex =
  ["--text", "--no-line-number", "--no-heading", "--color", "never"]
    <> (if isRegex then [] else ["--fixed-strings", "--ignore-case"])
    <> ["--", T.unpack pattern]

matchesTsvSearchFilter :: SearchFilter -> PathMatcher -> LBS8.ByteString -> Bool
matchesTsvSearchFilter filters matchesPath line =
  case LBS8.split '\t' line of
    [package, metadata, fullPath] ->
      matchesSearchFields filters matchesPath (LBS.toStrict package) (LBS.toStrict metadata) (LBS.toStrict fullPath)
    _ -> False

matchesSearchFields :: SearchFilter -> PathMatcher -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Bool
matchesSearchFields filters matchesPath package metadata fullPath =
  maybe True (`T.isPrefixOf` decodeUtf8 package) (filterPackage filters)
    && maybe True (hasStoreHash path) (filterHash filters)
    && (null (filterTypes filters) || maybe False (`elem` filterTypes filters) fileType)
    && matchesPath path
  where
    path = decodeUtf8 fullPath
    fileType = snd <$> BS8.unsnoc metadata
