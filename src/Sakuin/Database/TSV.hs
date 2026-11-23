module Sakuin.Database.TSV
  ( TsvDatabase (..),
    DatabaseWriter,
    withTsvDatabase,
    withDatabaseWriter,
    runTsvDatabase,
    runDatabaseWriter,
    enqueue,
    writerLoop,
    readTsvEntryCount,
    readDatabaseEntryCount,
    formatIndexedStorePath,
    formatIndexedStorePathBuilder,
    indexedPathFields,
    foldIndexedFiles,
    runTsvSearch,
    searchTsvDatabase,
    decompressDatabase,
    drainSearchResults,
    rgArguments,
    matchesTsvSearchFilter,
    matchesSearchFields,
  )
where

import Control.Monad (void, when)
import Data.ByteString.Builder (Builder, byteString, char8, toLazyByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static
import Effectful.Exception
import Sakuin.Database.Common
import Sakuin.Search
import Sakuin.Types
import System.IO (Handle, hIsEOF)
import System.Process.Typed

withTsvDatabase ::
  forall es a. (Concurrent :> es, IOE :> es) => Int -> Handle -> (TsvDatabase -> Eff es a) -> Eff es a
withTsvDatabase = withDatabaseWriter LBS.empty formatIndexedStorePath

runTsvDatabase ::
  forall es a. (Concurrent :> es) => TsvDatabase -> Eff (Database : es) a -> Eff es a
runTsvDatabase = runDatabaseWriter

readTsvEntryCount :: forall es. (Concurrent :> es) => TsvDatabase -> Eff es Int
readTsvEntryCount = readDatabaseEntryCount

formatIndexedStorePath :: IndexedStorePath -> LBS.ByteString
formatIndexedStorePath = toLazyByteString . formatIndexedStorePathBuilder

formatIndexedStorePathBuilder :: IndexedStorePath -> Builder
formatIndexedStorePathBuilder indexed = foldIndexedFiles line mempty (indexedFiles indexed)
  where
    (package, storePrefix) = indexedPathFields $ indexedPath indexed
    line path metadata rest =
      byteString package <> char8 '\t' <> metadata <> char8 '\t' <> byteString storePrefix <> path <> char8 '\n' <> rest

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
            liftIO $ checkRgExitCode rgProcess
            checkExitCode zstdProcess
      either (throwError . SearchProcessError . T.pack . displayException) pure result
    rgConfig input = setStdin (useHandleOpen input) $ proc "rg" (rgArguments pattern isRegex)
    zstdConfig = decompressDatabase databasePath

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
  rgBaseArguments
    <> (if isRegex then [] else ["--fixed-strings", "--ignore-case"])
    <> ["--", T.unpack pattern]

matchesTsvSearchFilter :: SearchFilter -> PathMatcher -> LBS8.ByteString -> Bool
matchesTsvSearchFilter filters matchesPath line =
  case LBS8.split '\t' line of
    [package, metadata, fullPath] ->
      matchesSearchFields filters matchesPath (LBS.toStrict package) (LBS.toStrict metadata) (LBS.toStrict fullPath)
    _ -> False
