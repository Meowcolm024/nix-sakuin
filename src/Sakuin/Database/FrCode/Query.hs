module Sakuin.Database.FrCode.Query
  ( FrCodeMatcher,
    frCodeMatcher,
    frCodeMatcherWith,
    runFrCodeSearchWith,
    printFrCodeMatches,
    formatMatch,
    writeMatch,
  )
where

import Control.Exception qualified as E
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Unsafe qualified as BSU
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, decodeUtf8', encodeUtf8)
import Data.Word (Word8)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static
import Effectful.Exception
import Sakuin.Database.Common (decompressDatabase, matchesFileTypeFilter, matchesPackageFilter, matchesSearchFields)
import Sakuin.Search
import Sakuin.Types
import System.Process.Typed (checkExitCode, getStdout, withProcessWait)

runFrCodeSearchWith ::
  forall es a.
  (IOE :> es, Error SearchError :> es) =>
  (Text -> Bool -> SearchFilter -> Either String FrCodeMatcher) ->
  (Bool -> FrCodeMatcher -> (BS.ByteString -> IO ()) -> LBS.ByteString -> IO ()) ->
  FilePath ->
  Bool ->
  Eff (Search : es) a ->
  Eff es a
runFrCodeSearchWith matcher search databasePath isMinimal = interpret $ \_ -> \case
  SearchPaths pattern isRegex filters -> do
    matchesPath <- either (throwError . InvalidSearchRegex . T.pack) pure $ matcher pattern isRegex filters
    result <- tryJust synchronous $ withProcessWait (decompressDatabase databasePath) $ \process -> do
      liftIO $ do
        contents <- LBS.hGetContents (getStdout process)
        search isMinimal matchesPath BS.putStr contents
      checkExitCode process
    either (throwError . SearchProcessError . T.pack . displayException) pure result
  where
    synchronous err = case fromException @SomeAsyncException err of
      Just _ -> Nothing
      Nothing -> Just err

-- The first two arguments prepare a predicate for a single package. Its
-- file records then supply their metadata and their relative path.
type FrCodeMatcher = BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Bool

frCodeMatcher :: Text -> Bool -> SearchFilter -> Either String FrCodeMatcher
frCodeMatcher = frCodeMatcherWith True

frCodeMatcherWith :: Bool -> Text -> Bool -> SearchFilter -> Either String FrCodeMatcher
frCodeMatcherWith useAsciiFastPath pattern isRegex filters = prepare <$> relativePathMatcher pattern isRegex filters
  where
    foldStorePath = foldsStorePath isRegex filters
    forListing path
      | foldStorePath = T.toCaseFold path
      | otherwise = path
    matchesAscii = asciiLiteralMatcher pattern filters
    matchesRelative matchesPath relativePath
      | useAsciiFastPath && not isRegex && BS.all (< 128) relativePath = matchesAscii relativePath
      | otherwise = matchesPath (decodeUtf8 relativePath)
    prepare matchesPath package storePrefix
      | not (matchesPackageFilter filters package) = \_ _ -> False
      | otherwise = case decodeUtf8' storePrefix of
          Right prefix
            | Just (storePath, "/") <- listingParts (prefix <> "/"),
              not foldStorePath || maybe False ((== "/") . snd) (listingParts (T.toCaseFold prefix <> "/")) ->
                let matchesHash = maybe True (== spHash storePath) (filterHash filters)
                 in \metadata relativePath ->
                      if "/" `BS.isPrefixOf` relativePath
                        then matchesHash && matchesFileTypeFilter filters metadata && matchesRelative matchesPath relativePath
                        else not (BS.null relativePath) && fallback metadata relativePath
          _ -> fallback
      where
        -- Some store directories have an earlier hash-shaped component. Use
        -- the existing full-path heuristic when the header is ambiguous.
        fallback metadata relativePath =
          matchesSearchFields filters (\path -> listingPathMatches (forListing path) matchesPath) package metadata (storePrefix <> relativePath)

asciiLiteralMatcher :: Text -> SearchFilter -> BS.ByteString -> Bool
asciiLiteralMatcher pattern filters
  | filterAtRoot filters = \path ->
      needleSize <= BS.length path
        && (not (filterWholeName filters) || needleSize == BS.length path)
        && equalsAt path 0
  | filterWholeName filters = \path ->
      let start = BS.length path - needleSize
       in start >= 0 && equalsAt path start
  | Just (first, _) <- BS.uncons needle =
      let alternate = if first >= 97 && first <= 122 then first - 32 else first
       in \path -> contains first path || (alternate /= first && contains alternate path)
  | otherwise = const True
  where
    folded = T.toCaseFold pattern
    needle = encodeUtf8 $ if filterAtRoot filters then ensureLeadingSlash folded else folded
    needleSize = BS.length needle

    -- Callers bound the candidate start so both indexes stay within their slices.
    equalsAt path start = go 0
      where
        go i =
          i == needleSize
            || (asciiLower (BSU.unsafeIndex path (start + i)) == BSU.unsafeIndex needle i && go (i + 1))

    contains first path =
      case BS.elemIndex first (BS.take (BS.length path - needleSize + 1) path) of
        Nothing -> False
        Just start -> equalsAt path start || contains first (BS.drop (start + 1) path)

asciiLower :: Word8 -> Word8
asciiLower byte
  | byte >= 65 && byte <= 90 = byte + 32
  | otherwise = byte

printFrCodeMatches ::
  Bool ->
  FrCodeMatcher ->
  (BS.ByteString -> IO ()) ->
  BS.ByteString ->
  BS.ByteString ->
  Set.Set BS.ByteString ->
  BS.ByteString ->
  BS.ByteString ->
  IO (Set.Set BS.ByteString)
printFrCodeMatches isMinimal matchesPath output package storePrefix = printMatch
  where
    matchesRow = matchesPath package storePrefix
    printMatch seen metadata relativePath
      | isMinimal && Set.member package seen = pure seen
      | otherwise = do
          matched <- E.evaluate $ matchesRow metadata relativePath
          if matched
            then do
              -- Force the owned output before the buffered decoder advances.
              line <- E.evaluate $ formatMatch isMinimal package metadata storePrefix relativePath
              writeMatch isMinimal output seen line
            else pure seen

formatMatch :: Bool -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
formatMatch isMinimal package metadata storePrefix relativePath
  | isMinimal = package
  | otherwise = BS.concat [package, "\t", metadata, "\t", storePrefix, relativePath, "\n"]

writeMatch :: Bool -> (BS.ByteString -> IO ()) -> Set.Set BS.ByteString -> BS.ByteString -> IO (Set.Set BS.ByteString)
writeMatch isMinimal output seen line
  | isMinimal && Set.member line seen = pure seen
  | isMinimal = do
      output (line <> "\n")
      pure $! Set.insert line seen
  | otherwise = output line *> pure seen
