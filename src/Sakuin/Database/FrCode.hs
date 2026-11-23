-- | Front coding based on the original Rust implementation in nix-index.
-- https://github.com/NixOS/nix-index/blob/master/src/frcode.rs
module Sakuin.Database.FrCode
  ( withFrCodeDatabase,
    runFrCodeSearch,
    foldFrCodeRows,
  )
where

import Control.Monad (void)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, byteString, int16BE, toLazyByteString, word8)
import Data.ByteString.Builder.Extra (safeStrategy, toLazyByteStringWith)
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int16, Int8)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static
import Effectful.Exception
import Sakuin.Database.TSV (DatabaseWriter, decompressDatabase, foldIndexedFiles, indexedPathFields, matchesSearchFields, withDatabaseWriter)
import Sakuin.Search
import Sakuin.Types
import System.IO (Handle)
import System.Process.Typed (checkExitCode, getStdout, withProcessWait)

withFrCodeDatabase ::
  forall es a. (Concurrent :> es, IOE :> es) => Int -> Handle -> (DatabaseWriter -> Eff es a) -> Eff es a
withFrCodeDatabase = withDatabaseWriter frCodeMagic encodeFrCodeStorePath

runFrCodeSearch ::
  forall es a. (IOE :> es, Error SearchError :> es) => FilePath -> Bool -> Eff (Search : es) a -> Eff es a
runFrCodeSearch databasePath isMinimal = interpret $ \_ -> \case
  SearchPaths pattern isRegex filters -> do
    matchesPath <- either (throwError . InvalidSearchRegex . T.pack) pure $ pathMatcher pattern isRegex filters
    result <- tryJust synchronous $ withProcessWait (decompressDatabase databasePath) $ \process -> do
      liftIO $ do
        contents <- LBS.hGetContents (getStdout process)
        void $ foldFrCodeRows (printMatch filters matchesPath isMinimal) Set.empty contents
      checkExitCode process
    either (throwError . SearchProcessError . T.pack . displayException) pure result
  where
    synchronous err = case fromException @SomeAsyncException err of
      Just _ -> Nothing
      Nothing -> Just err

printMatch :: SearchFilter -> PathMatcher -> Bool -> Set.Set BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> IO (Set.Set BS.ByteString)
printMatch filters matchesPath isMinimal seen package metadata fullPath
  | isMinimal && Set.member package seen = pure seen
  | not (matchesSearchFields filters matchesPath package metadata fullPath) = pure seen
  | isMinimal = BS.putStr (package <> "\n") *> pure (Set.insert package seen)
  | otherwise = BS.putStr (package <> "\t" <> metadata <> "\t" <> fullPath <> "\n") *> pure seen

frCodeMagic :: LBS.ByteString
frCodeMagic = "SAKUIN-FRCODE\1\n"

-- Each store path starts with a package record ('p' and the package name as
-- metadata, with the full store path as its path). File records have size/type
-- metadata and relative paths. The empty footer resets the differential so
-- each store path can be encoded independently.
encodeFrCodeStorePath :: IndexedStorePath -> LBS.ByteString
encodeFrCodeStorePath indexed = case indexedFiles indexed of
  FileNode (Directory entries) | Map.null entries -> LBS.empty
  files -> toLazyByteString $ header <> foldIndexedFiles encodeFile finish files initial
  where
    (package, storePrefix) = indexedPathFields $ indexedPath indexed
    (header, initial) = encodeFrCodeEntry emptyPrefix ("p" <> byteString (escapeFrCode package)) (escapeFrCode storePrefix)
    encodeFile path metadata rest previous =
      let (entry, next) = encodeFrCodeEntry previous metadata (escapeFrCode $ builderBytes path)
       in entry <> rest next
    finish (PrefixState _ shared) = frCodeRecord mempty (negate shared) BS.empty

data PrefixState = PrefixState !BS.ByteString !Int

emptyPrefix :: PrefixState
emptyPrefix = PrefixState BS.empty 0

encodeFrCodeEntry :: PrefixState -> Builder -> BS.ByteString -> (Builder, PrefixState)
encodeFrCodeEntry (PrefixState previous previousShared) metadata path =
  ( frCodeRecord metadata (shared - previousShared) (BS.drop shared path),
    PrefixState path shared
  )
  where
    shared = commonPrefixLength previous path

frCodeRecord :: Builder -> Int -> BS.ByteString -> Builder
frCodeRecord metadata differential suffix =
  metadata <> word8 0 <> encodeDifferential differential <> byteString suffix <> word8 10

commonPrefixLength :: BS.ByteString -> BS.ByteString -> Int
commonPrefixLength previous path = go 0
  where
    limit = min (fromIntegral (maxBound @Int16)) (min (BS.length previous) (BS.length path))
    go i
      | i < limit && BS.index previous i == BS.index path i = go (i + 1)
      | otherwise = i

encodeDifferential :: Int -> Builder
encodeDifferential diff
  | abs diff < fromIntegral (maxBound @Int8) = word8 (fromIntegral diff)
  | otherwise = word8 0x80 <> int16BE (fromIntegral diff)

-- UTF-8 never uses 0xff, leaving it available to escape the two separators.
escapeFrCode :: BS.ByteString -> BS.ByteString
escapeFrCode bytes = case BS.uncons rest of
  Nothing -> bytes
  Just escaped -> builderBytes $ byteString literal <> escapeRest escaped
  where
    (literal, rest) = splitLiteral bytes
    splitLiteral = BS.break (\byte -> byte == 0 || byte == 10)
    escapeRest (byte, suffix) =
      let (chunk, remaining) = splitLiteral suffix
       in word8 0xff <> word8 (byte + 1) <> byteString chunk <> foldMap escapeRest (BS.uncons remaining)

unescapeFrCode :: BS.ByteString -> Either String BS.ByteString
unescapeFrCode bytes
  | BS.null rest = Right bytes
  | otherwise = builderBytes <$> unescapeRest (byteString literal) rest
  where
    (literal, rest) = splitLiteral bytes
    splitLiteral = BS.break (== 0xff)
    unescapeRest builder remaining = case BS.uncons (BS.drop 1 remaining) of
      Just (1, suffix) -> append builder 0 suffix
      Just (11, suffix) -> append builder 10 suffix
      _ -> Left "invalid escaped byte"
    append builder byte suffix =
      let (chunk, remaining) = splitLiteral suffix
          next = builder <> word8 byte <> byteString chunk
       in if BS.null remaining then Right next else unescapeRest next remaining

builderBytes :: Builder -> BS.ByteString
builderBytes = LBS.toStrict . toLazyByteStringWith (safeStrategy 128 4096) LBS.empty

foldFrCodeRows ::
  (MonadFail m) =>
  (a -> BS.ByteString -> BS.ByteString -> BS.ByteString -> m a) ->
  a ->
  LBS.ByteString ->
  m a
foldFrCodeRows row initial input = case LBS.stripPrefix frCodeMagic input of
  Nothing -> invalid "invalid header or unsupported version"
  Just entries -> nextPackage initial entries
  where
    invalid message = fail $ "frcode database: " <> message
    checked = either invalid pure
    nextPackage acc remaining
      | LBS.null remaining = pure acc
      | otherwise = do
          (metadata, state@(PrefixState path _), rest) <- checked $ decodeFrCodeEntry emptyPrefix remaining
          package <- case BS.stripPrefix "p" metadata of
            Nothing -> invalid "expected a package record"
            Just escaped -> checked $ unescapeFrCode escaped
          storePrefix <- checked $ unescapeFrCode path
          nextFile package storePrefix acc state rest
    nextFile package storePrefix acc previous remaining = do
      (metadata, state@(PrefixState path _), rest) <- checked $ decodeFrCodeEntry previous remaining
      if BS.null metadata && BS.null path
        then nextPackage acc rest
        else case BS.uncons metadata of
          Just (digit, _) | digit >= 48 && digit <= 57 -> do
            relativePath <- checked $ unescapeFrCode path
            next <- row acc package metadata (storePrefix <> relativePath)
            nextFile package storePrefix next state rest
          _ -> invalid "invalid file metadata or missing package footer"

decodeFrCodeEntry :: PrefixState -> LBS.ByteString -> Either String (BS.ByteString, PrefixState, LBS.ByteString)
decodeFrCodeEntry (PrefixState previous previousShared) input = do
  let (metadata, separator) = LBS.break (== 0) input
  (_, encoded) <- maybe (Left "missing metadata terminator") Right $ LBS.uncons separator
  (diff, pathBytes) <- decodeDifferential encoded
  let shared = previousShared + diff
  if shared < 0 || shared > BS.length previous || shared > fromIntegral (maxBound @Int16)
    then Left "shared prefix is out of range"
    else do
      let (suffix, newline) = LBS.break (== 10) pathBytes
      (_, rest) <- maybe (Left "missing path terminator") Right $ LBS.uncons newline
      let path = BS.take shared previous <> LBS.toStrict suffix
      Right (LBS.toStrict metadata, PrefixState path shared, rest)

decodeDifferential :: LBS.ByteString -> Either String (Int, LBS.ByteString)
decodeDifferential input = do
  (first, rest) <- nextByte input
  if first /= 0x80
    then Right (fromIntegral (fromIntegral first :: Int8), rest)
    else do
      (high, lowBytes) <- nextByte rest
      (low, remaining) <- nextByte lowBytes
      let diff = fromIntegral high * 256 + fromIntegral low :: Int16
      Right (fromIntegral diff, remaining)
  where
    nextByte = maybe (Left "missing prefix differential") Right . LBS.uncons
