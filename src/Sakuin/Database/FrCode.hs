-- | Front coding based on the original Rust implementation in nix-index.
-- https://github.com/NixOS/nix-index/blob/master/src/frcode.rs
module Sakuin.Database.FrCode
  ( withFrCodeDatabase,
    runFrCodeSearch,
    runFrCodeParallelSearch,
    runFrCodeParallelRipgrepSearch,
    runFrCodeRipgrepSearch,
    frCodeMatcher,
    foldFrCodeRows,
    searchFrCodeChunks,
  )
where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Exception qualified as E
import Control.Monad (foldM, replicateM_, void)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, byteString, int16BE, toLazyByteString, word8)
import Data.ByteString.Builder.Extra (safeStrategy, toLazyByteStringWith)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int16, Int8)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, decodeUtf8', encodeUtf8)
import Data.Word (Word8)
import Effectful
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Concurrent.Async (concurrently_, mapConcurrently, mapConcurrently_)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static
import Effectful.Exception
import Sakuin.Database.Common (DatabaseWriter, checkRgExitCode, decompressDatabase, foldIndexedFiles, indexedPathFields, matchesFileTypeFilter, matchesPackageFilter, matchesSearchFields, rgBaseArguments, withDatabaseWriter)
import Sakuin.Search
import Sakuin.Types
import System.IO (BufferMode (BlockBuffering), Handle, hClose, hIsEOF, hSetBuffering)
import System.Process.Typed (checkExitCode, createPipe, getStdin, getStdout, proc, setStdin, setStdout, withProcessWait)

withFrCodeDatabase ::
  forall es a. (Concurrent :> es, IOE :> es) => Int -> Handle -> (DatabaseWriter -> Eff es a) -> Eff es a
withFrCodeDatabase = withDatabaseWriter frCodeMagic encodeFrCodeStorePath

runFrCodeSearch ::
  forall es a. (IOE :> es, Error SearchError :> es) => FilePath -> Bool -> Eff (Search : es) a -> Eff es a
runFrCodeSearch = runFrCodeSearchWith $ \_ _ -> searchFrCodeRows

runFrCodeParallelSearch ::
  forall es a. (IOE :> es, Error SearchError :> es) => FilePath -> Bool -> Eff (Search : es) a -> Eff es a
runFrCodeParallelSearch = runFrCodeSearchWith $ \_ _ isMinimal matchesPath output contents -> do
  workers <- getNumCapabilities
  searchFrCodeChunksWith workers isMinimal matchesPath output contents

runFrCodeRipgrepSearch ::
  forall es a. (IOE :> es, Error SearchError :> es) => FilePath -> Bool -> Eff (Search : es) a -> Eff es a
runFrCodeRipgrepSearch = runFrCodeSearchWith (searchFrCodeWithRg Nothing)

runFrCodeParallelRipgrepSearch ::
  forall es a. (IOE :> es, Error SearchError :> es) => FilePath -> Bool -> Eff (Search : es) a -> Eff es a
runFrCodeParallelRipgrepSearch = runFrCodeSearchWith $ \pattern isRegex isMinimal matchesPath output contents -> do
  workers <- getNumCapabilities
  searchFrCodeWithRg (Just workers) pattern isRegex isMinimal matchesPath output contents

runFrCodeSearchWith ::
  forall es a.
  (IOE :> es, Error SearchError :> es) =>
  (Text -> Bool -> Bool -> FrCodeMatcher -> (BS.ByteString -> IO ()) -> LBS.ByteString -> IO ()) ->
  FilePath ->
  Bool ->
  Eff (Search : es) a ->
  Eff es a
runFrCodeSearchWith search databasePath isMinimal = interpret $ \_ -> \case
  SearchPaths pattern isRegex filters -> do
    matchesPath <- either (throwError . InvalidSearchRegex . T.pack) pure $ frCodeMatcher pattern isRegex filters
    result <- tryJust synchronous $ withProcessWait (decompressDatabase databasePath) $ \process -> do
      liftIO $ do
        contents <- LBS.hGetContents (getStdout process)
        search pattern isRegex isMinimal matchesPath BS.putStr contents
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
frCodeMatcher pattern isRegex filters = prepare <$> relativePathMatcher pattern isRegex filters
  where
    foldStorePath = foldsStorePath isRegex filters
    forListing path
      | foldStorePath = T.toCaseFold path
      | otherwise = path
    matchesAscii = asciiLiteralMatcher pattern filters
    matchesRelative matchesPath relativePath
      | not isRegex && BS.all (< 128) relativePath = matchesAscii relativePath
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

searchFrCodeRows :: Bool -> FrCodeMatcher -> (BS.ByteString -> IO ()) -> LBS.ByteString -> IO ()
searchFrCodeRows isMinimal matchesPath output = void . foldFrCodePackages printPackageMatches Set.empty
  where
    printPackageMatches package storePrefix = printMatch
      where
        matchesRow = matchesPath package storePrefix
        printMatch seen metadata relativePath
          | isMinimal && Set.member package seen = pure seen
          | matchesRow metadata relativePath =
              writeMatch isMinimal output seen (formatMatch isMinimal package metadata storePrefix relativePath)
          | otherwise = pure seen

searchFrCodeWithRg :: Maybe Int -> Text -> Bool -> Bool -> FrCodeMatcher -> (BS.ByteString -> IO ()) -> LBS.ByteString -> IO ()
searchFrCodeWithRg workers pattern isRegex isMinimal matchesPath output contents =
  withProcessWait config $ \process -> do
    runEff . runConcurrent $
      concurrently_ (feed (getStdin process)) (liftIO $ drain (getStdout process) Set.empty)
    checkRgExitCode process
  where
    config = setStdin createPipe . setStdout createPipe $ proc "rg" (rgFrCodeArguments pattern isRegex)

    -- Escape the two framing bytes so tabs and newlines in a path survive
    -- the round trip through ripgrep.
    feed input = do
      liftIO $ hSetBuffering input (BlockBuffering Nothing)
      case workers of
        Nothing -> liftIO $ foldFrCodePackages (feedPackage input) () contents
        Just count -> foldFrCodeBatches count (feedBatch input) () contents
      liftIO $ hClose input

    feedPackage input package storePrefix =
      let header = rgRowHeader package storePrefix
       in \() metadata relativePath ->
            BS.hPut input $ BS.concat (rgRowFields header metadata relativePath)

    feedBatch input () chunks = do
      completed <- liftIO newChan
      let publish chunk = encodeChunk chunk >>= writeChan completed
      -- Workers publish complete chunks. Only this consumer writes to rg,
      -- so completion order cannot interleave the bytes of two chunks.
      concurrently_
        (mapConcurrently_ (liftIO . publish) chunks)
        (liftIO $ replicateM_ (length chunks) (readChan completed >>= LBS.hPut input))

    encodeChunk records = do
      rows <- foldFrCodeRecords nextChunkRecord collectPackage [] (reverse records)
      let encoded = toLazyByteString (mconcat $ reverse rows)
      -- Force encoding in the worker, before the single consumer writes it.
      _ <- E.evaluate (LBS.length encoded)
      pure encoded

    collectPackage package storePrefix =
      let header = rgRowHeader package storePrefix
       in \rows metadata relativePath ->
            pure (foldMap byteString (rgRowFields header metadata relativePath) : rows)

    rgRowHeader package storePrefix = BS.concat [escapeFrCode package, "\0", escapeFrCode storePrefix, "\0"]
    rgRowFields header metadata relativePath = [header, metadata, "\0", escapeFrCode relativePath, "\n"]

    drain input seen = do
      atEnd <- hIsEOF input
      if atEnd
        then pure ()
        else do
          line <- BS8.hGetLine input
          seen' <- case BS.split 0 line of
            [packageBytes, storeBytes, metadata, pathBytes] -> do
              package <- checkFrCode $ unescapeFrCode packageBytes
              if isMinimal && Set.member package seen
                then pure seen
                else do
                  storePrefix <- checkFrCode $ unescapeFrCode storeBytes
                  relativePath <- checkFrCode $ unescapeFrCode pathBytes
                  if matchesPath package storePrefix metadata relativePath
                    then writeMatch isMinimal output seen (formatMatch isMinimal package metadata storePrefix relativePath)
                    else pure seen
            _ -> fail "invalid FrCode row from rg"
          drain input seen'

rgFrCodeArguments :: Text -> Bool -> [String]
rgFrCodeArguments pattern isRegex =
  ["--no-config"] <> rgBaseArguments <> prefilter <> ["--", "-"]
  where
    -- A plain regex (possibly anchored) has a required literal. Other regexes
    -- go through unchanged to the exact matcher after ripgrep passes the rows.
    unanchored = fromMaybe pattern (T.stripPrefix "^" pattern)
    literal = fromMaybe unanchored (T.stripSuffix "$" unanchored)
    printableAscii = T.all (\c -> c >= ' ' && c <= '~')
    regexMeta c = c `elem` ("\\.^$|?*+()[]{}" :: String)
    quote c = if regexMeta c then T.pack ['\\', c] else T.singleton c
    prefilter
      | isRegex && printableAscii literal && not (T.any regexMeta literal) =
          ["--fixed-strings", "-e", T.unpack literal]
      | not isRegex && printableAscii pattern =
          -- Unicode full case folding can expand a character (e.g. ß to ss).
          -- Send non-ASCII rows to the Haskell matcher to preserve those hits.
          ["--ignore-case", "-e", T.unpack (T.concatMap quote pattern), "-e", "(?-u:[\\x80-\\xff])"]
      | otherwise = ["-e", ""]

searchFrCodeChunks :: Int -> Bool -> SearchFilter -> PathMatcher -> (BS.ByteString -> IO ()) -> LBS.ByteString -> IO ()
searchFrCodeChunks workers isMinimal filters matchesPath =
  searchFrCodeChunksWith workers isMinimal $ \package storePrefix metadata relativePath ->
    matchesSearchFields filters matchesPath package metadata (storePrefix <> relativePath)

searchFrCodeChunksWith :: Int -> Bool -> FrCodeMatcher -> (BS.ByteString -> IO ()) -> LBS.ByteString -> IO ()
searchFrCodeChunksWith workers isMinimal matchesPath output contents =
  runEff . runConcurrent $ void $ foldFrCodeBatches workers searchBatch Set.empty contents
  where
    searchBatch seen chunks = do
      matches <- mapConcurrently (liftIO . searchChunk seen) chunks
      liftIO $ foldM (foldM $ writeMatch isMinimal output) seen matches

    searchChunk seen reversedRecords = do
      (_, matches) <- foldFrCodeRecords nextChunkRecord collectMatch (seen, []) (reverse reversedRecords)
      pure $! reverse matches

    collectMatch package storePrefix = collect
      where
        matchesRow = matchesPath package storePrefix
        collect acc@(seen, matches) metadata relativePath
          | isMinimal && Set.member package seen = pure acc
          | matchesRow metadata relativePath = do
              let !line = formatMatch isMinimal package metadata storePrefix relativePath
                  !seen' = if isMinimal then Set.insert package seen else seen
              pure (seen', line : matches)
          | otherwise = pure acc

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
foldFrCodeRows row = foldFrCodePackages $ \package storePrefix acc metadata relativePath ->
  row acc package metadata (storePrefix <> relativePath)

foldFrCodePackages ::
  (MonadFail m) =>
  (BS.ByteString -> BS.ByteString -> a -> BS.ByteString -> BS.ByteString -> m a) ->
  a ->
  LBS.ByteString ->
  m a
foldFrCodePackages rows initial input = frCodeEntries input >>= foldFrCodeRecords nextRecord rows initial
  where
    nextRecord remaining
      | LBS.null remaining = Right Nothing
      | otherwise = Just <$> readFrCodeRecord remaining

frCodeEntries :: (MonadFail m) => LBS.ByteString -> m LBS.ByteString
frCodeEntries = checkFrCode . maybe (Left "invalid header or unsupported version") Right . LBS.stripPrefix frCodeMagic

checkFrCode :: (MonadFail m) => Either String a -> m a
checkFrCode = either (fail . ("frcode database: " <>)) pure

data FrCodeRecord = FrCodeRecord !BS.ByteString !Int !BS.ByteString

foldFrCodeRecords ::
  (MonadFail m) =>
  (source -> Either String (Maybe (FrCodeRecord, source))) ->
  (BS.ByteString -> BS.ByteString -> a -> BS.ByteString -> BS.ByteString -> m a) ->
  a ->
  source ->
  m a
foldFrCodeRecords nextRecord packageRows = nextPackage
  where
    nextPackage acc remaining =
      checkFrCode (nextRecord remaining) >>= \case
        Nothing -> pure acc
        Just (record@(FrCodeRecord metadata _ _), rest) -> do
          state@(PrefixState path _) <- checkFrCode $ expandFrCodeRecord emptyPrefix record
          package <- case BS.stripPrefix "p" metadata of
            Nothing -> checkFrCode $ Left "expected a package record"
            Just escaped -> checkFrCode $ unescapeFrCode escaped
          storePrefix <- checkFrCode $ unescapeFrCode path
          nextFile (packageRows package storePrefix) acc state rest
    nextFile row acc previous remaining = do
      (record@(FrCodeRecord metadata _ _), rest) <-
        checkFrCode $ nextRecord remaining >>= maybe (Left "missing package footer") Right
      state@(PrefixState path _) <- checkFrCode $ expandFrCodeRecord previous record
      if BS.null metadata && BS.null path
        then nextPackage acc rest
        else case BS.uncons metadata of
          Just (digit, _) | digit >= 48 && digit <= 57 -> do
            relativePath <- checkFrCode $ unescapeFrCode path
            next <- row acc metadata relativePath
            nextFile row next state rest
          _ -> checkFrCode $ Left "invalid file metadata or missing package footer"

foldFrCodeBatches :: (IOE :> es) => Int -> (a -> [[FrCodeRecord]] -> Eff es a) -> a -> LBS.ByteString -> Eff es a
foldFrCodeBatches workers consume initial contents = liftIO (frCodeEntries contents) >>= go initial
  where
    go acc remaining = do
      (chunks, rest) <- liftIO $ readChunks (max 1 workers) remaining
      case chunks of
        [] -> pure acc
        _ -> do
          next <- consume acc chunks
          go next rest

nextChunkRecord :: [FrCodeRecord] -> Either String (Maybe (FrCodeRecord, [FrCodeRecord]))
nextChunkRecord [] = Right Nothing
nextChunkRecord (record : rest) = Right $ Just (record, rest)

-- Frame the stream once on the reader. Only a package footer resets the
-- prefix state, so a package larger than the chunk target stays together.
readChunks :: Int -> LBS.ByteString -> IO ([[FrCodeRecord]], LBS.ByteString)
readChunks workers = checkFrCode . batch workers []
  where
    batch count chunks remaining
      | count == 0 || LBS.null remaining = Right (reverse chunks, remaining)
      | otherwise = do
          (records, rest) <- chunk [] 0 remaining
          batch (count - 1) (records : chunks) rest
    chunk records !size remaining = do
      (record@(FrCodeRecord metadata _ suffix), rest) <- readFrCodeRecord remaining
      let records' = record : records
          size' = size + BS.length metadata + BS.length suffix + 4
      if (size' >= 64 * 1024 && BS.null metadata) || LBS.null rest
        then Right (records', rest)
        else chunk records' size' rest

readFrCodeRecord :: LBS.ByteString -> Either String (FrCodeRecord, LBS.ByteString)
readFrCodeRecord input = do
  let (metadata, separator) = LBS.break (== 0) input
  (_, encoded) <- maybe (Left "missing metadata terminator") Right $ LBS.uncons separator
  (diff, pathBytes) <- decodeDifferential encoded
  let (suffix, newline) = LBS.break (== 10) pathBytes
  (_, rest) <- maybe (Left "missing path terminator") Right $ LBS.uncons newline
  let !record = FrCodeRecord (LBS.toStrict metadata) diff (LBS.toStrict suffix)
  pure (record, rest)

expandFrCodeRecord :: PrefixState -> FrCodeRecord -> Either String PrefixState
expandFrCodeRecord (PrefixState previous previousShared) (FrCodeRecord _ diff suffix)
  | shared < 0 || shared > BS.length previous || shared > fromIntegral (maxBound @Int16) =
      Left "shared prefix is out of range"
  | otherwise = Right $ PrefixState (BS.take shared previous <> suffix) shared
  where
    shared = previousShared + diff

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
