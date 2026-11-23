-- | Front coding based on the original Rust implementation in nix-index.
-- https://github.com/NixOS/nix-index/blob/master/src/frcode.rs
module Sakuin.Database.FrCode.Codec
  ( withFrCodeDatabase,
    FrCodeRecord (..),
    foldFrCodeRows,
    foldFrCodePackages,
    foldFrCodeRecords,
    foldFrCodeRecordsWith,
    frCodeEntries,
    checkFrCode,
    readFrCodeRecord,
    checkedSharedPrefix,
    shortDifferential,
    wideDifferential,
  )
where

import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, byteString, int16BE, toLazyByteString, word8)
import Data.ByteString.Builder.Extra (safeStrategy, toLazyByteStringWith)
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int16, Int8)
import Data.Map qualified as Map
import Data.Word (Word8)
import Effectful
import Effectful.Concurrent (Concurrent)
import Sakuin.Database.Common (DatabaseWriter, foldIndexedFiles, indexedPathFields, withDatabaseWriter)
import Sakuin.Types
import System.IO (Handle)

withFrCodeDatabase ::
  forall es a. (Concurrent :> es, IOE :> es) => Int -> Handle -> (DatabaseWriter -> Eff es a) -> Eff es a
withFrCodeDatabase = withDatabaseWriter frCodeMagic encodeFrCodeStorePath

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
foldFrCodePackages rows initial input = frCodeEntries input >>= foldFrCodeRecords nextStreamRecord rows initial

nextStreamRecord :: LBS.ByteString -> Either String (Maybe (FrCodeRecord, LBS.ByteString))
nextStreamRecord remaining
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
foldFrCodeRecords nextRecord packageRows =
  foldFrCodeRecordsWith nextRecord beginPath expandPath packageRows emptyPrefix
  where
    beginPath _ path = pure $ PrefixState path 0
    expandPath previous record = do
      state@(PrefixState path _) <- checkFrCode $ expandFrCodeRecord previous record
      pure (state, path)

foldFrCodeRecordsWith ::
  (MonadFail m) =>
  (source -> Either String (Maybe (FrCodeRecord, source))) ->
  (state -> BS.ByteString -> m state) ->
  (state -> FrCodeRecord -> m (state, BS.ByteString)) ->
  (BS.ByteString -> BS.ByteString -> a -> BS.ByteString -> BS.ByteString -> m a) ->
  state ->
  a ->
  source ->
  m a
foldFrCodeRecordsWith nextRecord beginPath expandPath packageRows = nextPackage
  where
    nextPackage previous acc remaining =
      checkFrCode (nextRecord remaining) >>= \case
        Nothing -> pure acc
        Just (record@(FrCodeRecord metadata _ _), rest) -> do
          PrefixState path _ <- checkFrCode $ expandFrCodeRecord emptyPrefix record
          package <- case BS.stripPrefix "p" metadata of
            Nothing -> checkFrCode $ Left "expected a package record"
            Just escaped -> checkFrCode $ unescapeFrCode escaped
          storePrefix <- checkFrCode $ unescapeFrCode path
          state <- beginPath previous path
          nextFile (packageRows package storePrefix) acc state rest
    nextFile row acc previous remaining = do
      (record@(FrCodeRecord metadata _ _), rest) <-
        checkFrCode $ nextRecord remaining >>= maybe (Left "missing package footer") Right
      (state, path) <- expandPath previous record
      if BS.null metadata && BS.null path
        then nextPackage state acc rest
        else case BS.uncons metadata of
          Just (digit, _) | digit >= 48 && digit <= 57 -> do
            relativePath <- checkFrCode $ unescapeFrCode path
            next <- row acc metadata relativePath
            nextFile row next state rest
          _ -> checkFrCode $ Left "invalid file metadata or missing package footer"
{-# INLINE foldFrCodeRecordsWith #-}

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
expandFrCodeRecord (PrefixState previous previousShared) (FrCodeRecord _ diff suffix) = do
  shared <- checkedSharedPrefix (BS.length previous) (previousShared + diff)
  pure $ PrefixState (BS.take shared previous <> suffix) shared

checkedSharedPrefix :: Int -> Int -> Either String Int
checkedSharedPrefix pathLength shared
  | shared < 0 || shared > pathLength || shared > fromIntegral (maxBound @Int16) =
      Left "shared prefix is out of range"
  | otherwise = Right shared

decodeDifferential :: LBS.ByteString -> Either String (Int, LBS.ByteString)
decodeDifferential input = do
  (first, rest) <- nextByte input
  if first /= 0x80
    then Right (shortDifferential first, rest)
    else do
      (high, lowBytes) <- nextByte rest
      (low, remaining) <- nextByte lowBytes
      Right (wideDifferential high low, remaining)
  where
    nextByte = maybe (Left "missing prefix differential") Right . LBS.uncons

shortDifferential :: Word8 -> Int
shortDifferential byte = fromIntegral (fromIntegral byte :: Int8)

wideDifferential :: Word8 -> Word8 -> Int
wideDifferential high low = fromIntegral (fromIntegral high * 256 + fromIntegral low :: Int16)
