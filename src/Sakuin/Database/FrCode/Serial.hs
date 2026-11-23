module Sakuin.Database.FrCode.Serial
  ( runFrCodeSearch,
    searchFrCodeRows,
  )
where

import Control.Monad (void, when)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Internal qualified as LBSI
import Data.ByteString.Unsafe qualified as BSU
import Data.Primitive.ByteArray qualified as BA
import Data.Set qualified as Set
import Data.Word (Word8)
import Effectful
import Effectful.Error.Static (Error)
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (castPtr)
import GHC.Exts (RealWorld)
import Sakuin.Database.FrCode.Codec (FrCodeRecord (..), checkFrCode, checkedSharedPrefix, foldFrCodeRecordsWith, frCodeEntries, readFrCodeRecord, shortDifferential, wideDifferential)
import Sakuin.Database.FrCode.Query (FrCodeMatcher, frCodeMatcher, printFrCodeMatches, runFrCodeSearchWith)
import Sakuin.Search (SearchError)
import Sakuin.Types (Search)

runFrCodeSearch ::
  forall es a. (IOE :> es, Error SearchError :> es) => FilePath -> Bool -> Eff (Search : es) a -> Eff es a
runFrCodeSearch = runFrCodeSearchWith frCodeMatcher searchFrCodeRows

searchFrCodeRows :: Bool -> FrCodeMatcher -> (BS.ByteString -> IO ()) -> LBS.ByteString -> IO ()
searchFrCodeRows isMinimal matchesPath output input = do
  contents <- frCodeEntries input
  initial <- newPathBuffer 256
  let printMatches = printFrCodeMatches isMinimal matchesPath output
  void $ foldFrCodeRecordsWith nextStrictRecord beginBufferedPath expandBufferedPath printMatches initial Set.empty (FrCodeCursor contents 0)

-- The offset is within the first strict chunk, or at its end.
data FrCodeCursor = FrCodeCursor !LBS.ByteString !Int

nextStrictRecord :: FrCodeCursor -> Either String (Maybe (FrCodeRecord, FrCodeCursor))
nextStrictRecord (FrCodeCursor LBSI.Empty _) = Right Nothing
nextStrictRecord (FrCodeCursor input@(LBSI.Chunk chunk rest) offset)
  | offset == BS.length chunk = nextStrictRecord (FrCodeCursor rest 0)
  | Just (record, end) <- readStrictRecord chunk offset =
      Right $ Just (record, FrCodeCursor input end)
  | otherwise = do
      -- Reuse the stream parser when any part of the record spans a chunk.
      (record, remaining) <- readFrCodeRecord (LBSI.Chunk (BSU.unsafeDrop offset chunk) rest)
      pure $ Just (record, FrCodeCursor remaining 0)

readStrictRecord :: BS.ByteString -> Int -> Maybe (FrCodeRecord, Int)
readStrictRecord chunk offset = do
  metadataSize <- BS.elemIndex 0 (BSU.unsafeDrop offset chunk)
  let deltaOffset = offset + metadataSize + 1
      remaining = BS.length chunk - deltaOffset
  (diff, suffixOffset) <- case remaining of
    0 -> Nothing
    _
      | first /= 0x80 -> Just (shortDifferential first, deltaOffset + 1)
      | remaining >= 3 ->
          Just (wideDifferential (BSU.unsafeIndex chunk (deltaOffset + 1)) (BSU.unsafeIndex chunk (deltaOffset + 2)), deltaOffset + 3)
      | otherwise -> Nothing
      where
        first = BSU.unsafeIndex chunk deltaOffset
  let suffix = BSU.unsafeDrop suffixOffset chunk
  suffixSize <- BS.elemIndex 10 suffix
  let !record = FrCodeRecord (BSU.unsafeTake metadataSize $ BSU.unsafeDrop offset chunk) diff (BSU.unsafeTake suffixSize suffix)
  pure (record, suffixOffset + suffixSize + 1)

-- The package header stays owned by the parser. Only file paths borrow this
-- buffer, and the serial search consumes each one before the next write.
data PathBuffer = PathBuffer
  { pathStorage :: !(BA.MutableByteArray RealWorld),
    pathView :: !(ForeignPtr Word8),
    pathLength :: !Int,
    pathShared :: !Int
  }

newPathBuffer :: Int -> IO PathBuffer
newPathBuffer capacity = do
  storage <- BA.newPinnedByteArray capacity
  -- The matcher borrows a view that keeps the pinned array alive.
  pure $ PathBuffer storage (BA.mutableByteArrayAsForeignPtr storage) 0 0

beginBufferedPath :: PathBuffer -> BS.ByteString -> IO PathBuffer
beginBufferedPath buffer = writePathSuffix buffer 0

expandBufferedPath :: PathBuffer -> FrCodeRecord -> IO (PathBuffer, BS.ByteString)
expandBufferedPath buffer (FrCodeRecord _ diff suffix) = do
  shared <- checkFrCode $ checkedSharedPrefix (pathLength buffer) (pathShared buffer + diff)
  next <- writePathSuffix buffer shared suffix
  pure (next, BSI.fromForeignPtr (pathView next) 0 (pathLength next))

writePathSuffix :: PathBuffer -> Int -> BS.ByteString -> IO PathBuffer
writePathSuffix buffer shared suffix = do
  let suffixSize = BS.length suffix
  when (suffixSize > maxBound - shared) $ fail "frcode database: path is too long"
  let size = shared + suffixSize
  capacity <- BA.getSizeofMutableByteArray (pathStorage buffer)
  next <-
    if size <= capacity
      then pure buffer
      else do
        let grownCapacity = if capacity <= maxBound `quot` 2 then max size (2 * capacity) else size
        grown <- newPathBuffer grownCapacity
        when (shared > 0) $
          BA.copyMutableByteArray (pathStorage grown) 0 (pathStorage buffer) 0 shared
        pure grown
  when (suffixSize > 0) $
    BSU.unsafeUseAsCStringLen suffix $ \(source, _) ->
      BA.copyPtrToMutableByteArray @IO @Word8 (pathStorage next) shared (castPtr source) suffixSize
  pure $ next {pathLength = size, pathShared = shared}
