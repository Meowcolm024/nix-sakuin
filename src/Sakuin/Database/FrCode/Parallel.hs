module Sakuin.Database.FrCode.Parallel
  ( runFrCodeParallelSearch,
    searchFrCodeChunks,
  )
where

import Control.Concurrent (getNumCapabilities)
import Control.Monad (foldM, void)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Set qualified as Set
import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.Concurrent.Async (mapConcurrently)
import Effectful.Error.Static (Error)
import Sakuin.Database.Common (matchesSearchFields)
import Sakuin.Database.FrCode.Codec (FrCodeRecord (..), checkFrCode, foldFrCodeRecords, frCodeEntries, readFrCodeRecord)
import Sakuin.Database.FrCode.Query (FrCodeMatcher, formatMatch, frCodeMatcher, runFrCodeSearchWith, writeMatch)
import Sakuin.Search (SearchError)
import Sakuin.Types (PathMatcher, Search, SearchFilter)

runFrCodeParallelSearch ::
  forall es a. (IOE :> es, Error SearchError :> es) => FilePath -> Bool -> Eff (Search : es) a -> Eff es a
runFrCodeParallelSearch = runFrCodeSearchWith frCodeMatcher $ \isMinimal matchesPath output contents -> do
  workers <- getNumCapabilities
  searchFrCodeChunksWith workers isMinimal matchesPath output contents

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
