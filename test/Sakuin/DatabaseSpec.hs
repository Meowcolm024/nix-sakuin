module Sakuin.DatabaseSpec (tests) where

import Codec.Compression.Zstd.Lazy qualified as Zstd
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Map qualified as Map
import Effectful
import Effectful.Concurrent
import Path (toFilePath)
import Path.IO (withSystemTempFile)
import Sakuin.Database
import Sakuin.MemoryDatabase
import Sakuin.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import System.IO (hClose)

tests :: TestTree
tests =
  testGroup
    "Database"
    [ testCase "stores indexed paths by hash and replaces duplicates" $ do
        let storePath = StorePath "/nix/store" "hash" "example"
            entryOrigin = Origin "example" "out" True "aarch64-darwin"
            initial = IndexedStorePath (WithOrigin entryOrigin storePath) (FileNode $ Directory Map.empty)
            replacement =
              IndexedStorePath
                (WithOrigin entryOrigin storePath)
                ( FileNode . Directory . Map.singleton "bin" . FileNode . Directory $
                    Map.singleton "example" (FileNode $ Regular 42 False)
                )
        runEff . runConcurrent $ do
          database <- newMemoryDatabase
          runMemoryDatabase database $ do
            addToDatabase initial
            addToDatabase replacement
          entries <- readMemoryDatabase database
          liftIO $ Map.lookup "hash" entries @?= Just replacement
          encoded <- encodeMemoryDatabase database
          liftIO $ decodeDatabase encoded @?= Right entries
          liftIO $
            LBS8.unpack (formatIndexedStorePath replacement)
              @?= "example.out\t0 d\t/nix/store/hash-example/bin\nexample.out\t42 r\t/nix/store/hash-example/bin/example\n"
          liftIO $ do
            let indexed = WithOrigin entryOrigin storePath
            LBS8.unpack (formatFileLine indexed $ FileLine ("/tool", Regular 7 True))
              @?= "example.out\t7 x\t/nix/store/hash-example/tool\n"
            LBS8.unpack (formatFileLine indexed $ FileLine ("/link", Symlink "tool"))
              @?= "example.out\t0 s\t/nix/store/hash-example/link\n"
          pure (),
      testCase "splits store paths evenly across bounded workers" $
        splitEvenly 3 ([1 .. 8] :: [Int]) @?= [[1, 2, 3], [4, 5, 6], [7, 8]],
      testCase "streams queued database entries through zstd" $
        withSystemTempFile "nix-sakuin-test.tsv.zst" $ \path handle -> do
          hClose handle
          let storePath = StorePath "/nix/store" "hash" "example"
              entryOrigin = Origin "example" "out" True "aarch64-darwin"
              emptyIndexed =
                IndexedStorePath
                  (WithOrigin entryOrigin (StorePath "/nix/store" "empty" "empty"))
                  (FileNode $ Directory Map.empty)
              indexed =
                IndexedStorePath
                  (WithOrigin entryOrigin storePath)
                  (FileNode $ Directory $ Map.singleton "tool" (FileNode $ Regular 7 True))
          runEff . runConcurrent $
            withTsvDatabase 1 (toFilePath path) $ \database ->
              runTsvDatabase database $ do
                addToDatabase emptyIndexed
                addToDatabase indexed
          contents <- Zstd.decompress <$> LBS8.readFile (toFilePath path)
          contents @?= "example.out\t7 x\t/nix/store/hash-example/tool\n"
    ]
