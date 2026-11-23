module Sakuin.Database.FrCodeSpec (tests) where

import Codec.Compression.Zstd.Lazy qualified as Zstd
import Control.Exception (IOException, try)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Either (isLeft)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent
import Path (toFilePath)
import Path.IO (withSystemTempFile)
import Sakuin.Database
import Sakuin.Database.FrCode (foldFrCodeRows, searchFrCodeChunks)
import Sakuin.Database.TSV (formatIndexedStorePath)
import Sakuin.Search (pathMatcher)
import Sakuin.Storage (withAtomicFile)
import Sakuin.Types
import System.IO (hClose)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "FrCode"
    [ testCase "writes signed differentials and a footer reset" $ do
        let listing = directory [("dir", directory [("a", regular), ("b", regular)]), ("other", regular)]
            package = indexed "demo" listing
        contents <- writeDatabase [package]
        contents
          @?= magic
            <> "pdemo.out\0\0/nix/store/0123456789abcdfghijklmnpqrsvwxyz-demo\n"
            <> "0 d\0\1dir\n"
            <> "7 x\0\3/a\n"
            <> "7 x\0\1b\n"
            <> "7 x\0\252other\n"
            <> "\0\255\n",
      testCase "round-trips multiple packages, empty trees and separators across chunks" $ do
        let packages =
              [ indexed "multi" $ directory [("bin", directory [("café", regular), ("caffè", regular)]), ("link", FileNode $ Symlink "bin")],
                indexed "empty" $ directory [],
                indexed "root" regular
              ]
        contents <- writeDatabase packages
        decoded <- decodeRows . LBS.fromChunks . map BS.singleton $ LBS.unpack contents
        decoded @?= expectedRows packages
        let special = indexed "pkg\n" $ directory [("a\nb\0\t", regular), ("a\nc", regular)]
            fullPath = "/nix/store/0123456789abcdfghijklmnpqrsvwxyz-pkg\n"
        writeDatabase [special] >>= decodeRows >>= (@?= ["pkg\n.out\t7 x\t" <> fullPath <> "/a\nb\0\t", "pkg\n.out\t7 x\t" <> fullPath <> "/a\nc"]),
      testCase "writes extended differentials and caps long shared prefixes" $ do
        let common = T.replicate 32768 "a"
            package = indexed "long" $ directory [(common <> "x", regular), (common <> "y", regular), ("b", regular)]
        contents <- writeDatabase [package]
        let bytes = LBS.toStrict contents
        assertBool "large positive differential" $ "7 x\0\128\127\254aay\n" `BS.isInfixOf` bytes
        assertBool "large negative differential" $ "7 x\0\128\128\2b\n" `BS.isInfixOf` bytes
        decodeRows contents >>= (@?= expectedRows [package]),
      testCase "reads an empty database and rejects invalid or truncated records" $ do
        writeDatabase [] >>= decodeRows >>= (@?= [])
        let header = magic <> "pempty.out\0\0/nix/store/example\n"
            invalid = ["tsv", magic <> "bad", magic <> "p\0\128\0", magic <> "p\0\255\n", header, header <> "7 x\0\0/unfinished"]
        mapM_ (\contents -> try @IOException (decodeRows contents) >>= assertBool "expected a decoding failure" . isLeft) invalid,
      testCase "searches multiple chunks in order and deduplicates across batches" $ do
        let listing = directory [("bin", directory [("Tool", regular)]), (T.replicate (70 * 1024) "z", regular)]
            first = indexed "first" listing
            third = first {indexedPath = fmap (\path -> path {spName = "third"}) (indexedPath first)}
            packages = [first, indexed "second" listing, third]
            matching = filter (BS.isSuffixOf "/bin/Tool") $ expectedRows packages
            filters = SearchFilter Nothing Nothing ['x'] True True
        contents <- writeDatabase packages
        matcher <- either fail pure $ pathMatcher "/bin/tool" False filters
        collectSearch False filters matcher contents >>= (@?= map (<> "\n") matching)
        collectSearch True filters matcher contents >>= (@?= ["first.out\n", "second.out\n"])
        let filtered = filters {filterPackage = Just "second", filterHash = Just "0123456789abcdfghijklmnpqrsvwxyz"}
        regex <- either fail pure $ pathMatcher "/bin/T..l" True filtered
        collectSearch True filtered regex contents >>= (@?= ["second.out\n"]),
      testCase "rejects a missing footer in a parallel chunk" $ do
        let filters = SearchFilter Nothing Nothing [] False False
            search = collectSearch False filters (const True)
        writeDatabase [] >>= search >>= (@?= [])
        contents <- writeDatabase [indexed "first" $ directory [("file", regular)]]
        let truncated = LBS.take (LBS.length contents - 3) contents
        try @IOException (search truncated) >>= assertBool "expected a decoding failure" . isLeft
    ]

indexed :: Text -> FileNode -> IndexedStorePath
indexed name = IndexedStorePath $ WithOrigin (Origin name "out" True "aarch64-darwin") (StorePath "/nix/store" "0123456789abcdfghijklmnpqrsvwxyz" name)

directory :: [(Text, FileNode)] -> FileNode
directory = FileNode . Directory . Map.fromList

regular :: FileNode
regular = FileNode $ Regular 7 True

magic :: LBS.ByteString
magic = "SAKUIN-FRCODE\1\n"

writeDatabase :: [IndexedStorePath] -> IO LBS.ByteString
writeDatabase packages = withSystemTempFile "nix-sakuin-frcode-test" $ \path initialHandle -> do
  hClose initialHandle
  runEff . runConcurrent $
    withAtomicFile path $ \handle ->
      withDatabase FrCode 1 handle $ \writer ->
        runDatabaseWriter writer $ mapM_ addToDatabase packages
  Zstd.decompress . LBS.fromStrict <$> BS.readFile (toFilePath path)

decodeRows :: LBS.ByteString -> IO [BS.ByteString]
decodeRows contents = reverse <$> foldFrCodeRows addRow [] contents
  where
    addRow rows package metadata path = pure $ BS.intercalate "\t" [package, metadata, path] : rows

expectedRows :: [IndexedStorePath] -> [BS.ByteString]
expectedRows = map LBS.toStrict . LBS8.lines . foldMap formatIndexedStorePath

collectSearch :: Bool -> SearchFilter -> PathMatcher -> LBS.ByteString -> IO [BS.ByteString]
collectSearch isMinimal filters matcher contents = do
  linesSeen <- newIORef []
  searchFrCodeChunks 2 isMinimal filters matcher (\line -> modifyIORef' linesSeen (line :)) contents
  reverse <$> readIORef linesSeen
