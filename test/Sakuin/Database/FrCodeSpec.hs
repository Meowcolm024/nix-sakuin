module Sakuin.Database.FrCodeSpec (tests) where

import Codec.Compression.Zstd.Lazy qualified as Zstd
import Control.Exception (IOException, try)
import Control.Monad (forM_)
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
import Sakuin.Database.FrCode (foldFrCodeRows, frCodeMatcher, searchFrCodeChunks)
import Sakuin.Database.TSV (formatIndexedStorePath, matchesSearchFields)
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
        try @IOException (search truncated) >>= assertBool "expected a decoding failure" . isLeft,
      testCase "relative matching agrees with full-path filters and legacy store paths" $ do
        let prefix = "/other/store/0123456789abcdfghijklmnpqrsvwxyz-demo"
            nested = "/other/8x37013i8mdk7i7pcr6j45qjaclpi447-parent/0123456789abcdfghijklmnpqrsvwxyz-demo"
            uppercase = "/other/store/0123456789ABCDFGHIJKLMNPQRSVWXYZ-demo"
            filters = SearchFilter Nothing Nothing [] False False
            rooted = filters {filterAtRoot = True, filterWholeName = True}
            filtered = rooted {filterPackage = Just "demo", filterHash = Just "0123456789abcdfghijklmnpqrsvwxyz", filterTypes = ['x']}
            queries =
              [ ("/bin/tool", False, filtered),
                ("/bin/T..l", True, filtered),
                ("/bin/T..l", True, rooted),
                ("/bin/tool", False, rooted),
                ("/bin", False, filters {filterAtRoot = True}),
                ("Tool", False, filters {filterWholeName = True}),
                ("-demo", False, filters),
                (".", True, filters)
              ]
        forM_ queries $ \(pattern, isRegex, options) -> do
          old <- either fail pure $ pathMatcher pattern isRegex options
          new <- either fail pure $ frCodeMatcher pattern isRegex options
          forM_ [prefix, nested, uppercase] $ \storePath ->
            forM_ ["demo.out", "other.out"] $ \package ->
              forM_ ["7 x", "7 r"] $ \metadata ->
                forM_ ["/bin/Tool", "/bin/Tool-extra", "/xx/bin/Tool", "suffix/bin/Tool", ""] $ \relativePath ->
                  new package storePath metadata relativePath
                    @?= matchesSearchFields options old package metadata (storePath <> relativePath),
      testCase "matches ASCII literals in place and preserves Unicode folding" $ do
        let storePath = "/nix/store/0123456789abcdfghijklmnpqrsvwxyz-demo"
            filters = SearchFilter Nothing Nothing [] False False
            atRoot = filters {filterAtRoot = True}
            suffix = filters {filterWholeName = True}
            exact = atRoot {filterWholeName = True}
            cases =
              [ ("pandoc", filters, "/panda/PanDOC", True),
                ("pandoc", filters, "/panda/PanDO", False),
                ("ana", filters, "/bAnAna", True),
                ("a-b", filters, "/A_B", False),
                ("bin/pandoc", atRoot, "/BIN/Pandoc-extra", True),
                ("bin/pandoc", exact, "/BIN/Pandoc-extra", False),
                ("bin/pandoc", exact, "/BIN/Pandoc", True),
                ("pandoc", suffix, "/BIN/Pandoc", True),
                ("x/pandoc", suffix, "/PanDOC", False),
                ("", filters, "/x", True),
                ("", exact, "/x", False),
                ("a\0b", filters, "/A\0B", True),
                ("STRASSE", suffix, "/Stra\195\159e", True),
                ("strasse", exact, "/Stra\195\159e", True),
                ("K", filters, "/K", True),
                ("k", filters, "/\226\132\170", True),
                ("é", filters, "/E", False)
              ]
        forM_ cases $ \(pattern, options, relativePath, expected) -> do
          matcher <- either fail pure $ frCodeMatcher pattern False options
          matcher "demo.out" storePath "7 x" relativePath @?= expected
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
