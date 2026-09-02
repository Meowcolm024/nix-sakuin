module Sakuin.HydraSpec (tests) where

import Codec.Compression.Lzma qualified as Lzma
import Codec.Compression.Zstd qualified as Zstd
import Data.Map qualified as Map
import Sakuin
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

data MockCacheData = MockCacheData
  { mockNarInfos :: Map StoreHash NarInfo,
    mockListings :: Map StoreHash FileNode
  }

newtype MockCache a = MockCache {unMockCache :: Reader MockCacheData a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadCache MockCache where
  fetchNarinfo storePath =
    MockCache $ asks (Map.lookup (spHash storePath) . mockNarInfos)

  fetchListing storePath =
    MockCache $ asks (Map.lookup (spHash storePath) . mockListings)

runMockCache :: MockCacheData -> MockCache a -> a
runMockCache cache = (`runReader` cache) . unMockCache

tests :: TestTree
tests =
  testGroup
    "Hydra"
    [ testCase "parses the decompressed listing fixture" $ do
        listing <- readFileBS listingFixture
        parseListing listing @?= Right expectedListing,
      testCase "passes through an uncompressed listing" $ do
        listing <- readFileBS listingFixture
        decodeListing listing @?= Right listing,
      testCase "decodes zstd and xz listings generated at runtime" $ do
        listing <- readFileBS listingFixture
        decodeListing (Zstd.compress 3 listing) @?= Right listing
        decodeListing (toStrict (Lzma.compress (fromStrict listing))) @?= Right listing,
      testCase "serves fixture data from a mock cache by store hash" $ do
        narinfoBytes <- readFileBS narinfoFixture
        listingBytes <- readFileBS listingFixture
        case (parseNarInfo narinfoBytes, parseListing listingBytes) of
          (Just narinfo, Right listing) -> do
            let storePath = niStorePath narinfo
                storeHash = spHash storePath
                cache =
                  MockCacheData
                    { mockNarInfos = Map.singleton storeHash narinfo,
                      mockListings = Map.singleton storeHash listing
                    }
            (niNarPath <$> runMockCache cache (fetchNarinfo storePath))
              @?= Just (niNarPath narinfo)
            runMockCache cache (fetchListing storePath) @?= Just listing
            let missing = StorePath "/nix/store" "missing" "missing"
            isNothing (runMockCache cache (fetchNarinfo missing)) @?= True
            runMockCache cache (fetchListing missing) @?= Nothing
          (Nothing, _) -> assertFailure "failed to parse narinfo fixture"
          (_, Left err) -> assertFailure err
    ]

listingFixture :: FilePath
listingFixture = "test/assets/5a5lrqlgqqhfd02lp7l8gqdypcckxiqd.ls"

narinfoFixture :: FilePath
narinfoFixture = "test/assets/5a5lrqlgqqhfd02lp7l8gqdypcckxiqd.narinfo"

expectedListing :: FileNode
expectedListing =
  Directory $
    Map.singleton
      "bin"
      ( Directory $
          Map.fromList
            [ ("agda", Regular 124375104 True),
              ("agda-mode", Regular 21067552 True)
            ]
      )
