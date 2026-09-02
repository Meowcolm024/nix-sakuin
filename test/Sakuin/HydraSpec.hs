module Sakuin.HydraSpec (tests) where

import Codec.Compression.Lzma qualified as Lzma
import Codec.Compression.Zstd.Lazy qualified as Zstd
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isNothing)
import Effectful
import Effectful.Dispatch.Dynamic
import Sakuin
import Sakuin.Hydra
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

data MockCacheData = MockCacheData
  { mockNarInfos :: Map StoreHash NarInfo,
    mockListings :: Map StoreHash FileNode
  }

runMockCache :: forall es a. MockCacheData -> Eff (Fetch : es) a -> Eff es a
runMockCache cache = interpret $ \_ -> \case
  FetchNarInfo storePath -> pure $ Map.lookup (spHash storePath) (mockNarInfos cache)
  FetchListing storePath -> pure $ Map.lookup (spHash storePath) (mockListings cache)

tests :: TestTree
tests =
  testGroup
    "Hydra"
    [ testCase "parses the decompressed listing fixture" $ do
        listing <- LBS.readFile listingFixture
        parseListing listing @?= Right expectedListing,
      testCase "passes through an uncompressed listing" $ do
        listing <- LBS.readFile listingFixture
        decodeListing listing @?= listing,
      testCase "decodes zstd and xz listings generated at runtime" $ do
        listing <- LBS.readFile listingFixture
        decodeListing (Zstd.compress 3 listing) @?= listing
        decodeListing (Lzma.compress listing) @?= listing,
      testCase "serves fixture data from a mock cache by store hash" $ do
        narinfoBytes <- BS.readFile narinfoFixture
        listingBytes <- LBS.readFile listingFixture
        case (parseNarInfo narinfoBytes, parseListing listingBytes) of
          (Just narinfo, Right listing) -> do
            let storePath = niStorePath narinfo
                storeHash = spHash storePath
                cache =
                  MockCacheData
                    { mockNarInfos = Map.singleton storeHash narinfo,
                      mockListings = Map.singleton storeHash listing
                    }
            (niNarPath <$> runPureEff (runMockCache cache (fetchNarInfo storePath)))
              @?= Just (niNarPath narinfo)
            runPureEff (runMockCache cache (fetchListing storePath)) @?= Just listing
            let missing = StorePath "/nix/store" "missing" "missing"
            isNothing (runPureEff (runMockCache cache (fetchNarInfo missing))) @?= True
            runPureEff (runMockCache cache (fetchListing missing)) @?= Nothing
          (Nothing, _) -> assertFailure "failed to parse narinfo fixture"
          (_, Left err) -> assertFailure err
    ]

listingFixture :: FilePath
listingFixture = "test/assets/5a5lrqlgqqhfd02lp7l8gqdypcckxiqd.ls"

narinfoFixture :: FilePath
narinfoFixture = "test/assets/5a5lrqlgqqhfd02lp7l8gqdypcckxiqd.narinfo"

expectedListing :: FileNode
expectedListing =
  FileNode . Directory $
    Map.singleton
      "bin"
      ( FileNode . Directory $
          Map.fromList
            [ ("agda", FileNode $ Regular 124375104 True),
              ("agda-mode", FileNode $ Regular 21067552 True)
            ]
      )
