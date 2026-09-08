module Sakuin.HydraSpec (tests) where

import Codec.Compression.Brotli qualified as Brotli
import Codec.Compression.Lzma qualified as Lzma
import Codec.Compression.Zstd.Lazy qualified as Zstd
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Either (isLeft)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isNothing)
import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Static (runReader)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types.Header (hContentEncoding)
import Sakuin
import Sakuin.FetchCache
import Sakuin.Hydra
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

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
        runEff (parseListing listing) >>= (@?= expectedListing),
      testCase "decodes an uncompressed listing" $ do
        listing <- LBS.readFile listingFixture
        runEff (decodeListing listing) >>= (@?= Right expectedListing),
      testCase "decodes zstd and xz listings generated at runtime" $ do
        listing <- LBS.readFile listingFixture
        runEff (decodeListing $ Zstd.compress 3 listing) >>= (@?= Right expectedListing)
        runEff (decodeListing $ Lzma.compress listing) >>= (@?= Right expectedListing),
      testCase "reports corrupt compressed listings" $ do
        xzResult <- runEff . decodeListing $ LBS.pack [0xFD, 0x37, 0x7A, 0x58, 0x5A, 0x00]
        assertBool "truncated xz input unexpectedly decoded" $ isLeft xzResult
        brotliResult <- runEff . decodeListing $ decodeResponseBody [(hContentEncoding, "br")] "not brotli"
        assertBool "invalid Brotli input unexpectedly decoded" $ isLeft brotliResult,
      testCase "decodes a Brotli HTTP response body" $ do
        listing <- LBS.readFile listingFixture
        decodeResponseBody [(hContentEncoding, "br")] (Brotli.compress listing) @?= listing,
      testCase "serves cache hits" $ do
        narinfoBytes <- BS.readFile narinfoFixture
        case parseNarInfo narinfoBytes of
          Just cachedNarinfo -> do
            let cachedPath = niStorePath cachedNarinfo
                initialCache =
                  Map.singleton
                    (spHash cachedPath)
                    (FetchCacheEntry (Found cachedNarinfo) NotFetched)
            manager <- newManager defaultManagerSettings
            (cachedResult, snapshot) <-
              runEff
                . runConcurrent
                . runReader manager
                . runLogSilent
                $ runHydraFetchCache initialCache (fetchNarInfo cachedPath)
            cachedResult @?= Just cachedNarinfo
            cachedNarInfo (Map.findWithDefault emptyFetchCacheEntry (spHash cachedPath) snapshot)
              @?= Found cachedNarinfo
          Nothing -> assertFailure "failed to parse narinfo fixture",
      testCase "serves fixture data from a mock cache by store hash" $ do
        narinfoBytes <- BS.readFile narinfoFixture
        listingBytes <- LBS.readFile listingFixture
        case parseNarInfo narinfoBytes of
          Just narinfo -> do
            listing <- runEff $ parseListing listingBytes
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
          Nothing -> assertFailure "failed to parse narinfo fixture"
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
