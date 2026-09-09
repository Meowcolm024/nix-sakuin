module Sakuin.FetchCacheSpec (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map qualified as Map
import Effectful
import Effectful.Concurrent (runConcurrent)
import Sakuin.FetchCache
import Sakuin.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "FetchCache"
    [ testCase "preserves independently cached narinfo and listing values" $ do
        narinfoBytes <- BS.readFile narinfoFixture
        listingBytes <- LBS.readFile listingFixture
        case parseNarInfo narinfoBytes of
          Just narinfo -> do
            snapshot <- runEff . runConcurrent $ do
              cache <- newFetchCacheState Map.empty
              let storeHash = spHash (niStorePath narinfo)
              storeCachedNarInfo cache storeHash (Found narinfoBytes)
              storeCachedListing cache storeHash (Found $ LBS.toStrict listingBytes)
              readFetchCacheState cache
            decodeFetchCache (encodeFetchCache snapshot) @?= Right snapshot
          Nothing -> assertFailure "failed to parse narinfo fixture"
    ]

listingFixture :: FilePath
listingFixture = "test/assets/5a5lrqlgqqhfd02lp7l8gqdypcckxiqd.ls"

narinfoFixture :: FilePath
narinfoFixture = "test/assets/5a5lrqlgqqhfd02lp7l8gqdypcckxiqd.narinfo"
