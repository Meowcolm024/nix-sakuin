module Sakuin.MockRegistrySpec (tests) where

import Data.List
import Data.Map qualified as Map
import Data.Maybe
import Effectful
import Sakuin
import Sakuin.MockRegistry
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "MockRegistry"
    [ testCase "generates a deterministic sorted store set" $ do
        let config = defaultMockRegistryConfig {mockEntryCount = 12, mockSeedCount = 2}
            registryA = generateMockRegistry config
            registryB = generateMockRegistry config
        Map.keys (mockEntries registryA) @?= Map.keys (mockEntries registryB)
        Map.keys (mockEntries registryA) @?= sort (Map.keys (mockEntries registryA)),
      testCase "only seeds are top-level and unreachable entries are omitted" $ do
        let registry =
              generateMockRegistry
                defaultMockRegistryConfig
                  { mockEntryCount = 12,
                    mockSeedCount = 1,
                    mockExternalReferenceCount = 2
                  }
            expected = mockExpected registry
            Packages seeds = mockSeeds registry
        Map.size expected < Map.size (mockEntries registry) @?= True
        Map.keysSet (Map.filter (orToplevel . origin . indexedPath) expected)
          @?= Map.keysSet seeds,
      testCase "serves internal entries and rejects external references" $ do
        let registry =
              generateMockRegistry
                defaultMockRegistryConfig
                  { mockEntryCount = 8,
                    mockSeedCount = 1,
                    mockExternalReferenceCount = 2
                  }
            internal = mockPath (snd (Map.findMin (mockEntries registry)))
        isJust (runPureEff (runMockCache registry (fetchNarInfo internal))) @?= True
        isJust (runPureEff (runMockCache registry (fetchListing internal))) @?= True
        case mockExternalPaths registry of
          external : _ -> do
            isNothing (runPureEff (runMockCache registry (fetchNarInfo external))) @?= True
            isNothing (runPureEff (runMockCache registry (fetchListing external))) @?= True
          [] -> fail "mock registry did not generate the requested external paths"
    ]
