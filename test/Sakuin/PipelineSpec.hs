module Sakuin.PipelineSpec (tests) where

import Sakuin.MockRegistry
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Pipeline"
    [ testCase "indexes exactly the reachable generated registry entries" $ do
        let registry =
              generateMockRegistry
                defaultMockRegistryConfig
                  { mockEntryCount = 24,
                    mockSeedCount = 5,
                    mockExternalReferenceCount = 4
                  }
        actual <- runMockPipeline 4 registry
        actual @?= mockExpected registry,
      testCase "accepts an empty generated seed set" $ do
        let registry =
              generateMockRegistry
                defaultMockRegistryConfig
                  { mockEntryCount = 12,
                    mockSeedCount = 0
                  }
        actual <- runMockPipeline 2 registry
        actual @?= mockExpected registry
    ]
