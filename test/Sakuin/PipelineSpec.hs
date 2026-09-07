module Sakuin.PipelineSpec (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map qualified as Map
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static (runErrorNoCallStack, runErrorNoCallStackWith)
import Effectful.Fail
import Sakuin
import Sakuin.Hydra (parseListing)
import Sakuin.MemoryDatabase
import Sakuin.MockRegistry
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

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
        actual @?= mockExpected registry,
      testCase "rejects a non-positive worker count" $ do
        let registry = generateMockRegistry defaultMockRegistryConfig
        result <- runEff . runErrorNoCallStack . runConcurrent . runLogSilent $ do
          database <- newMemoryDatabase
          runMockDatabase database . runMockCache registry $
            runPipeline defaultPipelineConfig {pipelineWorkerCount = 0} (mockSeeds registry)
        result @?= Left (InvalidPipelineWorkerCount 0),
      testCase "filters a fixture listing during indexing" $ do
        narinfoBytes <- BS.readFile "test/assets/8x37013i8mdk7i7pcr6j45qjaclpi447.narinfo"
        listingBytes <- LBS.readFile "test/assets/8x37013i8mdk7i7pcr6j45qjaclpi447.ls"
        case (parseNarInfo narinfoBytes, parseListing listingBytes) of
          (Just narinfo, Right listing) -> do
            let fixturePath = niStorePath narinfo
                entryOrigin = Origin "alex" "out" True "aarch64-darwin"
                packages = Packages $ Map.singleton (spHash fixturePath) (WithOrigin entryOrigin fixturePath)
                runFixtureFetch = interpret $ \_ -> \case
                  FetchNarInfo _ -> pure $ Just narinfo {niReferences = []}
                  FetchListing _ -> pure $ Just listing
            entries <-
              runEff
                . runFailIO
                . runErrorNoCallStackWith (fail . T.unpack . formatError @PipelineError)
                . runConcurrent
                . runLogSilent
                $ do
              database <- newMemoryDatabase
              runMemoryDatabase database . runFixtureFetch $
                runPipeline
                  defaultPipelineConfig
                    { pipelineWorkerCount = 1,
                      pipelineFilterPrefix = Just "/bin/"
                    }
                  packages
              readMemoryDatabase database
            (toFileList . indexedFiles <$> Map.lookup (spHash fixturePath) entries)
              @?= Just
                [ FileLine ("/bin", Directory ()),
                  FileLine ("/bin/alex", Regular 21807552 True)
                ]
          (Nothing, _) -> assertFailure "failed to parse narinfo fixture"
          (_, Left err) -> assertFailure err
    ]
