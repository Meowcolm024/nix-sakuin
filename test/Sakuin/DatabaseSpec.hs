module Sakuin.DatabaseSpec (tests) where

import Data.Map qualified as Map
import Sakuin.Database
import Sakuin.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Database"
    [ testCase "stores indexed paths by hash and replaces duplicates" $ do
        database <- newMemoryDatabase
        let storePath = StorePath "/nix/store" "hash" "example"
            entryOrigin = Origin "example" "out" True "aarch64-darwin"
            initial = IndexedStorePath (WithOrigin entryOrigin storePath) (FileNode $ Directory Map.empty)
            replacement =
              IndexedStorePath
                (WithOrigin entryOrigin storePath)
                ( FileNode . Directory . Map.singleton "bin" . FileNode . Directory $
                    Map.singleton "example" (FileNode $ Regular 42 False)
                )
        runDatabaseT database $ do
          addToDatabase initial
          addToDatabase replacement
        entries <- readMemoryDatabase database
        Map.lookup "hash" entries @?= Just replacement
        formatted <- formatMemoryDatabase database
        formatted @?= "hash\t/bin/example\tRegular {size = 42, executable = False}\n"
    ]
