module Sakuin.ProgressSpec (tests) where

import Sakuin.Progress (formatProgress)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Progress"
    [ testCase "includes queued and active work" $
        formatProgress 12 3 2 @?= "12 paths indexed, 3 paths in queue, 2 active workers"
    ]
