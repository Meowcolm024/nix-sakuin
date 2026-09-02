module Sakuin.NixEnvSpec (tests) where

import Data.Map qualified as Map
import Sakuin
import Sakuin.NixEnv (parsePackages)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "NixEnv"
    [ testCase "parses the nix-env package fixture" $ do
        json <- readFileBS "test/assets/packages.json"
        case parsePackages (toString (decodeUtf8 json :: Text)) of
          Left err -> assertFailure err
          Right (Packages packages) ->
            Map.keys packages
              @?= [ "8x37013i8mdk7i7pcr6j45qjaclpi447",
                    "a8gldf6cnq7zqrdq3nqzym0kjy91dxbf",
                    "bryk0z6qn4p4jy10blggrfpq74jh455c",
                    "l6nadiwa0mxax8h3lk301gkl923alg25",
                    "yj989irxax60w546axvwj6y3swmkkq05"
                  ]
    ]
