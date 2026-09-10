module Sakuin.NixEnvSpec (tests) where

import Data.ByteString.Lazy qualified as LBS
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
        json <- LBS.readFile "test/assets/packages.json"
        case parsePackages json of
          Left err -> assertFailure err
          Right (Packages packages) ->
            Map.keys packages
              @?= [ "0qjk4lafvk3fzn4p7v1zkc0d80ykn64s",
                    "2l4pgv9hmhsk7q57rk6lp4hpgii084f2",
                    "3ydfd2aiprqgdgsqfyzrkgzhbkpvj3nd",
                    "5a5lrqlgqqhfd02lp7l8gqdypcckxiqd",
                    "8x37013i8mdk7i7pcr6j45qjaclpi447",
                    "a48x7im2w44j0f0i5q036dps4xfdr9qr",
                    "a8gldf6cnq7zqrdq3nqzym0kjy91dxbf",
                    "bryk0z6qn4p4jy10blggrfpq74jh455c",
                    "l699q415vk2jprx3w60yrw580fxfxfjg",
                    "l6nadiwa0mxax8h3lk301gkl923alg25",
                    "q56j1lnni5fg2x8zma8dd7bl65rl42wc",
                    "yj989irxax60w546axvwj6y3swmkkq05"
                  ]
    , testCase "formats nix-env failures with context" $
        formatError (NixEnvExitFailure 17 "evaluation failed")
          @?= "nix-env failed with exit code 17: evaluation failed"
    ]
