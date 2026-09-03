module Sakuin.LogSpec (tests) where

import Data.IORef
import Effectful
import Sakuin.Log
import System.Log.Logger qualified as L
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Log"
    [ testCase "forwards levels and messages to the configured writer" $ do
        entries <- newIORef []
        runEff . runLogWith (\priority message -> modifyIORef' entries (<> [(priority, message)])) $ do
          logInfo "info"
          logWarn "warning"
          logErr "error"
        readIORef entries
          >>= ( @?= [ (L.INFO, "info"),
                       (L.WARNING, "warning"),
                       (L.ERROR, "error")
                     ]
              ),
      testCase "can discard logs" $
        runPureEff (runLogSilent (logInfo "ignored")) @?= ()
    ]
