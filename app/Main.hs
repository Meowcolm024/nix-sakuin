module Main where

import Cli (Command (..), cliParser)
import Index (runIndex)
import Locate (runLocate)

main :: IO ()
main =
  cliParser >>= \case
    Index opts -> runIndex opts
    Locate opts -> runLocate opts
