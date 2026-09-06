module Main where

import Cli
import Index
import Locate

main :: IO ()
main =
  cliParser >>= \case
    Index opts -> runIndex opts
    Locate opts -> runLocate opts
