module Main where

import Cli
import Index

main :: IO ()
main =
  cliParser >>= \case
    Index opts -> runIndex opts
    Locate opts -> print opts
