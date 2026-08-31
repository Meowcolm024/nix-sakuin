module Main where

import Cli
import Data.Version (showVersion)
import Index
import Paths_nix_sakuin (version)

main :: IO ()
main =
  cliParser >>= \case
    Index opts -> runIndex opts
    Locate opts -> print opts
    Version -> putStrLn $ "nix-sakuin " <> showVersion version
