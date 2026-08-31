module Main where

import Cli
import Data.Version (showVersion)
import Paths_nix_sakuin (version)

-- import Sakuin

main :: IO ()
main = do
  cmd <- cliParser
  case cmd of
    Index opts -> print opts
    Locate opts -> print opts
    Version -> putStrLn $ "nix-sakuin " <> showVersion version