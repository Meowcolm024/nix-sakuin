module Main where

import Cli
import Data.Map qualified as Map
import Data.Version (showVersion)
import Paths_nix_sakuin (version)
import Sakuin

main :: IO ()
main = do
  cmd <- cliParser
  case cmd of
    Index opts -> do
      print opts
      Packages pkgs <- queryPackages "<nixpkgs>" Nothing Nothing
      print $ length pkgs
      forM_ (Map.toList pkgs) $ \(k, v) ->
        putTextLn $ k <> ": " <> show v
      hFlush stdout
    Locate opts -> print opts
    Version -> putStrLn $ "nix-sakuin " <> showVersion version