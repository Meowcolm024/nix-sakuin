module Index where

import Cli
import Data.Map qualified as Map
import Sakuin

runIndex :: IndexOptions -> IO ()
runIndex opts = do
  print opts
  Packages pkgs <- queryAllScopes "<nixpkgs>" (indexSystem opts) (indexExtraScopes opts)
  print $ length pkgs
  forM_ (take 5 $ Map.toList pkgs) $ \(k, v) -> do
    putTextLn $ k <> ": " <> show v
    testFetchNarInfo v >>= print
  hFlush stdout
