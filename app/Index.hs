module Index where

import Cli
import Sakuin

runIndex :: IndexOptions -> IO ()
runIndex opts = do
  print opts
  Packages pkgs <- queryAllScopes "<nixpkgs>" (indexSystem opts) (indexExtraScopes opts)
  putTextLn $ "package count: " <> show (length pkgs)
  hFlush stdout
