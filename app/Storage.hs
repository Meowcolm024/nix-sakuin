{-# LANGUAGE QuasiQuotes #-}

module Storage where

import Path
import Path.IO

databaseFileName :: Path Rel File
databaseFileName = [relfile|database.tsv.zst|]

databasePath :: Path Abs Dir -> Path Abs File
databasePath directory = directory </> databaseFileName

resolveDatabaseDir :: Maybe (Path Abs Dir) -> IO (Path Abs Dir)
resolveDatabaseDir configured = do
  directory <- maybe (getXdgDir XdgCache $ parseRelDir "nix-sakuin") pure configured
  createDirIfMissing False directory
  pure directory
