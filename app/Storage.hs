{-# LANGUAGE QuasiQuotes #-}

module Storage where

import Path
import Path.IO

databasePath :: Path Abs Dir -> Path Abs File
databasePath directory = directory </> [relfile|database.tsv.zst|]

resolveDatabaseDir :: Maybe (Path Abs Dir) -> IO (Path Abs Dir)
resolveDatabaseDir configured = do
  directory <- maybe (getXdgDir XdgCache $ parseRelDir "nix-sakuin") pure configured
  createDirIfMissing False directory
  pure directory

fetchCachePath :: IO (Path Abs File)
fetchCachePath = do
  tmpDir <- getTempDir
  pure $ tmpDir </> [relfile|nix-sakuin-fetch-cache.cbor|]
