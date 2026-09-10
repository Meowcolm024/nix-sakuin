{-# LANGUAGE QuasiQuotes #-}

module Sakuin.Storage where

import Effectful
import Effectful.Exception (ExitCase (..), generalBracket, onException)
import Path
import Path.IO
import System.IO (Handle, hClose)

withAtomicFile :: forall es a. (IOE :> es) => Path Abs File -> (Handle -> Eff es a) -> Eff es a
withAtomicFile destination action = fst <$> generalBracket acquire release (action . snd)
  where
    acquire = openBinaryTempFile (parent destination) (toFilePath (filename destination) <> ".tmp")
    release (temporaryPath, handle) exitCase = do
      let removeTemporary = removeFile temporaryPath
      liftIO (hClose handle) `onException` removeTemporary
      case exitCase of
        ExitCaseSuccess _ -> renameFile temporaryPath destination `onException` removeTemporary
        ExitCaseException _ -> removeTemporary
        ExitCaseAbort -> removeTemporary

databasePath :: Path Abs Dir -> Path Abs File
databasePath directory = directory </> [relfile|database.tsv.zst|]

resolveDatabaseDir :: forall es. (IOE :> es) => Maybe (Path Abs Dir) -> Eff es (Path Abs Dir)
resolveDatabaseDir configured = do
  directory <- maybe (getXdgDir XdgCache $ parseRelDir "nix-sakuin") pure configured
  createDirIfMissing False directory
  pure directory

fetchCachePath :: forall es. (IOE :> es) => Eff es (Path Abs File)
fetchCachePath = do
  tmpDir <- getTempDir
  pure $ tmpDir </> [relfile|nix-sakuin-fetch-cache.cbor|]
