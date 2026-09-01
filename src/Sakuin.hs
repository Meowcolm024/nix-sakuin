module Sakuin (Packages (..), queryAllScopes, testFetchNarInfo) where

import Network.HTTP.Client.TLS (newTlsManager)
import Sakuin.Hydra
import Sakuin.NixEnv
import Sakuin.Types

testFetchNarInfo :: StoreEntry -> IO (Maybe NarInfo)
testFetchNarInfo se = do
  let rtio = runCacheIO (fetchNarinfo se)
  mgr <- newTlsManager
  runReaderT rtio mgr
