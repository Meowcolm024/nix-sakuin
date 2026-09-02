module Sakuin.Pipeline where

import Data.Map qualified as Map
import Sakuin.Types
import Sakuin.WorkQueue

seedQueue :: WorkQueue StoreHash (WithOrigin StorePath) -> Packages -> IO ()
seedQueue wq (Packages m) =
  atomically $ mapM_ (\(k, v) -> addWork wq k v) (Map.toList m)
