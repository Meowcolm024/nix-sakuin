module Sakuin.WorkQueue where

import Control.Monad (unless)
import Data.Set (Set)
import Data.Set qualified as Set
import Effectful
import Effectful.Concurrent.STM

data WorkQueue k v = WorkQueue
  { wqSeen :: TVar (Set k),
    wqPending :: TQueue (k, v),
    wqPendingCount :: TVar Int,
    wqActive :: TVar Int
  }

newWorkQueue :: forall es k v. (Concurrent :> es) => Eff es (WorkQueue k v)
newWorkQueue =
  WorkQueue
    <$> newTVarIO Set.empty
    <*> newTQueueIO
    <*> newTVarIO 0
    <*> newTVarIO 0

addWork :: (Ord k) => WorkQueue k v -> k -> v -> STM ()
addWork wq k v = do
  seen <- readTVar (wqSeen wq)
  unless (Set.member k seen) $ do
    modifyTVar' (wqSeen wq) (Set.insert k)
    writeTQueue (wqPending wq) (k, v)
    modifyTVar' (wqPendingCount wq) (+ 1)

claim :: WorkQueue k v -> STM (k, v)
claim wq = do
  item <- readTQueue (wqPending wq)
  modifyTVar' (wqPendingCount wq) (subtract 1)
  modifyTVar' (wqActive wq) (+ 1)
  pure item

finish :: WorkQueue k v -> STM ()
finish wq = modifyTVar' (wqActive wq) (subtract 1)

pendingCount :: WorkQueue k v -> STM Int
pendingCount = readTVar . wqPendingCount

awaitCompletion :: WorkQueue k v -> STM ()
awaitCompletion wq = do
  isActive <- readTVar (wqActive wq)
  isEmpty <- isEmptyTQueue (wqPending wq)
  check (isActive == 0 && isEmpty)
