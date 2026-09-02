module Sakuin.Pipeline where

import Control.Monad
import Data.Map qualified as Map
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async
import Effectful.Concurrent.STM
import Effectful.Exception
import Effectful.Fail
import Sakuin.Types
import Sakuin.WorkQueue

seedQueue ::
  forall es.
  (Concurrent :> es) =>
  WorkQueue StoreHash (WithOrigin StorePath) -> Packages -> Eff es ()
seedQueue wq (Packages m) =
  atomically $ mapM_ (\(k, v) -> addWork wq k v) (Map.toList m)

runPipeline ::
  forall es.
  (Concurrent :> es, Database :> es, Fetch :> es, Fail :> es) =>
  Int ->
  Packages ->
  Eff es ()
runPipeline workerCount packages
  | workerCount <= 0 = fail "pipeline worker count must be positive"
  | otherwise = do
      wq <- newWorkQueue
      seedQueue wq packages
      workers <- replicateM workerCount . async $ (worker wq addToDatabase)
      let stopWorkers = do
            mapM_ cancel workers
            void $ mapM waitCatch workers
          waitForOutcome =
            race
              (atomically $ awaitCompletion wq)
              (waitAnyCatch workers)
      finally
        ( waitForOutcome >>= \case
            Left () -> pure ()
            Right (_, Left err) -> throwIO err
            Right (_, Right ()) -> fail "pipeline worker stopped unexpectedly"
        )
        stopWorkers

worker ::
  forall es.
  (Concurrent :> es, Fetch :> es) =>
  WorkQueue StoreHash (WithOrigin StorePath) ->
  (IndexedStorePath -> Eff es ()) ->
  Eff es ()
worker wq emit = forever $ workerOnce wq emit

workerOnce ::
  forall es.
  (Concurrent :> es, Fetch :> es) =>
  WorkQueue StoreHash (WithOrigin StorePath) ->
  (IndexedStorePath -> Eff es ()) ->
  Eff es ()
workerOnce wq emit =
  bracket
    (atomically $ claim wq)
    (\_ -> atomically $ finish wq)
    (\(_, entry) -> process entry)
  where
    process entry = do
      let storePath = value entry
      narinfo <- fetchNarInfo storePath
      listing <- fetchListing storePath
      forM_ narinfo $ \info ->
        atomically $
          forM_ (annotateReferences entry info) $ \reference ->
            addWork wq (spHash (value reference)) reference
      forM_ listing $ \files ->
        emit $ IndexedStorePath entry files

referenceOrigin :: Origin -> Origin
referenceOrigin entryOrigin = entryOrigin {orToplevel = False}

annotateReferences :: WithOrigin StorePath -> NarInfo -> [WithOrigin StorePath]
annotateReferences parent narinfo =
  WithOrigin (referenceOrigin (origin parent)) <$> niReferences narinfo
