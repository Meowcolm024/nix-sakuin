module Sakuin.Pipeline where

import Control.Monad
import Data.Foldable (traverse_)
import Data.Map qualified as Map
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async
import Effectful.Concurrent.STM
import Effectful.Exception
import Effectful.Fail
import Sakuin.Progress (reportProgress)
import Sakuin.Log
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
  (Concurrent :> es, Database :> es, Fetch :> es, Fail :> es, Log :> es) =>
  Int -> Packages -> Eff es ()
runPipeline workerCount = runPipelineInternal workerCount Nothing

runPipelineWithProgress ::
  forall es.
  (Concurrent :> es, Database :> es, Fetch :> es, Fail :> es, IOE :> es, Log :> es) =>
  Int -> Eff es Int -> Packages -> Eff es ()
runPipelineWithProgress workerCount getIndexedCount =
  runPipelineInternal workerCount (Just $ reportProgress getIndexedCount)

runPipelineInternal ::
  forall es.
  (Concurrent :> es, Database :> es, Fetch :> es, Fail :> es, Log :> es) =>
  Int ->
  Maybe (WorkQueue StoreHash (WithOrigin StorePath) -> Eff es ()) ->
  Packages ->
  Eff es ()
runPipelineInternal workerCount startProgress packages
  | workerCount <= 0 = fail "pipeline worker count must be positive"
  | otherwise = do
      logInfo $ "starting pipeline with " <> T.show workerCount <> " workers"
      wq <- newWorkQueue
      seedQueue wq packages
      progressWorker <- traverse (async . ($ wq)) startProgress
      workers <- replicateM workerCount . async $ (worker wq addToDatabase)
      let stopWorkers = do
            traverse_ cancel progressWorker
            mapM_ cancel workers
            void $ traverse waitCatch progressWorker
            void $ mapM waitCatch workers
          waitForOutcome =
            race
              (atomically $ awaitCompletion wq)
              (waitAnyCatch workers)
      finally
        ( waitForOutcome >>= \case
            Left () -> logInfo "pipeline complete"
            Right (_, Left err) -> throwIO err
            Right (_, Right ()) -> fail "pipeline worker stopped unexpectedly"
        )
        stopWorkers

worker ::
  forall es.
  (Concurrent :> es, Fetch :> es, Log :> es) =>
  WorkQueue StoreHash (WithOrigin StorePath) ->
  (IndexedStorePath -> Eff es ()) ->
  Eff es ()
worker wq emit = forever $ workerOnce wq emit

workerOnce ::
  forall es.
  (Concurrent :> es, Fetch :> es, Log :> es) =>
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
      fetched <- try @SomeException $ (,) <$> fetchNarInfo storePath <*> fetchListing storePath
      case fetched of
        Left err -> case fromException @SomeAsyncException err of
          Just asyncErr -> throwIO asyncErr
          Nothing ->
            logWarn $
              "skipping " <> spHash storePath <> "-" <> spName storePath <> ": " <> T.pack (displayException err)
        Right (narinfo, listing) -> do
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
