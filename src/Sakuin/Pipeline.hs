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
import Sakuin.Log
import Sakuin.Progress (reportProgress)
import Sakuin.Types
import Sakuin.WorkQueue

data PipelineConfig es = PipelineConfig
  { pipelineWorkerCount :: Int,
    pipelineFilterPrefix :: Maybe T.Text,
    pipelineIndexedCount :: Maybe (Eff es Int)
  }

defaultPipelineConfig :: PipelineConfig es
defaultPipelineConfig =
  PipelineConfig
    { pipelineWorkerCount = 100,
      pipelineFilterPrefix = Nothing,
      pipelineIndexedCount = Nothing
    }

seedQueue ::
  forall es.
  (Concurrent :> es) =>
  WorkQueue StoreHash (WithOrigin StorePath) -> Packages -> Eff es ()
seedQueue wq (Packages m) =
  atomically $ mapM_ (\(k, v) -> addWork wq k v) (Map.toList m)

runPipeline ::
  forall es.
  (Concurrent :> es, Database :> es, Fetch :> es, Fail :> es, IOE :> es, Log :> es) =>
  PipelineConfig es -> Packages -> Eff es ()
runPipeline config =
  runPipelineInternal
    (pipelineWorkerCount config)
    (pipelineFilterPrefix config)
    (reportProgress <$> pipelineIndexedCount config)

runPipelineInternal ::
  forall es.
  (Concurrent :> es, Database :> es, Fetch :> es, Fail :> es, Log :> es) =>
  Int ->
  Maybe T.Text ->
  Maybe (WorkQueue StoreHash (WithOrigin StorePath) -> Eff es ()) ->
  Packages ->
  Eff es ()
runPipelineInternal workerCount filterPrefix startProgress packages
  | workerCount <= 0 = fail "pipeline worker count must be positive"
  | otherwise = do
      logInfo $ "starting pipeline with " <> T.show workerCount <> " workers"
      wq <- newWorkQueue
      seedQueue wq packages
      progressWorker <- traverse (async . ($ wq)) startProgress
      workers <- replicateM workerCount . async $ worker wq filterPrefix addToDatabase
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
  Maybe T.Text ->
  (IndexedStorePath -> Eff es ()) ->
  Eff es ()
worker wq filterPrefix emit = forever $ workerOnce wq filterPrefix emit

workerOnce ::
  forall es.
  (Concurrent :> es, Fetch :> es, Log :> es) =>
  WorkQueue StoreHash (WithOrigin StorePath) ->
  Maybe T.Text ->
  (IndexedStorePath -> Eff es ()) ->
  Eff es ()
workerOnce wq filterPrefix emit =
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
            forM_ (maybe (Just files) (`filterFileTree` files) filterPrefix) $ \filteredFiles ->
              emit $ IndexedStorePath entry filteredFiles

referenceOrigin :: Origin -> Origin
referenceOrigin entryOrigin = entryOrigin {orToplevel = False}

annotateReferences :: WithOrigin StorePath -> NarInfo -> [WithOrigin StorePath]
annotateReferences parent narinfo =
  WithOrigin (referenceOrigin (origin parent)) <$> niReferences narinfo
