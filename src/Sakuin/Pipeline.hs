module Sakuin.Pipeline where

import Control.Concurrent.Async (async, cancel, race, waitAnyCatch, waitCatch)
import Control.Exception qualified as Exception
import Control.Monad.Catch qualified as Catch
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Data.Map qualified as Map
import Sakuin.Types
import Sakuin.WorkQueue

seedQueue :: WorkQueue StoreHash (WithOrigin StorePath) -> Packages -> IO ()
seedQueue wq (Packages m) =
  atomically $ mapM_ (\(k, v) -> addWork wq k v) (Map.toList m)

runPipeline ::
  (MonadCache m, MonadDatabase m, MonadUnliftIO m, Catch.MonadMask m) =>
  Int ->
  Packages ->
  m ()
runPipeline workerCount packages
  | workerCount <= 0 = liftIO $ fail "pipeline worker count must be positive"
  | otherwise = do
      wq <- liftIO newWorkQueue
      liftIO $ seedQueue wq packages
      withRunInIO $ \run -> do
        workers <- replicateM workerCount . async $ run (worker wq addToDatabase)
        let stopWorkers = do
              mapM_ cancel workers
              void $ mapM waitCatch workers
            waitForOutcome =
              race
                (atomically $ awaitCompletion wq)
                (waitAnyCatch workers)
        Exception.finally
          ( waitForOutcome >>= \case
              Left () -> pure ()
              Right (_, Left err) -> Exception.throwIO err
              Right (_, Right ()) -> fail "pipeline worker stopped unexpectedly"
          )
          stopWorkers

worker ::
  (MonadCache m, MonadIO m, Catch.MonadMask m) =>
  WorkQueue StoreHash (WithOrigin StorePath) ->
  (IndexedStorePath -> m ()) ->
  m ()
worker wq emit = forever $ workerOnce wq emit

workerOnce ::
  (MonadCache m, MonadIO m, Catch.MonadMask m) =>
  WorkQueue StoreHash (WithOrigin StorePath) ->
  (IndexedStorePath -> m ()) ->
  m ()
workerOnce wq emit =
  Catch.bracket
    (liftIO . atomically $ claim wq)
    (\_ -> liftIO . atomically $ finish wq)
    (\(_, entry) -> process entry)
  where
    process entry = do
      let storePath = value entry
      narinfo <- fetchNarinfo storePath
      listing <- fetchListing storePath
      forM_ narinfo $ \info ->
        liftIO . atomically $
          forM_ (annotateReferences entry info) $ \reference ->
            addWork wq (spHash (value reference)) reference
      forM_ listing $ \files ->
        emit $ IndexedStorePath entry files

referenceOrigin :: Origin -> Origin
referenceOrigin entryOrigin = entryOrigin {orToplevel = False}

annotateReferences :: WithOrigin StorePath -> NarInfo -> [WithOrigin StorePath]
annotateReferences parent narinfo =
  WithOrigin (referenceOrigin (origin parent)) <$> niReferences narinfo
