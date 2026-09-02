module Sakuin.Progress
  ( formatProgress,
    reportProgress,
  )
where

import Control.Monad (forever)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM
import Effectful.Exception (finally)
import Sakuin.WorkQueue
import System.IO (hFlush, stdout)

formatProgress :: Int -> Int -> Text
formatProgress indexed queued =
  T.show indexed <> " paths indexed, " <> T.show queued <> " paths in queue"

reportProgress ::
  forall es k v.
  (Concurrent :> es, IOE :> es) =>
  Eff es Int ->
  WorkQueue k v ->
  Eff es ()
reportProgress getIndexedCount queue = finally loop (liftIO $ clearLine *> hFlush stdout)
  where
    loop = forever $ do
      indexed <- getIndexedCount
      queued <- atomically $ pendingCount queue
      liftIO $ do
        clearLine
        T.putStr $ formatProgress indexed queued
        hFlush stdout
      threadDelay 200000
    clearLine = T.putStr "\r\ESC[2K"
