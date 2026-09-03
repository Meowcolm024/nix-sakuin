module Sakuin.Log
  ( Log,
    runLog,
    runLogWith,
    runLogSilent,
    setupLogger,
    cleanupLogger,
    logM,
    logInfo,
    logWarn,
    logErr,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import System.IO (stderr)
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (streamHandler)
import System.Log.Logger qualified as L

data Log :: Effect where
  Log :: forall m. L.Priority -> Text -> Log m ()

type instance DispatchOf Log = Dynamic

runLog :: forall es a. (IOE :> es) => L.Logger -> Eff (Log : es) a -> Eff es a
runLog logger = runLogWith $ \prio msg -> L.logL logger prio (T.unpack msg)

runLogWith ::
  forall es a.
  (IOE :> es) =>
  (L.Priority -> Text -> IO ()) ->
  Eff (Log : es) a ->
  Eff es a
runLogWith writeLog = interpret $ \_ (Log priority message) ->
  liftIO $ writeLog priority message

runLogSilent :: forall es a. Eff (Log : es) a -> Eff es a
runLogSilent = interpret $ \_ (Log _ _) -> pure ()

setupLogger :: Int -> IO L.Logger
setupLogger v
  | v < 0 = fail "Verbosity level must be >= 0"
  | otherwise = do
      let level = [L.ERROR, L.WARNING, L.INFO] !! min v 2
      consoleHandler <- streamHandler stderr level
      let formattedConsoleHandler =
            setFormatter consoleHandler (simpleLogFormatter "\r\ESC[2K[$prio] $msg")
      L.updateGlobalLogger
        L.rootLoggerName
        (L.setLevel level . L.setHandlers [formattedConsoleHandler])
      L.getRootLogger

cleanupLogger :: IO ()
cleanupLogger = L.removeAllHandlers

logM :: forall es. (Log :> es) => L.Priority -> Text -> Eff es ()
logM prio msg = send (Log prio msg)

logInfo :: forall es. (Log :> es) => Text -> Eff es ()
logInfo = logM L.INFO

logWarn :: forall es. (Log :> es) => Text -> Eff es ()
logWarn = logM L.WARNING

logErr :: forall es. (Log :> es) => Text -> Eff es ()
logErr = logM L.ERROR
