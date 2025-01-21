module This.Types
  ( App (..),
    AppCmd (..),
    RApp,
    module This.Types.Percent,
    URI,
    ReadmeCmd (..),
    ReadmeDisplayFormat (..),
    ServerCmd (..),
    module RIO.Time,
  )
where

import Control.Monad.Extra (fromMaybeM)
import Database.Redis qualified as Redis
import Katip qualified as K
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wreq qualified as Wreq
import RIO
import RIO.Process
import RIO.Time (NominalDiffTime, UTCTime)
import System.Random.Stateful
import Text.URI
import This.Types.Percent

-- | Convenience alias.
type RApp = RIO App

-- | Common data throughout the app
data App = App
  { appProcessContext :: ProcessContext,
    appWreqOpts :: Wreq.Options,
    appGenM :: AtomicGenM StdGen,
    appLogEnv :: K.LogEnv,
    appLogCtx :: K.LogContexts,
    appLogNs :: K.Namespace,
    appRedis :: (Redis.ConnectInfo, MVar Redis.Connection)
  }

-- Note that this is ineffecient because there is no connection pooling.
-- It is best to get a connection and reuse it, but not all commands need that.
instance Redis.MonadRedis RApp where
  liftRedis r = do
    (connInfo, mvar) <- appRedis <$> ask
    conn <- fromMaybeM (mkConn connInfo mvar) (tryReadMVar mvar)
    liftIO $ Redis.runRedis conn r
    where
      mkConn connInfo mvar = liftIO $ do
        conn <- Redis.checkedConnect connInfo
        unlessM (tryPutMVar mvar conn) (Redis.disconnect conn)
        readMVar mvar

instance K.Katip RApp where
  getLogEnv = appLogEnv <$> ask
  localLogEnv f = local (\env -> env {appLogEnv = f (appLogEnv env)})

instance K.KatipContext RApp where
  getKatipContext = appLogCtx <$> ask
  localKatipContext f = local (\env -> env {appLogCtx = f (appLogCtx env)})
  getKatipNamespace = appLogNs <$> ask
  localKatipNamespace f = local (\env -> env {appLogNs = f (appLogNs env)})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})

data AppCmd
  = ReadmeCmd ReadmeCmd
  | ServerCmd ServerCmd

newtype ReadmeCmd = ReadmeCmd' {readmeDisplayFormat :: ReadmeDisplayFormat}

data ServerCmd = ServerCmd'
  { serverPort :: Warp.Port,
    serverHost :: Warp.HostPreference,
    serverUpstreams :: [URI]
  }

-- Add in "ReadmeDisplayMdv" at some point in the future.
data ReadmeDisplayFormat = ReadmeDisplayPlain
