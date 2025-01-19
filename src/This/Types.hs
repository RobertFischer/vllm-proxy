module This.Types
  ( App (..),
    AppCmd (..),
    RApp,
    module This.Types.Percent,
    URI,
    ReadmeCmd (..),
    ReadmeDisplayFormat (..),
    module RIO.Time,
  )
where

import Katip qualified as K
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
  { appLogFunc :: LogFunc,
    appProcessContext :: ProcessContext,
    appWreqOpts :: Wreq.Options,
    appGenM :: AtomicGenM StdGen,
    appLogEnv :: K.LogEnv,
    appLogCtx :: K.LogContexts,
    appLogNs :: K.Namespace
  }

instance K.Katip RApp where
  getLogEnv = appLogEnv <$> ask
  localLogEnv f = local (\env -> env {appLogEnv = f (appLogEnv env)})

instance K.KatipContext RApp where
  getKatipContext = appLogCtx <$> ask
  localKatipContext f = local (\env -> env {appLogCtx = f (appLogCtx env)})
  getKatipNamespace = appLogNs <$> ask
  localKatipNamespace f = local (\env -> env {appLogNs = f (appLogNs env)})

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})

newtype AppCmd
  = ReadmeCmd ReadmeCmd

newtype ReadmeCmd = ReadmeCmd' {readmeDisplayFormat :: ReadmeDisplayFormat}

-- Add in "ReadmeDisplayMdv" at some point in the future.
data ReadmeDisplayFormat = ReadmeDisplayPlain
