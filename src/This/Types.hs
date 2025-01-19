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
    appGenM :: AtomicGenM StdGen
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})

newtype AppCmd
  = ReadmeCmd ReadmeCmd

newtype ReadmeCmd = ReadmeCmd' {readmeDisplayFormat :: ReadmeDisplayFormat}

-- Add in "ReadmeDisplayMdv" at some point in the future.
data ReadmeDisplayFormat = ReadmeDisplayPlain
