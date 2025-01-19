module Main.Types
  ( module Main.Types,
    module This.Types,
    SimpleApp,
  )
where

import This
import This.Types

-- | Convenience alias.
type InitM = RIO SimpleApp

-- | Verbosity for logging
data Verbosity
  = -- | 'LevelDebug' + 'setLogVerboseFormat'
    VerbosityLoud
  | -- | 'LevelDebug'
    VerbosityDebug
  | -- | 'LevelInfo'
    VerbosityInfo
  | -- | 'LevelWarn'
    VerbosityWarn
  | -- | 'LevelError'
    VerbosityError
  deriving (Eq, Ord, Enum, Bounded)

data GlobalOpts = GlobalOpts
  { optVerbosity :: Verbosity,
    optTimeoutMins :: Natural
  }
