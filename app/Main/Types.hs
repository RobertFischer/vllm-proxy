module Main.Types
  ( module Main.Types,
    module This.Types,
    SimpleApp,
  )
where

import Database.Redis qualified as Redis
import This
import This.Types

-- | How to structure the output
data LogFormat = YAMLLogFormat | JSONLogFormat | BracketLogFormat

-- | Convenience alias.
type InitM = RIO SimpleApp

-- | Verbosity for logging
data Verbosity
  = -- | 'DebugS' + 'V3'
    VerbosityLoudest
  | -- | 'DebugS' + 'V2'
    VerbosityLouder
  | -- | 'DebugS' + 'V1'
    VerbosityLoud
  | -- | 'DebugS' + 'V0'
    VerbosityDebug
  | -- | 'InfoS'
    VerbosityInfo
  | -- | 'NoticeS'
    VerbosityNotice
  | -- | 'WarningS'
    VerbosityWarn
  | -- | 'ErrorS'
    VerbosityError
  deriving (Eq, Ord, Enum, Bounded)

data GlobalOpts = GlobalOpts
  { optVerbosity :: Verbosity,
    optTimeoutMins :: Natural,
    optLogFormat :: LogFormat,
    optRedis :: Redis.ConnectInfo
  }
