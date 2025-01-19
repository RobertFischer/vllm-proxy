module Run.Readme (run) where

import Include (readme)
import RIO.ByteString qualified as SBS
import This

run :: ReadmeCmd -> RApp ()
run opts =
  case readmeDisplayFormat opts of
    ReadmeDisplayPlain -> SBS.putStr readme
