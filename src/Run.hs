module Run (run) where

import Run.Readme qualified as Readme
import Run.Server qualified as Server
import This

run :: AppCmd -> RApp ()
run = \case
  ReadmeCmd opts -> Readme.run opts
  ServerCmd opts -> Server.run opts
