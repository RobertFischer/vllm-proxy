module Run (run) where

import Run.Readme qualified as Readme
import This

run :: AppCmd -> RApp ()
run = \case
  ReadmeCmd opts -> Readme.run opts
