module Include (readme) where

import Data.ByteString.Char8 qualified as C8
import Development.IncludeFile
import RIO
import RIO.ByteString qualified as SBS

$(includeFileInSource "./README.md" "rawReadme")

-- | Provides 'README.md' from source with some clean-up.
readme :: ByteString
readme =
  C8.dropSpace
    $ fromMaybe
      rawReadme
      ("<!-- @format -->" `SBS.stripPrefix` rawReadme)
