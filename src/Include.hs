module Include (readme) where

import Development.IncludeFile

$(includeFileInSource "./README.md" "readme")
