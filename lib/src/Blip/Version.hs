module Blip.Version (version, versionString) where

import Paths_bliplib (version)
import Data.Version (showVersion)

versionString :: String
versionString = showVersion version
