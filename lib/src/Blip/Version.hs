module Blip.Version (version, versionString) where

-- XXX fixme import
-- import Paths_blip (version)
import Data.Version (showVersion)

version = "0.1.0"
versionString = version

-- versionString :: String
-- versionString = showVersion version
