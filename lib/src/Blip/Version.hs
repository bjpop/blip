-----------------------------------------------------------------------------
-- |
-- Module      : Blip.Version
-- Copyright   : (c) 2012, 2013 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Version number of Blip, derived magically from cabal file.
--
-----------------------------------------------------------------------------
module Blip.Version (version, versionString) where

import Paths_bliplib (version)
import Data.Version (showVersion)

versionString :: String
versionString = showVersion version
