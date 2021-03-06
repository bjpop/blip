-----------------------------------------------------------------------------
-- |
-- Module      : ProgName
-- Copyright   : (c) 2012, 2013 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Define the name of the compiler program, for consistent use in the rest
-- of the code, such as error messages.
--
-----------------------------------------------------------------------------

module ProgName (progName) where

progName :: String
progName = "blip"
