-----------------------------------------------------------------------------
-- |
-- Module      : Scope
-- Copyright   : (c) 2012, 2013 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Support for managing variable scope.
--
-----------------------------------------------------------------------------
module Scope (Scope (..), empty) where

data Scope = Scope {}
    deriving (Eq, Show)

empty :: Scope
empty = Scope {}
