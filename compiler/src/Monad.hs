{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Monad
-- Copyright   : (c) 2012, 2013 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Custom monad support for the compiler.
--
-----------------------------------------------------------------------------

module Monad (Compile (..), runCompileMonad)  where

import Types (CompileState (..))
import Control.Monad.State.Strict as State hiding (State)
-- import Control.Monad.State.Class (MonadState (..))
import Control.Applicative (Applicative (..))

newtype Compile a
   = Compile (StateT CompileState IO a)
   deriving (Monad, Functor, MonadIO, Applicative)

instance MonadState CompileState Compile where
   get = Compile get
   put s = Compile $ put s

runCompileMonad :: Compile a -> CompileState -> IO a
runCompileMonad (Compile comp) = evalStateT comp
