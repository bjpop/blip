{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Blip.Compiler.Monad
-- Copyright   : (c) 2012, 2013 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Custom monad support for the compiler.
--
-----------------------------------------------------------------------------

module Blip.Compiler.Monad (Compile (..), runCompileMonad)  where

import Blip.Compiler.Types (CompileState (..))
import Control.Monad.State.Strict as State hiding (State)
import Control.Applicative (Applicative (..))

newtype Compile a
   = Compile (StateT CompileState IO a)
   deriving (Monad, Functor, MonadIO, Applicative)

instance MonadState CompileState Compile where
   get = Compile get
   put s = Compile $ put s

runCompileMonad :: Compile a -> CompileState -> IO a
runCompileMonad (Compile comp) = evalStateT comp
