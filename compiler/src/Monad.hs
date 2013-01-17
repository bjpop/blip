{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module Monad (Compile (..), runCompileMonad)  where

import Types (CompileState (..), BlockState (..))
import Control.Monad.State.Strict as State hiding (State)
import Control.Monad.State.Class (MonadState (..))
import Control.Applicative (Applicative (..))

newtype Compile a
   = Compile (StateT CompileState IO a)
   deriving (Monad, Functor, MonadIO, Applicative)

instance MonadState CompileState Compile where
   get = Compile get
   put s = Compile $ put s

runCompileMonad :: Compile a -> CompileState -> IO a
runCompileMonad (Compile comp) = evalStateT comp

{-
setBlockState :: BlockState -> Compile ()
setBlockState blockState = do
   oldState <- get
   put $ oldState { state_blockState = blockState }

getBlockState :: Compile BlockState
getBlockState = gets state_blockState
-}
