{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Prims 
-- Copyright   : (c) 2012, 2013 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Primitive functions
--
-----------------------------------------------------------------------------

module Prims
   ( addPrimGlobal
   , printPrim
   , returnNone
   )  where

import Control.Monad.Trans (liftIO)
import Text.Printf (printf)
import Types
   ( ObjectID, Heap, HeapObject (..), ProgramCounter, Stack
   , StackObject (..), EvalState (..), Eval (..), PrimFun )
import State
   ( lookupHeap
   , getNextObjectID
   , insertHeap
   , setGlobal
   , returnNone
   , allocateHeapObject
   )

addPrimGlobal :: Int -> String -> PrimFun -> Eval ()
addPrimGlobal arity name fun = do
   let primObject = Primitive arity name fun
   objectID <- allocateHeapObject primObject 
   setGlobal name objectID 
   
printPrim :: PrimFun
printPrim [x] = do
   object <- lookupHeap x
   liftIO $ putStrLn $ toString object
   returnNone
   where
   toString :: HeapObject -> String
   toString (CodeObject {}) = printf "<code %d>" x
   toString (StringObject {..}) = show stringObject_string
   toString (TupleObject {..}) = "<tuple>"
   toString (IntObject {..}) = show initObject_value
   toString (FloatObject {..}) = show floatObject_value 
   toString NoneObject = "None"
   toString EllipsisObject = "..."
   toString (UnicodeObject {..}) = "'" ++ unicodeObject_value ++ "'"
   toString TrueObject = "True"
   toString FalseObject = "False"
   toString (ComplexObject {..}) = printf "%f + %fj" complexObject_real complexObject_imaginary
   toString (LongObject {..}) = show longObject_value
   toString (Primitive {..}) = printf "<prim %s>" primitiveName
printPrim _other = error "print called with wrong number of arguments"
