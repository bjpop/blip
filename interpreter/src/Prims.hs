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

import Data.ByteString.Lazy.Char8 as BS (unpack)
import Data.Vector as Vector (mapM, toList)
import Data.List (intersperse)
import Control.Monad.Trans (liftIO)
import Text.Printf (printf)
import Types
   ( ObjectID, Heap, HeapObject (..), ProgramCounter, ValueStack
   , EvalState (..), Eval (..), PrimFun )
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
   let primObject = PrimitiveObject arity name fun
   objectID <- allocateHeapObject primObject 
   setGlobal name objectID 
   
printPrim :: PrimFun
printPrim [x] = do
   object <- lookupHeap x
   objectString <- toString object
   liftIO $ putStrLn objectString
   returnNone
   where
   toString :: HeapObject -> Eval String
   toString (CodeObject {}) = return $ printf "<code %d>" x
   toString (StringObject {..}) = return $ BS.unpack stringObject_string
   toString (TupleObject {..}) = return $ "<tuple>"
   toString (IntObject {..}) = return $ show initObject_value
   toString (FloatObject {..}) = return $ show floatObject_value 
   toString NoneObject = return "None"
   toString EllipsisObject = return "..."
   toString (UnicodeObject {..}) = return $ unicodeObject_value
   toString TrueObject = return $ "True"
   toString FalseObject = return $ "False"
   toString (ComplexObject {..}) =
      return $ printf "%f + %fj" complexObject_real complexObject_imaginary
   toString (LongObject {..}) =
      return $ show longObject_value
   toString (PrimitiveObject {..}) =
      return $ printf "<prim %s>" primitiveName
   toString (ListObject {..}) = do
      elementObjects <- Vector.mapM lookupHeap listObject_elements
      elementStrings <- Vector.mapM toString elementObjects
      let elementStringsList = Vector.toList elementStrings
      return $ "[" ++ concat (intersperse ", " elementStringsList) ++ "]"
printPrim _other = error "print called with wrong number of arguments"
