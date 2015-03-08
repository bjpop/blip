{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Prims 
-- Copyright   : (c) 2012, 2013, 2014 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Primitive functions
--
-----------------------------------------------------------------------------

module Blip.Interpreter.Prims
   ( printIfNotNone
   , heapObjectToString
   )  where

import Data.ByteString.Lazy.Char8 as BS (unpack)
import Data.Vector as Vector (freeze, mapM, toList)
import Data.List (intersperse)
import Control.Monad.Trans (liftIO)
import Text.Printf (printf)
import Blip.Interpreter.Types
   ( ObjectID, HeapObject (..), Eval (..) )
import Blip.Interpreter.State
   ( lookupHeap )
import Blip.Interpreter.HashTable.Basic as HT (foldM)

printIfNotNone :: ObjectID -> Eval () 
printIfNotNone x = do
   object <- lookupHeap x
   case object of
      NoneObject -> return ()
      _other -> do
         objectString <- heapObjectToString object
         liftIO $ putStrLn objectString
   
heapObjectToString :: HeapObject -> Eval String
heapObjectToString (CodeObject {}) = return $ printf "<code>"
heapObjectToString (StringObject {..}) = return $ BS.unpack stringObject_string
-- heapObjectToString (TupleObject {..}) = return $ "<tuple>"
heapObjectToString (TupleObject {..}) = do
   let vector = tupleObject_elements 
   elementObjects <- Vector.mapM lookupHeap vector 
   elementStrings <- Vector.mapM heapObjectToString elementObjects
   let elementStringsList = Vector.toList elementStrings
   let commaList =
          if length elementStringsList == 1
             then zipWith (++) elementStringsList (repeat ", ") 
             else intersperse ", " elementStringsList
   return $ printf "(%s)" (concat commaList)
heapObjectToString (IntObject {..}) = return $ show initObject_value
heapObjectToString (FloatObject {..}) = return $ show floatObject_value 
heapObjectToString NoneObject = return "None"
heapObjectToString EllipsisObject = return "..."
heapObjectToString (UnicodeObject {..}) = return $ unicodeObject_value
heapObjectToString TrueObject = return $ "True"
heapObjectToString FalseObject = return $ "False"
heapObjectToString (ComplexObject {..}) =
   return $ printf "%f + %fj" complexObject_real complexObject_imaginary
heapObjectToString (LongObject {..}) =
   return $ show longObject_value
heapObjectToString (PrimitiveObject {..}) =
   return $ printf "<prim %s>" primitiveName
heapObjectToString (ListObject {..}) = do
   vector <- liftIO $ Vector.freeze listObject_elements
   elementObjects <- Vector.mapM lookupHeap vector 
   elementStrings <- Vector.mapM heapObjectToString elementObjects
   let elementStringsList = Vector.toList elementStrings
   return $ "[" ++ concat (intersperse ", " elementStringsList) ++ "]"
heapObjectToString (FrameObject {}) = return $ printf "<frame>"
heapObjectToString (DictObject {..}) = do
   elementStrings <- HT.foldM prettyKeyVal [] dictHashTable 
   return $ printf "{%s}" (concat $ intersperse ", " elementStrings)
   where
   prettyKeyVal :: [String] -> (ObjectID, ObjectID) -> Eval [String] 
   prettyKeyVal strs (objectID1, objectID2) = do
      object1 <- lookupHeap objectID1
      object2 <- lookupHeap objectID2
      str1 <- heapObjectToString object1
      str2 <- heapObjectToString object2
      return ((str1 ++ ": " ++ str2) : strs)
heapObjectToString (FunctionObject {..}) = do
   nameObject <- lookupHeap functionName
   case nameObject of
      UnicodeObject str -> return $ printf "<function %s>" str
      other -> error $ "function name not a unicode object: " ++ show other
heapObjectToString (TypeObject {..}) = do
   nameObject <- lookupHeap typeName
   case nameObject of
       UnicodeObject {..} -> 
          return $ printf "<class '%s'>" unicodeObject_value
       other -> error $ "type name is not a string: " ++ show other
heapObjectToString (MethodObject {}) = return "<method>" 
