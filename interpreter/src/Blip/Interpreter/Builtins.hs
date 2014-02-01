{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Blip.Interpreter.Builtins.Builtins
-- Copyright   : (c) 2012, 2013, 2014 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Standard objects and types
--
-----------------------------------------------------------------------------

module Blip.Interpreter.Builtins
   (initBuiltins, hashObject, eqObject, newHashTable,
   newHashTableSized, typeOf)
   where

import Data.Hashable (hash)
import Data.Char (toUpper)
import Control.Monad (foldM)
import Control.Monad.Trans (liftIO)
import Blip.Interpreter.State
   (insertHeap, allocateHeapObjectPush, allocateHeapObject,
    setGlobal, lookupHeap, returnNone, pushValueStack,
    allocateHeapObjectPush)
import Blip.Interpreter.StandardObjectID
   (noneObjectID, noneTypeID, intTypeID, floatTypeID,
   objectTypeID, typeTypeID, strTypeID, listTypeID, tupleTypeID,
   dictTypeID, funTypeID, boolTypeID, complexTypeID, funTypeID,
   bytesTypeID, ellipsisTypeID, codeTypeID, primTypeID, frameTypeID,
   methodTypeID)
import Blip.Interpreter.Types
   (HeapObject (..), Eval, PrimFun, ObjectID, HashTable)
import Blip.Interpreter.Prims (heapObjectToString)
import Blip.Interpreter.HashTable.Basic as HT (new, newSized, insert)

-- This should really build a module
initBuiltins :: Eval ()
initBuiltins = do
    noneObject
    noneType
    intType
    floatType
    bytesType
    strType
    tupleType
    ellipsisType
    boolType
    complexType
    listType 
    dictType
    typeType
    codeType
    primType
    frameType
    funType
    objectType
    methodType
    addPrimBuiltin 1 "print" printPrim
    addPrimBuiltin 1 "id" idPrim
    addPrimBuiltin 1 "type" typePrim
    
-- These objects must appear at known ObjectIDs
-- Their IDs are defined in StandardObjectID

noneObject :: Eval ()
noneObject = insertHeap noneObjectID NoneObject  

-- These objects may appear at any ObjectID

-- XXX this should really add to the module not to the global scope
addPrimBuiltin :: Int -> String -> PrimFun -> Eval ()
addPrimBuiltin arity name fun = do
   let primObject = PrimitiveObject arity name fun
   objectID <- allocateHeapObject primObject
   setGlobal name objectID

printPrim :: PrimFun
printPrim [x] = do
   object <- lookupHeap x
   objectString <- heapObjectToString object
   liftIO $ putStrLn objectString
   returnNone
printPrim _other = error "print called with wrong number of arguments"

idPrim :: PrimFun
idPrim [x] = allocateHeapObjectPush $ IntObject $ fromIntegral x
idPrim _other = error "id called with wrong number of arguments"

typeOf :: HeapObject -> Eval ObjectID
typeOf object =
   case object of
      NoneObject {} -> return noneTypeID
      IntObject {} -> return intTypeID   
      FloatObject {} -> return floatTypeID   
      StringObject {} -> return bytesTypeID   
      UnicodeObject {} -> return strTypeID
      TupleObject {} -> return tupleTypeID
      EllipsisObject {} -> return ellipsisTypeID
      TrueObject {} -> return boolTypeID
      FalseObject {} -> return boolTypeID
      ComplexObject {} -> return complexTypeID
      ListObject {} -> return listTypeID
      DictObject {} -> return dictTypeID 
      TypeObject {} -> return typeTypeID
      CodeObject {} -> return codeTypeID
      PrimitiveObject {} -> return primTypeID
      FrameObject {} -> return frameTypeID
      FunctionObject {} -> return funTypeID
      LongObject {} -> return intTypeID
      MethodObject {} -> return methodTypeID

typePrim :: PrimFun
typePrim [objectID] = do
   object <- lookupHeap objectID
   typeID <- typeOf object
   pushValueStack typeID
typePrim _other = error "type called with wrong number of arguments"

intType :: Eval ()
intType = makeType "int" intTypeID []

floatType :: Eval ()
floatType = makeType "float" floatTypeID []

bytesType :: Eval ()
bytesType = makeType "bytes" bytesTypeID []

strType :: Eval ()
strType =
   makeType "str" strTypeID strMethods 
   where
   strMethods :: [HeapObject]
   strMethods = [upperMethod]
   upperMethod :: HeapObject
   upperMethod = PrimitiveObject 1 "upper" upperPrim

upperPrim :: PrimFun
upperPrim [objectID] = do
   object <- lookupHeap objectID
   case object of
      UnicodeObject string -> do
          allocateHeapObjectPush $ UnicodeObject $ map toUpper string
      _other -> error $ "upper applied to a non string object"
upperPrim _other = error $ "wrong number of arguments to upper method"

tupleType :: Eval ()
tupleType = makeType "tuple" tupleTypeID []

noneType :: Eval ()
noneType = makeType "NoneType" noneTypeID []

ellipsisType :: Eval ()
ellipsisType = makeType "ellipsis" ellipsisTypeID []

boolType :: Eval ()
boolType = makeType "bool" boolTypeID []

complexType :: Eval ()
complexType = makeType "complex" complexTypeID []

listType :: Eval ()
listType = makeType "list" listTypeID []

dictType :: Eval ()
dictType = makeType "dict" dictTypeID []

typeType :: Eval ()
typeType = makeType "type" typeTypeID []

codeType :: Eval ()
codeType = makeType "code" codeTypeID []

primType :: Eval ()
primType = makeType "builtin_function_or_method" primTypeID []

frameType :: Eval ()
frameType = makeType "frame" frameTypeID []

funType :: Eval ()
funType = makeType "fun" funTypeID []

objectType :: Eval ()
objectType = makeType "object" objectTypeID []

methodType :: Eval ()
methodType = makeType "method" methodTypeID []

hashObject :: ObjectID -> Eval Int
hashObject objectID = do
   object <- lookupHeap objectID 
   case object of
      IntObject int -> return $ hash int
      StringObject bs -> return $ hash bs
      UnicodeObject str -> return $ hash str
      FloatObject float -> return $ hash float
      -- XXX should really call the __hash__ method
      _other -> return $ hash objectID 

eqObject :: ObjectID -> ObjectID -> Eval Bool
eqObject objectID1 objectID2 = do
   object1 <- lookupHeap objectID1
   object2 <- lookupHeap objectID2
   case (object1, object2) of
      (IntObject int1, IntObject int2) ->
          return $ int1 == int2
      (UnicodeObject str1, UnicodeObject str2) ->
          return $ str1 == str2
      (FloatObject float1, FloatObject float2) ->
          return $ float1 == float2
      (StringObject str1, StringObject str2) ->
          return $ str1 == str2
      -- XXX should really call the __eq__ method
      _other -> return $ objectID1 == objectID2 

makeMethods :: [HeapObject] -> Eval HeapObject
makeMethods methods = do
   hashTable <- newHashTableSized $ length methods
   _ <- foldM addMethod hashTable methods
   return $ DictObject hashTable
   where
   addMethod :: HashTable -> HeapObject -> Eval HashTable
   addMethod hashTable prim@(PrimitiveObject {..}) = do
      nameObjectID <- allocateHeapObject $ UnicodeObject primitiveName
      primObjectID <- allocateHeapObject prim
      HT.insert hashTable nameObjectID primObjectID
      return hashTable
   addMethod _hashTable otherObject =
      error $ "addMethod supplied non primitive object: " ++ show otherObject

makeType :: String -> ObjectID -> [HeapObject] -> Eval ()
makeType name objectID methods = do
   nameObjectID <- allocateHeapObject $ UnicodeObject name
   methodsDict <- makeMethods methods 
   dictObjectID <- allocateHeapObject methodsDict
   let typeObject =
          TypeObject
          { typeName = nameObjectID
          , typeAttributes = dictObjectID
          }
   insertHeap objectID typeObject 

newHashTableSized :: Int -> Eval HashTable
newHashTableSized size =
    liftIO $ HT.newSized size hashObject eqObject

newHashTable :: Eval HashTable
newHashTable =
   liftIO $ HT.new hashObject eqObject
