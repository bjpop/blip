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
   (initBuiltins, hashObject, eqObject, newHashTable, newHashTableSized)
   where

import Control.Monad (foldM)
import Control.Monad.Trans (liftIO)
import Blip.Interpreter.State
   (insertHeap, allocateHeapObjectPush, allocateHeapObject,
    setGlobal, lookupHeap, returnNone, pushValueStack)
import Blip.Interpreter.StandardObjectID
   ( noneObjectID, noneTypeID, intTypeID, floatTypeID
   , objectTypeID, typeTypeID, strTypeID, listTypeID, tupleTypeID
   , dictTypeID, funTypeID, boolTypeID, complexTypeID, funTypeID
   , bytesTypeID, ellipsisTypeID, codeTypeID, primTypeID, frameTypeID )
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

typePrim :: PrimFun
typePrim [objectID] = do
   object <- lookupHeap objectID
   case object of
      NoneObject {} -> pushValueStack noneTypeID
      IntObject {} -> pushValueStack intTypeID   
      FloatObject {} -> pushValueStack floatTypeID   
      StringObject {} -> pushValueStack bytesTypeID   
      UnicodeObject {} -> pushValueStack strTypeID
      TupleObject {} -> pushValueStack tupleTypeID
      EllipsisObject {} -> pushValueStack ellipsisTypeID
      TrueObject {} -> pushValueStack boolTypeID
      FalseObject {} -> pushValueStack boolTypeID
      ComplexObject {} -> pushValueStack complexTypeID
      ListObject {} -> pushValueStack listTypeID
      DictObject {} -> pushValueStack dictTypeID 
      TypeObject {} -> pushValueStack typeTypeID
      CodeObject {} -> pushValueStack codeTypeID
      PrimitiveObject {} -> pushValueStack primTypeID
      FrameObject {} -> pushValueStack frameTypeID
      FunctionObject {} -> pushValueStack funTypeID
      LongObject {} -> pushValueStack intTypeID
typePrim _other = error "type called with wrong number of arguments"

intType :: Eval ()
intType = makeType "int" intTypeID []

floatType :: Eval ()
floatType = makeType "float" floatTypeID []

bytesType :: Eval ()
bytesType = makeType "bytes" bytesTypeID []

strType :: Eval ()
strType = makeType "str" strTypeID []

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

-- XXX fixme
hashObject :: ObjectID -> Eval Int
hashObject _objectID = return 12

-- XXX fixme
eqObject :: ObjectID -> ObjectID -> Eval Bool
eqObject objectID1 objectID2 = do
   object1 <- lookupHeap objectID1
   object2 <- lookupHeap objectID2
   case (object1, object2) of
      (IntObject int1, IntObject int2) ->
          return $ int1 == int2
      _other -> return False

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
