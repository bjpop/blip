-----------------------------------------------------------------------------
-- |
-- Module      : Blip.Interpreter.Builtins.StandardObjectID 
-- Copyright   : (c) 2012, 2013, 2014 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- ObjectIDs for known builtin objects. These are constant and allow us
-- to refer to various standard objects directly. 
--
-----------------------------------------------------------------------------

module Blip.Interpreter.StandardObjectID
   ( noneObjectID, noneTypeID, firstFreeID, intTypeID, floatTypeID
   , objectTypeID, typeTypeID, strTypeID, listTypeID, tupleTypeID 
   , dictTypeID, funTypeID, boolTypeID, complexTypeID
   , bytesTypeID, ellipsisTypeID, codeTypeID, primTypeID, frameTypeID
   , methodTypeID )
   where

import Blip.Interpreter.Types (ObjectID)

data StandardObjectID
   = NoneObjectID         -- the None value
   | NoneTypeID           -- the type of None
   | ObjectTypeID         -- the object class
   | TypeTypeID           -- the type class
   | IntTypeID
   | FloatTypeID
   | StrTypeID
   | ListTypeID
   | TupleTypeID
   | DictTypeID
   | FunctionTypeID
   | BoolTypeID
   | ComplexTypeID
   | BytesTypeID
   | EllipsisTypeID
   | CodeTypeID  
   | PrimTypeID
   | FrameTypeID
   | MethodTypeID
   | FirstFreeID          -- this should always be last in the list
   deriving (Eq, Ord, Show, Enum)

toObjectID :: StandardObjectID -> ObjectID
toObjectID = fromIntegral . fromEnum

firstFreeID :: ObjectID
firstFreeID = toObjectID FirstFreeID

noneObjectID :: ObjectID
noneObjectID = toObjectID NoneObjectID

noneTypeID :: ObjectID
noneTypeID = toObjectID NoneTypeID 

intTypeID :: ObjectID
intTypeID = toObjectID IntTypeID

floatTypeID :: ObjectID
floatTypeID = toObjectID FloatTypeID

objectTypeID :: ObjectID
objectTypeID = toObjectID ObjectTypeID

typeTypeID :: ObjectID
typeTypeID = toObjectID TypeTypeID

strTypeID :: ObjectID 
strTypeID = toObjectID StrTypeID

listTypeID :: ObjectID
listTypeID = toObjectID ListTypeID

tupleTypeID :: ObjectID
tupleTypeID = toObjectID TupleTypeID

dictTypeID :: ObjectID
dictTypeID = toObjectID DictTypeID

funTypeID :: ObjectID
funTypeID = toObjectID FunctionTypeID

boolTypeID :: ObjectID
boolTypeID = toObjectID BoolTypeID

complexTypeID :: ObjectID
complexTypeID = toObjectID ComplexTypeID

bytesTypeID :: ObjectID
bytesTypeID = toObjectID BytesTypeID  

ellipsisTypeID :: ObjectID
ellipsisTypeID = toObjectID EllipsisTypeID

codeTypeID :: ObjectID
codeTypeID = toObjectID CodeTypeID  

primTypeID :: ObjectID
primTypeID = toObjectID PrimTypeID  

frameTypeID :: ObjectID
frameTypeID = toObjectID FrameTypeID  

methodTypeID :: ObjectID
methodTypeID = toObjectID MethodTypeID  
