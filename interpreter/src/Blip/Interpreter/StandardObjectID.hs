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
   (noneObjectID, noneTypeID, firstFreeID)
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
   | CodeTypeID  
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
