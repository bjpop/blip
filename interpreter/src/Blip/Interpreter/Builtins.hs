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
   (initBuiltins)
   where

import Control.Monad.Trans (liftIO)
import Blip.Interpreter.State
   (insertHeap, allocateHeapObjectPush, allocateHeapObject,
    setGlobal, lookupHeap, returnNone)
import Blip.Interpreter.StandardObjectID (noneObjectID)
import Blip.Interpreter.Types (HeapObject (..), Eval, PrimFun)
import Blip.Interpreter.Prims (heapObjectToString)

-- This should really build a module
initBuiltins :: Eval ()
initBuiltins = do
    noneObject
    addPrimBuiltin 1 "print" printPrim
    addPrimBuiltin 1 "id" idPrim
    
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
