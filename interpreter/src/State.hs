{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      : State 
-- Copyright   : (c) 2012, 2013 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- State manipulation
--
-----------------------------------------------------------------------------

module State 
   ( runEvalMonad
   , initState
   , allocateHeapObject
   , allocateHeapObjectPush
   , getNextObjectID
   , insertHeap
   , lookupHeap
   , getProgramCounter
   , setProgramCounter
   , incProgramCounter
   , getGlobal
   , setGlobal
   , getStack
   , pushStack
   , popStackMaybe
   , popStack
   , popStackObject
   , peekStackMaybe
   , peekStack
   , peekStackObject
   , returnNone
   , lookupName
   , lookupConst
   )  where

import Data.Word (Word16)
import Data.Vector as Vector (length, (!))
import qualified Data.Map as Map (insert, lookup, empty)
import Data.Map (Map)
import Control.Monad.State.Strict as State hiding (State)
import Control.Applicative (Applicative (..))
import Types
   ( ObjectID, Heap, HeapObject (..), ProgramCounter, Stack
   , StackObject (..), EvalState (..), Eval (..) )

initState =
   EvalState
   { evalState_objectID = 0
   , evalState_heap = Map.empty
   , evalState_programCounter = 0
   , evalState_stack = []
   , evalState_globals = Map.empty
   }

runEvalMonad :: Eval a -> EvalState -> IO a
runEvalMonad (Eval comp) = evalStateT comp

-- XXX it would be nice if we could share the None object,
-- rather than allocate a new one each time. But this would
-- require us knowing where it was allocated.
returnNone :: Eval ()
returnNone = do
   objectID <- allocateHeapObject NoneObject
   pushStack objectID

allocateHeapObject :: HeapObject -> Eval ObjectID
allocateHeapObject object = do
   objectID <- getNextObjectID
   insertHeap objectID object
   return objectID

allocateHeapObjectPush :: HeapObject -> Eval ()
allocateHeapObjectPush object =
   allocateHeapObject object >>= pushStack

-- post increments the counter
getNextObjectID :: Eval ObjectID
getNextObjectID = do
   oldID <- gets evalState_objectID
   modify $ \state -> state { evalState_objectID = oldID + 1 }
   return oldID

insertHeap :: ObjectID -> HeapObject -> Eval ()
insertHeap objectID object = do
   oldState <- get
   let oldHeap = evalState_heap oldState
       newHeap = Map.insert objectID object oldHeap
   put $ oldState { evalState_heap = newHeap }

lookupHeapMaybe :: ObjectID -> Eval (Maybe HeapObject)
lookupHeapMaybe objectID = do
   heap <- gets evalState_heap
   return $ Map.lookup objectID heap

lookupHeap :: ObjectID -> Eval HeapObject
lookupHeap objectID = do
   maybeObject <- lookupHeapMaybe objectID
   case maybeObject of
      Nothing -> error $ "Failed to find object on heap: " ++ show objectID
      Just object -> return object

-- does not increment the counter
getProgramCounter :: Eval ProgramCounter 
getProgramCounter = gets evalState_programCounter

setProgramCounter :: ProgramCounter -> Eval () 
setProgramCounter pc =
   modify $ \state -> state { evalState_programCounter = pc } 

incProgramCounter :: ProgramCounter -> Eval ()
incProgramCounter n = 
   modify $ \state ->
      let oldPC = evalState_programCounter state
          newPC = oldPC + n
      in state { evalState_programCounter = newPC }

getGlobal :: String -> Eval ObjectID
getGlobal name = do
   globals <- gets evalState_globals
   case Map.lookup name globals of
      Nothing -> error $ "global name not found: " ++ name
      Just objectID -> return objectID

setGlobal :: String -> ObjectID -> Eval ()
setGlobal name objectID =
   modify $ \state ->
      let globals = evalState_globals state
          newGlobals = Map.insert name objectID globals
      in state { evalState_globals = newGlobals } 

getStack :: Eval Stack
getStack = gets evalState_stack

pushStack :: StackObject -> Eval ()
pushStack stackObject =
   modify $ \state ->
      let stack = evalState_stack state
      in state { evalState_stack = stackObject : stack }

popStackMaybe :: Eval (Maybe StackObject)
popStackMaybe = do
   stack <- gets evalState_stack
   case stack of
      [] -> return Nothing
      top:bottom -> do 
         modify $ \state -> state { evalState_stack = bottom }
         return $ Just top 

popStack :: Eval StackObject
popStack = do
   topMaybe <- popStackMaybe
   case topMaybe of
      Nothing -> error $ "attempt to pop empty stack"
      Just x -> return x

popStackObject :: Eval HeapObject
popStackObject = popStack >>= lookupHeap

peekStackMaybe :: Eval (Maybe StackObject)
peekStackMaybe = do
   stack <- gets evalState_stack
   case stack of
      [] -> return Nothing
      top:_bottom -> return $ Just top 

peekStack :: Eval StackObject
peekStack = do
   objectMaybe <- peekStackMaybe
   case objectMaybe of
      Nothing -> error $ "attempt to peek an empty stack"
      Just x -> return x

peekStackObject :: Eval HeapObject
peekStackObject = peekStack >>= lookupHeap

lookupName :: ObjectID -> Word16 -> Eval String
lookupName namesTupleID arg = do 
   namesTupleObject <- lookupHeap namesTupleID 
   case namesTupleObject of
      TupleObject {..} -> do
         let tupleSize = Vector.length tupleObject_elements
             arg64 = fromIntegral arg
         if arg64 < 0 || arg64 >= tupleSize
            then error $ "index into name tuple out of bounds"
            else do
               let unicodeObjectID = tupleObject_elements ! arg64
               unicodeObject <- lookupHeap unicodeObjectID
               case unicodeObject of
                  UnicodeObject {..} -> return unicodeObject_value
                  other -> error $ "name does not point to a unicode object: " ++ show other
      other -> error $ "names tuple not a tuple: " ++ show other

lookupConst :: ObjectID -> Word16 -> Eval ObjectID
lookupConst constsTupleID arg = do 
   constsTupleObject <- lookupHeap constsTupleID 
   case constsTupleObject of
      TupleObject {..} -> do
         let tupleSize = Vector.length tupleObject_elements
             arg64 = fromIntegral arg
         if arg64 < 0 || arg64 >= tupleSize
            then error $ "index into const tuple out of bounds"
            else return $ tupleObject_elements ! arg64
      other -> error $ "names tuple not a tuple: " ++ show other
