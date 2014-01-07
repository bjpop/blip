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
   , pushFrame
   , popFrame
   , getValueStack
   , pushValueStack
   , popValueStack
   , popValueStackObject
   , peekValueStackFromBottom
   , peekValueStackFromBottomObject
   , peekValueStackFromTop
   , peekValueStackFromTopObject
   , returnNone
   , lookupName
   , lookupConst
   )  where

import Data.Word (Word16)
import Data.Vector as Vector (length, (!))
import Data.Vector.Generic.Mutable as MVector (write, read, length)
import qualified Data.Map as Map (insert, lookup, empty)
import Data.Map (Map)
import Control.Monad.State.Strict as State hiding (State)
import Control.Applicative (Applicative (..), (<$>))
import Types
   ( ObjectID, Heap, HeapObject (..), ProgramCounter, ValueStack
   , EvalState (..), Eval (..) )
import Text.Printf (printf)

initState =
   EvalState
   { evalState_objectID = 0
   , evalState_heap = Map.empty
   , evalState_frameStack = []
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
   pushValueStack objectID

allocateHeapObject :: HeapObject -> Eval ObjectID
allocateHeapObject object = do
   objectID <- getNextObjectID
   insertHeap objectID object
   return objectID

allocateHeapObjectPush :: HeapObject -> Eval ()
allocateHeapObjectPush object =
   allocateHeapObject object >>= pushValueStack

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

getFrame :: Eval HeapObject
getFrame = do
   frameStack <- gets evalState_frameStack
   case frameStack of
      [] -> error $ "attempt to get frame from empty stack"
      heapObject:_rest -> do
         case heapObject of
            FrameObject {} -> return heapObject
            _other -> error $ "top of frame stack not a frame object"

modifyFrame :: (HeapObject -> HeapObject) -> Eval ()
modifyFrame modifier = do
   frameStack <- gets evalState_frameStack
   case frameStack of
      [] -> error $ "attempt to get frame from empty stack"
      oldFrame:rest ->
         modify $ \state -> state { evalState_frameStack = modifier oldFrame:rest }

-- does not increment the counter
getProgramCounter :: Eval ProgramCounter 
getProgramCounter = frameProgramCounter <$> getFrame

setProgramCounter :: ProgramCounter -> Eval () 
setProgramCounter pc =
   modifyFrame $ \frame ->
      let oldPC = frameProgramCounter frame
      in frame { frameProgramCounter = oldPC + 1 }

incProgramCounter :: ProgramCounter -> Eval ()
incProgramCounter n = 
   modifyFrame $ \frame ->
      let oldPC = frameProgramCounter frame
      in frame { frameProgramCounter = oldPC + n }

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

popFrame :: Eval ()
popFrame = do
   frameStack <- gets evalState_frameStack
   case frameStack of
      [] -> error "pop empty frame stack"
      _top:rest -> modify $ \state -> state { evalState_frameStack = rest }

pushFrame :: HeapObject -> Eval ()
pushFrame frameObject =
   modify $ \state -> 
      let frameStack = evalState_frameStack state
          newFrameStack = frameObject : frameStack
      in state { evalState_frameStack = newFrameStack }

getValueStack :: Eval ValueStack
getValueStack = frameValueStack <$> getFrame

mywrite vector position value = do
   let vectorSize = MVector.length vector
   liftIO $ printf "Vector size: %d\n" vectorSize
   liftIO $ printf "position: %d\n" position
   liftIO $ MVector.write vector position value

-- stack pointer points to the item on top of the stack
pushValueStack :: ObjectID -> Eval ()
pushValueStack objectID = do
   oldFrame <- getFrame
   let oldStackPointer = frameStackPointer oldFrame
       newStackPointer = oldStackPointer + 1
       valueStack = frameValueStack oldFrame
   -- XXX this may fail if the stack pointer is out of range
   liftIO $ MVector.write valueStack newStackPointer objectID
   -- mywrite valueStack newStackPointer objectID
   let newFrame = oldFrame { frameStackPointer = newStackPointer }
   modifyFrame $ \_ignore -> newFrame 

popValueStack :: Eval ObjectID
popValueStack = do
   oldFrame <- getFrame
   let oldStackPointer = frameStackPointer oldFrame
       newStackPointer = oldStackPointer - 1
       valueStack = frameValueStack oldFrame
   -- XXX this may fail if the stack pointer is out of range
   objectID <- liftIO $ MVector.read valueStack oldStackPointer
   let newFrame = oldFrame { frameStackPointer = newStackPointer }
   modifyFrame $ \_ignore -> newFrame 
   return objectID

popValueStackObject :: Eval HeapObject
popValueStackObject = popValueStack >>= lookupHeap

peekValueStackFromTop :: Int -> Eval ObjectID
peekValueStackFromTop offset = do
   frame <- getFrame
   let stackPointer = frameStackPointer frame 
       valueStack = frameValueStack frame 
   -- XXX this may fail if the stack pointer is out of range
   objectID <- liftIO $ MVector.read valueStack (stackPointer - offset) 
   return objectID

peekValueStackFromTopObject :: Int -> Eval HeapObject
peekValueStackFromTopObject offset =
   peekValueStackFromTop offset >>= lookupHeap

peekValueStackFromBottom :: Int -> Eval ObjectID
peekValueStackFromBottom offset = do
   frame <- getFrame
   let valueStack = frameValueStack frame 
   -- XXX this may fail if the stack pointer is out of range
   objectID <- liftIO $ MVector.read valueStack offset 
   return objectID

peekValueStackFromBottomObject :: Int -> Eval HeapObject
peekValueStackFromBottomObject offset =
   peekValueStackFromBottom offset >>= lookupHeap

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
