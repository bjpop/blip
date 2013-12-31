{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Interpret 
-- Copyright   : (c) 2013 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Simple reference byte code interpreter for Python 3. 
--
-----------------------------------------------------------------------------

module Interpret (interpretFile) where

import Text.Printf (printf)
import Data.List as List (length)
import Control.Monad (replicateM)
import Control.Monad.Trans (liftIO)
import Control.Applicative ((<$>))
import System.IO (withFile, IOMode (..))
import Blip.Marshal (readPyc, PycFile (..), PyObject (..))
import Blip.Bytecode
   ( Bytecode (..), Opcode (..), decode, word8ToOpcode, hasArg
   , word8sToWord16 )
import qualified Data.Map as Map (insert, lookup)
import Data.Map (Map)
import qualified Data.ByteString.Lazy as B (ByteString, index, length)
import Data.Word (Word8, Word16, Word32)
import Data.Vector as Vector (Vector, fromList, length, (!))
import Types 
   ( Eval (..), EvalState (..), StackObject ) 
import State 
   ( runEvalMonad, getNextObjectID, insertHeap
   , lookupHeap, initState, getProgramCounter, incProgramCounter
   , pushStack, popStack, getStack, getGlobal
   , allocateHeapObject ) 
import Types (ObjectID, Heap, HeapObject (..))
import Prims (printPrim, addPrimGlobal)

interpretFile :: FilePath -> IO ()
interpretFile pycFilename = do
   withFile pycFilename ReadMode $ \handle -> do
      pycFile <- readPyc handle 
      runTopObject $ object pycFile

runTopObject :: PyObject -> IO ()
runTopObject object =
   runEvalMonad
      (initGlobals >> runTopObjectEval object)
      initState 

initGlobals :: Eval ()
initGlobals = do
   addPrimGlobal 1 "print" printPrim

runTopObjectEval :: PyObject -> Eval ()
runTopObjectEval object = do
   objectID <- loadIntoHeap object
   evalTopObject objectID

evalTopObject :: ObjectID -> Eval ()
evalTopObject objectID = do
   heapObject <- lookupHeap objectID
   case heapObject of
      CodeObject {} -> evalCodeObject heapObject
      _other -> return ()

evalCodeObject :: HeapObject -> Eval ()
evalCodeObject object@(CodeObject {..}) = do
   pc <- getProgramCounter
   stack <- getStack
   -- liftIO $ printf "Program counter: %d\n" pc
   -- liftIO $ printf "Stack: %s\n" (show stack)
   bytecodeObject <- lookupHeap codeObject_code
   let code = 
          case bytecodeObject of
             StringObject {..} -> stringObject_string
             _other -> error "Bytecode not a string"
   let numInstructions = B.length code 
   -- check the program counter is in valid range
   if pc < 0 || pc >= numInstructions
      then return ()
      else do
         let nextOpCodeWord8 = B.index code pc
         case Map.lookup nextOpCodeWord8 word8ToOpcode of
            Nothing -> error ("bad op code: " ++ show nextOpCodeWord8)
            Just opCode -> do
               if hasArg opCode
                  then do
                     let arg1 = B.index code (pc + 1)
                         arg2 = B.index code (pc + 2)
                         argWord16 = word8sToWord16 arg1 arg2
                     -- liftIO $ printf "%s %d\n" (show opCode) argWord16
                     incProgramCounter 3
                     evalOpCode object opCode argWord16
                  else do
                     -- liftIO $ printf "%s\n" (show opCode)
                     incProgramCounter 1 
                     -- pass a dummy arg
                     evalOpCode object opCode 0
               -- evalCodeObject object
evalCodeObject other = error ("try to eval non code object: " ++ show other)

-- Some opcodes don't use the arg, but we pass it anyway (a dummy arg) to simplify
-- the program
evalOpCode :: HeapObject -> Opcode -> Word16 -> Eval ()
evalOpCode codeObject@(CodeObject {..}) LOAD_NAME arg = do
   namesTupleObject <- lookupHeap codeObject_names 
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
                  UnicodeObject {..} -> do -- liftIO $ printf "name: %s\n" unicodeObject_value 
                     globalID <- getGlobal unicodeObject_value
                     pushStack globalID
                  other -> error $ "name does not point to a unicode object: " ++ show other
               evalCodeObject codeObject
      other -> error $ "names tuple not a tuple: " ++ show other
evalOpCode codeObject@(CodeObject {..}) LOAD_CONST arg = do
   constsTupleObject <- lookupHeap codeObject_consts 
   case constsTupleObject of
      TupleObject {..} -> do
         let tupleSize = Vector.length tupleObject_elements 
             arg64 = fromIntegral arg
         if arg64 < 0 || arg64 >= tupleSize 
            then error $ "index into const tuple out of bounds"
            else do
               let constObjectID = tupleObject_elements ! arg64
               pushStack constObjectID 
               evalCodeObject codeObject
      other -> error $ "names tuple not a tuple: " ++ show other
evalOpCode codeObject BINARY_MULTIPLY arg = do
   stackArg1 <- popStack
   stackArg2 <- popStack
   stackObject1 <- lookupHeap stackArg1
   staclObject2 <- lookupHeap stackArg2
   multiply stackObject1 staclObject2
   evalCodeObject codeObject
evalOpCode codeObject CALL_FUNCTION arg = do
   functionArgs <- replicateM (fromIntegral arg) popStack
   functionObjectID <- popStack
   functionObject <- lookupHeap functionObjectID
   callFunction functionObject functionArgs 
   evalCodeObject codeObject
evalOpCode codeObject POP_TOP arg = popStack >> evalCodeObject codeObject 
evalOpCode codeObject otherOpCode arg = do
    evalCodeObject codeObject

callFunction :: HeapObject -> [ObjectID] -> Eval ()
callFunction (Primitive arity name fun) args
   | arity == List.length args = fun args
   | otherwise = error (printf "primitve of arity %d applied to %d arguments"
                               arity (List.length args))

multiply :: HeapObject -> HeapObject -> Eval ()
multiply (IntObject int1) (IntObject int2) = do
   let resultObject = IntObject (int1 * int2)
   objectID <- allocateHeapObject resultObject
   pushStack objectID
multiply (FloatObject float1) (FloatObject float2) = do
   let resultObject = FloatObject (float1 * float2)
   objectID <- allocateHeapObject resultObject
   pushStack objectID
-- XXX FIXME
multiply _other1 _other2 = error $ "Multiply on non ints or floats"

-- Load a PyObject from a pyc file into a heap, turning
-- each nested PyObject into its corresponding Object.
-- Returns the ID of the topmost inserted object
loadIntoHeap :: PyObject -> Eval ObjectID 
loadIntoHeap object = do
   heapObject <- case object of 
      Code {..} -> do
         codeID <- loadIntoHeap code
         constsID <- loadIntoHeap consts
         namesID <- loadIntoHeap names
         varnamesID <- loadIntoHeap varnames
         freevarsID <- loadIntoHeap freevars
         cellvarsID <- loadIntoHeap cellvars
         filenameID <- loadIntoHeap filename
         nameID <- loadIntoHeap name
         lnotabID <- loadIntoHeap lnotab
         return $
            CodeObject
            { codeObject_argcount = argcount
            , codeObject_kwonlyargcount = kwonlyargcount
            , codeObject_nlocals = nlocals
            , codeObject_stacksize = stacksize
            , codeObject_flags = flags
            , codeObject_code = codeID
            , codeObject_consts = constsID
            , codeObject_names = namesID
            , codeObject_varnames = varnamesID
            , codeObject_freevars = freevarsID
            , codeObject_cellvars = cellvarsID
            , codeObject_filename = filenameID
            , codeObject_name = nameID
            , codeObject_firstlineno = firstlineno
            , codeObject_lnotab = lnotabID
            }
      Tuple {..} -> do
         elementIDs <- mapM loadIntoHeap elements 
         return $ TupleObject $ fromList elementIDs
      atomicObject ->
         return $ atomicPyObjectToHeapObject atomicObject
   allocateHeapObject heapObject

atomicPyObjectToHeapObject :: PyObject -> HeapObject
atomicPyObjectToHeapObject (Long val) = LongObject val
atomicPyObjectToHeapObject (Complex {..})
   = ComplexObject { complexObject_real = real, complexObject_imaginary = imaginary }
atomicPyObjectToHeapObject TrueObj = TrueObject
atomicPyObjectToHeapObject FalseObj = FalseObject
atomicPyObjectToHeapObject (Unicode val) = UnicodeObject val
atomicPyObjectToHeapObject Ellipsis = EllipsisObject
atomicPyObjectToHeapObject None = NoneObject
atomicPyObjectToHeapObject (Float val) = FloatObject val
atomicPyObjectToHeapObject (Int val) = IntObject $ fromIntegral val
atomicPyObjectToHeapObject (String val) = StringObject val
atomicPyObjectToHeapObject other
   = error ("atomicPyObjectToHeapObject applied to non-atomic object: " ++ show other)
