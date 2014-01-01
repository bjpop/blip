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

import Data.Fixed (mod')
import Text.Printf (printf)
import Data.List as List (length)
import Control.Monad (replicateM, when)
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
import Data.Int (Int32)
import Data.Vector as Vector (Vector, fromList, length, (!))
import Types 
   ( Eval (..), EvalState (..), StackObject ) 
import State 
   ( runEvalMonad, getNextObjectID, insertHeap
   , lookupHeap, initState, getProgramCounter, incProgramCounter
   , pushStack, popStack, getStack, getGlobal, setGlobal
   , allocateHeapObject, lookupName, setProgramCounter ) 
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

evalOpCode :: HeapObject -> Opcode -> Word16 -> Eval ()
evalOpCode codeObject opcode arg =
   evalOneOpCode codeObject opcode arg >> evalCodeObject codeObject

-- Some opcodes don't use the arg, but we pass it anyway (a dummy arg) to simplify
-- the program
evalOneOpCode :: HeapObject -> Opcode -> Word16 -> Eval ()
evalOneOpCode codeObject@(CodeObject {..}) opcode arg =
   case opcode of
      POP_TOP -> popStack >> return () 
      ROT_TWO -> do
         first <- popStack
         second <- popStack
         pushStack first 
         pushStack second 
      -- (first:second:third:rest) -> (second:third:first:rest)
      ROT_THREE -> do
         first <- popStack
         second <- popStack
         third <- popStack
         pushStack first 
         pushStack third
         pushStack second
      DUP_TOP -> do
         first <- popStack
         pushStack first
      DUP_TOP_TWO -> do
         first <- popStack
         second <- popStack
         pushStack second
         pushStack first
      NOP -> return ()
      BINARY_POWER -> binOpPower
      BINARY_MULTIPLY -> binOpMultiply
      BINARY_MODULO -> binOpModulo
      BINARY_ADD -> binOpAdd
      BINARY_SUBTRACT -> binOpSubtract
      BINARY_TRUE_DIVIDE -> binOpDivide
      -- XXX load name should really look in local scope, then enclosing, then global, then builtins
      LOAD_NAME -> do 
         nameString <- lookupName codeObject_names arg
         globalID <- getGlobal nameString 
         pushStack globalID
      STORE_NAME -> do
         nameString <- lookupName codeObject_names arg
         objectID <- popStack
         setGlobal nameString objectID
      LOAD_CONST -> do
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
            other -> error $ "names tuple not a tuple: " ++ show other
      CALL_FUNCTION -> do
         functionArgs <- replicateM (fromIntegral arg) popStack
         functionObjectID <- popStack
         functionObject <- lookupHeap functionObjectID
         callFunction functionObject functionArgs 
      JUMP_ABSOLUTE -> setProgramCounter $ fromIntegral arg 
      -- setup loop should push a block onto the block stack
      SETUP_LOOP -> return ()
      -- pop block should pop a block off the block stack
      POP_BLOCK -> return ()
      POP_JUMP_IF_FALSE -> do
         top <- popStack
         object <- lookupHeap top
         case object of
            FalseObject -> setProgramCounter $ fromIntegral arg
            TrueObject -> return ()
            _other -> error "pop jump if false encountered non bool on stack"
      COMPARE_OP ->
         case arg of
            0 -> binOpLT -- < 
            1 -> binOpLTE -- <= 
            2 -> binOpEQ -- ==
            3 -> binOpNEQ -- !=
            4 -> binOpGT 
            5 -> binOpGTE -- >=
            6 -> return () -- in
            7 -> return () -- not in
            8 -> return () -- is
            9 -> return () -- is not
      _otherOpCode -> return ()

callFunction :: HeapObject -> [ObjectID] -> Eval ()
callFunction (Primitive arity name fun) args
   | arity == List.length args = fun args
   | otherwise =
        error (printf "primitve of arity %d applied to %d arguments"
               arity (List.length args))

data BinOp =
   BinOp
   { binOpIntInt :: Int32 -> Int32 -> HeapObject 
   , binOpFloatFloat :: Double -> Double -> HeapObject
   }

binOpCompare :: (Int32 -> Int32 -> Bool) -> (Double -> Double -> Bool) -> Eval ()
binOpCompare intCompare floatCompare =
   binOp $ BinOp 
      (\x y -> if intCompare y x then TrueObject else FalseObject)
      (\x y -> if floatCompare y x then TrueObject else FalseObject)

binOpGT :: Eval () 
binOpGT = binOpCompare (>) (>)

binOpLT :: Eval ()
binOpLT = binOpCompare (<) (<)

binOpLTE :: Eval ()
binOpLTE = binOpCompare (<=) (<=)

binOpGTE :: Eval ()
binOpGTE = binOpCompare (>=) (>=)

binOpEQ :: Eval ()
binOpEQ = binOpCompare (==) (==)

binOpNEQ :: Eval ()
binOpNEQ = binOpCompare (/=) (/=)

binOpMultiply :: Eval () 
binOpMultiply =
   binOp $ BinOp
      (\x y -> IntObject (y * x))
      (\x y -> FloatObject (y * x))

binOpPower :: Eval () 
binOpPower =
   binOp $ BinOp
      (\x y -> IntObject (y ^ x))
      (\x y -> FloatObject (y ** x))

binOpAdd :: Eval () 
binOpAdd =
   binOp $ BinOp
      (\x y -> IntObject (y + x))
      (\x y -> FloatObject (y + x))

binOpModulo :: Eval () 
binOpModulo =
   binOp $ BinOp
      (\x y -> IntObject (mod y x))
      (\x y -> FloatObject (mod' y x))

binOpSubtract :: Eval () 
binOpSubtract =
   binOp $ BinOp
      (\x y -> IntObject (y - x))
      (\x y -> FloatObject (y - x))

binOpDivide :: Eval () 
binOpDivide =
   binOp $ BinOp
      (\x y -> FloatObject ((fromIntegral y) / (fromIntegral x)))
      (\x y -> FloatObject (y / x))

-- binOp :: BinOp -> HeapObject -> HeapObject ->  Eval ()
binOp :: BinOp -> Eval ()
binOp ops = do
   first <- popStack
   second <- popStack
   object1 <- lookupHeap first 
   object2 <- lookupHeap second 
   let resultObject = case (object1, object2) of
          (IntObject x, IntObject y) ->
             binOpIntInt ops x y
          (FloatObject x, FloatObject y) -> 
             binOpFloatFloat ops x y
          (IntObject x, FloatObject y) -> 
             binOpFloatFloat ops (fromIntegral x) y
          (FloatObject x, IntObject y) -> 
             binOpFloatFloat ops x (fromIntegral y)
          (_other1, _other2) ->
             error "binary operator called on non int or float arguments"
   objectID <- allocateHeapObject resultObject
   pushStack objectID
-- XXX FIXME

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
