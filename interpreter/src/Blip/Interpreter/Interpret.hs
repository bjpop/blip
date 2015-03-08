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

module Blip.Interpreter.Interpret
   (interpretFile, interpretObject) where

import Data.ByteString.Lazy as BS (length, index)
import Data.Fixed (mod')
import Text.Printf (printf)
import Data.List as List (length, reverse)
import Control.Monad as Monad (replicateM)
import Control.Monad.Trans (liftIO)
import System.IO (withFile, IOMode (..))
import Blip.Marshal (readPyc, PycFile (..), PyObject (..))
import Blip.Bytecode
   ( Opcode (..), word8ToOpcode, hasArg, word8sToWord16 )
import qualified Data.Map as Map (lookup)
import Data.Word (Word16, Word32)
import Data.Vector as Vector
   ( fromList, replicateM, reverse, thaw, (!))
import Data.Vector.Generic.Mutable as MVector (new, read, write)
import Blip.Interpreter.Types 
   ( Eval (..) )
import Blip.Interpreter.State 
   ( runEvalMonad,lookupHeap, initState, getProgramCounter, incProgramCounter
   , pushValueStack, popValueStack, popValueStackObject, getGlobal, setGlobal
   , allocateHeapObject, allocateHeapObjectPush, lookupName, lookupNameID
   , setProgramCounter, peekValueStackFromTop, peekValueStackFromBottom
   , lookupConst, pushFrame, popFrame, pokeValueStack ) 
import Blip.Interpreter.Types (ObjectID, HeapObject (..))
import Blip.Interpreter.HashTable.Basic as HT (newSized, insert, lookup)
import Blip.Interpreter.Builtins (initBuiltins, hashObject, eqObject, typeOf)
import Blip.Interpreter.StandardObjectID (noneObjectID)

interpretFile :: FilePath -> IO ()
interpretFile pycFilename = do
   withFile pycFilename ReadMode $ \handle -> do
      pycFile <- readPyc handle 
      runEvalMonad initState $ do
         initBuiltins
         interpretObject (object pycFile) >> return ()

interpretObject :: PyObject -> Eval ObjectID
interpretObject object = do
   objectID <- loadIntoHeap object
   heapObject <- lookupHeap objectID
   let stackSize = getCodeStackSize heapObject 
   valueStack <- liftIO $ MVector.new $ fromIntegral stackSize
   let newFrame =
          FrameObject
          { frameCode = objectID
          , frameValueStack = valueStack
          , frameStackPointer = -1 -- empty stack
          , frameProgramCounter = 0
          }
   pushFrame newFrame
   heapObject <- lookupHeap objectID
   case heapObject of
      CodeObject {} -> evalCodeObject heapObject
      _other -> error "evalTopObject: not a code object"

-- XXX shouldn't keep looking up the bytecode string
-- from the heap every time
evalCodeObject :: HeapObject -> Eval ObjectID
evalCodeObject object@(CodeObject {..}) = do
   pc <- getProgramCounter
   -- stack <- getValueStack
   -- liftIO $ printf "Program counter: %d\n" pc
   bytecodeObject <- lookupHeap codeObject_code
   let code = 
          case bytecodeObject of
             StringObject {..} -> stringObject_string
             _other -> error "Bytecode not a string"
   let numInstructions = BS.length code 
   -- check the program counter is in valid range
   if pc < 0 || pc >= numInstructions
      then error $ printf "Bad program counter %d" pc
      else do
         let nextOpCodeWord8 = BS.index code pc
         case Map.lookup nextOpCodeWord8 word8ToOpcode of
            Nothing -> error ("bad op code: " ++ show nextOpCodeWord8)
            Just opCode -> do
               if hasArg opCode
                  then do
                     let arg1 = BS.index code (pc + 1)
                         arg2 = BS.index code (pc + 2)
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

evalOpCode :: HeapObject -> Opcode -> Word16 -> Eval ObjectID
evalOpCode _codeObject RETURN_VALUE _arg = popValueStack
evalOpCode codeObject opcode arg =
   evalOneOpCode codeObject opcode arg >> evalCodeObject codeObject

-- Some opcodes don't use the arg, but we pass it anyway (a dummy arg) to simplify
-- the program
evalOneOpCode :: HeapObject -> Opcode -> Word16 -> Eval ()
evalOneOpCode (CodeObject {..}) opcode arg =
   case opcode of
      POP_TOP -> popValueStack >> return () 
      ROT_TWO -> do
         first <- popValueStack
         second <- popValueStack
         pushValueStack first 
         pushValueStack second 
      -- (first:second:third:rest) -> (second:third:first:rest)
      ROT_THREE -> do
         first <- popValueStack
         second <- popValueStack
         third <- popValueStack
         pushValueStack first 
         pushValueStack third
         pushValueStack second
      DUP_TOP -> peekValueStackFromTop 0 >>= pushValueStack
      DUP_TOP_TWO -> do
         first <- peekValueStackFromTop 0 
         second <- peekValueStackFromTop 1
         pushValueStack second
         pushValueStack first
      NOP -> return ()
      UNARY_POSITIVE ->
         unaryOp (\x -> if x < 0 then negate x else x)
                 (\x -> if x < 0 then negate x else x)
      UNARY_NEGATIVE ->
         unaryOp (\x -> if x > 0 then negate x else x)
                 (\x -> if x > 0 then negate x else x)
      UNARY_NOT -> do
         object <- popValueStackObject
         case object of
            TrueObject -> allocateHeapObjectPush FalseObject
            FalseObject -> allocateHeapObjectPush TrueObject 
            _other -> error "not applied to non boolean"
      BINARY_POWER -> binOpPower
      BINARY_MULTIPLY -> binOpMultiply
      BINARY_MODULO -> binOpModulo
      BINARY_ADD -> binOpAdd
      BINARY_SUBTRACT -> binOpSubtract
      BINARY_TRUE_DIVIDE -> binOpDivide
      -- INPLACE_ADD -> 
      -- XXX load name should really look in local scope, then enclosing, then global, then builtins
      LOAD_NAME -> do 
         nameString <- lookupName codeObject_names arg
         getGlobal nameString >>= pushValueStack
      LOAD_GLOBAL -> do 
         nameString <- lookupName codeObject_names arg
         getGlobal nameString >>= pushValueStack
      BUILD_LIST -> do
         elementIDs <- Vector.replicateM (fromIntegral arg) popValueStack
         -- arguments are popped off the stack in reverse order
         mvector <- liftIO $ Vector.thaw $ Vector.reverse elementIDs 
         allocateHeapObjectPush $ ListObject $ mvector
      STORE_NAME -> do
         nameString <- lookupName codeObject_names arg
         objectID <- popValueStack
         setGlobal nameString objectID
      LOAD_CONST -> lookupConst codeObject_consts arg >>= pushValueStack
      CALL_FUNCTION -> do
         functionArgs <- Monad.replicateM (fromIntegral arg) popValueStack
         functionObject <- popValueStackObject
         callFunction functionObject $ List.reverse functionArgs
      JUMP_ABSOLUTE -> setProgramCounter $ fromIntegral arg 
      -- program counter has already been advanced (by 3) to the next instruction
      -- the bytecode takes this into consideration
      JUMP_FORWARD -> incProgramCounter $ fromIntegral arg
      -- XXX setup loop should push a block onto the block stack
      SETUP_LOOP -> return ()
      -- XXX pop block should pop a block off the block stack
      POP_BLOCK -> return ()
      POP_JUMP_IF_FALSE -> do
         object <- popValueStackObject
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
            4 -> binOpGT -- >
            5 -> binOpGTE -- >=
            6 -> return () -- in
            7 -> return () -- not in
            8 -> return () -- is
            9 -> return () -- is not
            _other -> error $ "comparison operator not supported: " ++ show arg
      MAKE_FUNCTION -> do
         functionNameID <- popValueStack
         codeObjectID <- popValueStack
         allocateHeapObjectPush $ FunctionObject functionNameID codeObjectID
      LOAD_FAST ->
         peekValueStackFromBottom (fromIntegral arg) >>= pushValueStack 
      STORE_FAST -> 
         popValueStack >>= pokeValueStack (fromIntegral arg)
      BUILD_MAP -> do
         hashTable <- liftIO $ HT.newSized (fromIntegral arg) hashObject eqObject 
         allocateHeapObjectPush $ DictObject hashTable
      STORE_MAP -> do
         keyID <- popValueStack
         valID <- popValueStack
         dictID <- peekValueStackFromTop 0 
         dictObject <- lookupHeap dictID
         storeDict dictObject keyID valID
      BINARY_SUBSCR -> do
         indexID <- popValueStack
         container  <- popValueStackObject
         result <- getItem container indexID
         pushValueStack result 
      STORE_SUBSCR -> do
         indexID <- popValueStack
         container <- popValueStackObject
         valueID <- popValueStack
         setItem container indexID valueID 
      LOAD_ATTR -> do
         nameID <- lookupNameID codeObject_names arg
         objectID <- popValueStack
         attributeObjectID <- getAttribute objectID nameID
         pushValueStack attributeObjectID
      otherOpCode -> error $ "unsupported opcode: " ++ show otherOpCode
evalOneOpCode otherObject _opcode _arg =
    error $ "evalOneOpCode called on non code object: " ++ show otherObject

callFunction :: HeapObject -> [ObjectID] -> Eval ()
callFunction (PrimitiveObject arity _name fun) args
   | arity == List.length args = fun args
   | otherwise =
        error (printf "primitve of arity %d applied to %d arguments"
               arity (List.length args))
callFunction (FunctionObject _nameObjectID codeObjectID) args = do
   codeObject <- lookupHeap codeObjectID
   let stackSize = getCodeStackSize codeObject + (fromIntegral $ List.length args)
   valueStack <- liftIO $ MVector.new $ fromIntegral stackSize
   let newFrame =
          FrameObject
          { frameCode = codeObjectID 
          , frameValueStack = valueStack 
          , frameStackPointer = -1 -- empty stack
          , frameProgramCounter = 0
          }
   pushFrame newFrame
   mapM_ pushValueStack args
   codeObject <- lookupHeap codeObjectID
   resultObjectID <- evalCodeObject codeObject
   popFrame
   pushValueStack resultObjectID
callFunction (MethodObject functionID self) args = do
   functionObject <- lookupHeap functionID
   callFunction functionObject (self:args)
callFunction other _args = do
   error $ "call to unsupported object " ++ show other

unaryOp :: (Integer -> Integer) -> (Double -> Double) -> Eval()
unaryOp intOp floatOp = do
   object <- popValueStackObject
   case object of
      IntObject x -> do
         let posObject = IntObject $ intOp x
         allocateHeapObjectPush posObject 
      FloatObject x -> do
         let posObject = FloatObject $ floatOp x
         allocateHeapObjectPush posObject 
      _other -> error "unary operator applied to non number"

data BinOp =
   BinOp
   { binOpIntInt :: Integer -> Integer -> HeapObject 
   , binOpFloatFloat :: Double -> Double -> HeapObject
   }

binOpCompare :: (Integer -> Integer -> Bool) -> (Double -> Double -> Bool) -> Eval ()
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
   first <- popValueStack
   second <- popValueStack
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
   allocateHeapObjectPush resultObject
-- XXX FIXME

-- Load a PyObject from a pyc file into a heap, turning
-- each nested PyObject into its corresponding Object.
-- Returns the ID of the topmost inserted object
loadIntoHeap :: PyObject -> Eval ObjectID 
loadIntoHeap (Code {..}) = do
   codeID <- loadIntoHeap code
   constsID <- loadIntoHeap consts
   namesID <- loadIntoHeap names
   varnamesID <- loadIntoHeap varnames
   freevarsID <- loadIntoHeap freevars
   cellvarsID <- loadIntoHeap cellvars
   filenameID <- loadIntoHeap filename
   nameID <- loadIntoHeap name
   lnotabID <- loadIntoHeap lnotab
   allocateHeapObject $ 
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
loadIntoHeap (Tuple {..}) = do
   elementIDs <- mapM loadIntoHeap elements 
   allocateHeapObject $ TupleObject $ fromList elementIDs
-- None is allocated at initialisation time
loadIntoHeap None = return noneObjectID
loadIntoHeap atomicObject =
   allocateHeapObject $ atomicPyObjectToHeapObject atomicObject

atomicPyObjectToHeapObject :: PyObject -> HeapObject
atomicPyObjectToHeapObject (Long val) = LongObject val
atomicPyObjectToHeapObject (Complex {..})
   = ComplexObject { complexObject_real = real, complexObject_imaginary = imaginary }
atomicPyObjectToHeapObject TrueObj = TrueObject
atomicPyObjectToHeapObject FalseObj = FalseObject
atomicPyObjectToHeapObject (Unicode val) = UnicodeObject val
atomicPyObjectToHeapObject Ellipsis = EllipsisObject
atomicPyObjectToHeapObject (Float val) = FloatObject val
atomicPyObjectToHeapObject (Int val) = IntObject $ fromIntegral val
atomicPyObjectToHeapObject (String val) = StringObject val
atomicPyObjectToHeapObject None = NoneObject
atomicPyObjectToHeapObject other =
   error ("atomicPyObjectToHeapObject applied to non-atomic object: " ++ show other)

getCodeStackSize :: HeapObject -> Word32
getCodeStackSize (CodeObject {..}) = codeObject_stacksize
getCodeStackSize other =
   error $ "attempt to get stack size from non-code object: " ++ show other 

storeDict :: HeapObject -> ObjectID -> ObjectID -> Eval ()
storeDict object keyID valID =
    case object of
       DictObject {..} -> do
          HT.insert dictHashTable keyID valID
       other -> error $ "STORE_MAP on non dict object: " ++ show other

getItem :: HeapObject -> ObjectID -> Eval ObjectID
getItem container indexID =
   case container of
      -- XXX should check if index is in bounds
      TupleObject {..} -> do
         indexObject <- lookupHeap indexID
         case indexObject of
             IntObject indexVal -> do
                let indexInt = fromIntegral indexVal
                return (tupleObject_elements ! indexInt)
             _other -> error $ "tuple indexed with non integer"
      ListObject {..} -> do
         indexObject <- lookupHeap indexID
         case indexObject of
             IntObject indexVal -> do
                let indexInt = fromIntegral indexVal
                liftIO $ MVector.read listObject_elements indexInt
             _other -> error $ "list indexed with non integer"
      (DictObject {..}) -> do
         maybeObjectID <- HT.lookup dictHashTable indexID
         case maybeObjectID of
            -- XXX should raise key error exception
            Nothing -> error $ "Key Error"
            Just resultID -> return resultID
      -- XXX should check if index is in bounds
      _otherObject -> error $ "getItem not supported on: " ++ show container

setItem :: HeapObject -> ObjectID -> ObjectID -> Eval ()
setItem container indexID valueID =
   case container of
      DictObject {..} ->
         HT.insert dictHashTable indexID valueID
      -- XXX should check if index is in bounds
      ListObject {..} -> do
         indexObject <- lookupHeap indexID
         case indexObject of
             IntObject indexVal -> do
                let indexInt = fromIntegral indexVal
                liftIO $ MVector.write listObject_elements indexInt valueID
             _other -> error $ "list indexed with non integer"
      _otherObject -> error $ "setItem not supported on: " ++ show container

getAttribute :: ObjectID -> ObjectID -> Eval ObjectID
getAttribute objectID attributeID = do
   object <- lookupHeap objectID
   typeID <- typeOf object
   typeObject <- lookupHeap typeID
   case typeObject of
      TypeObject {..} -> do
         dictObject <- lookupHeap typeAttributes
         case dictObject of
            DictObject {..} -> do
               maybeValue <- HT.lookup dictHashTable attributeID
               case maybeValue of
                  Nothing -> error $ "getAttribute failed"
                  Just valueID -> do
                     valueObject <- lookupHeap valueID
                     case valueObject of
                        FunctionObject {} -> 
                           allocateHeapObject $ MethodObject valueID objectID
                        PrimitiveObject {} -> 
                           allocateHeapObject $ MethodObject valueID objectID
                        _other -> return valueID 
            other -> error $ "Type attributes not a dictionary: " ++ show other
      other -> error $ "Type of object not a type: " ++ show other
