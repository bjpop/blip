{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Types
-- Copyright   : (c) 2013 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Types for the bytecode interpreter
--
-----------------------------------------------------------------------------

module Types
   ( Eval (..)
   , EvalState (..)
   , Heap
   , ObjectID
   , HeapObject (..)
   , Stack
   , StackObject (..)
   , ProgramCounter
   , Globals 
   , PrimFun
   )  where

import qualified Data.Map as Map (insert, lookup, empty)
import Data.Map (Map)
import Control.Monad.State.Strict as State hiding (State)
import Control.Applicative (Applicative (..))
import qualified Data.ByteString.Lazy as B (ByteString)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int64, Int32)
import Data.Vector (Vector)

type PrimFun = [ObjectID] -> Eval ()

data EvalState =
   EvalState 
   { evalState_objectID :: !ObjectID
   , evalState_heap :: !Heap
   , evalState_programCounter :: !ProgramCounter
   , evalState_stack :: !Stack
   , evalState_globals :: !Globals
   }

newtype Eval a
   = Eval (StateT EvalState IO a)
   deriving (Monad, Functor, MonadIO, Applicative)

instance MonadState EvalState Eval where
   get = Eval get
   put s = Eval $ put s

type ProgramCounter = Int64
type ObjectID = Word64
type Heap = Map ObjectID HeapObject
type Stack = [StackObject]

type StackObject = ObjectID

-- hack to get things working, 
-- should really be a dictionary object on the heap
type Globals = Map String ObjectID

data HeapObject 
   = CodeObject
        { codeObject_argcount :: !Word32 -- #arguments, except *args
        , codeObject_kwonlyargcount :: !Word32 -- #keyword only arguments 
        , codeObject_nlocals :: !Word32 -- #local variables
        , codeObject_stacksize :: !Word32 -- #entries needed for evaluation stack
        , codeObject_flags :: !Word32 -- CO_..., see below
        , codeObject_code :: !ObjectID -- instruction opcodes (a string)
        , codeObject_consts :: !ObjectID -- list (constants used) XXX seems to be a tuple
        , codeObject_names :: !ObjectID -- list of strings (names used)
        , codeObject_varnames :: !ObjectID -- tuple of strings (local variable names)
        , codeObject_freevars :: !ObjectID -- tuple of strings (free variable names)
        , codeObject_cellvars :: !ObjectID -- tuple of strings (cell variable names)
        , codeObject_filename :: !ObjectID -- unicode (where it was loaded from)
        , codeObject_name :: !ObjectID -- unicode (name, for reference)
        , codeObject_firstlineno :: !Word32 -- first source line number
        , codeObject_lnotab :: !ObjectID -- string (encoding addr<->lineno mapping)
        }
   | StringObject { stringObject_string :: !B.ByteString }
   | TupleObject { tupleObject_elements :: !(Vector ObjectID) }
   | IntObject { initObject_value :: !Int32 }  
   | FloatObject { floatObject_value :: !Double }
   | NoneObject
   | EllipsisObject
   | UnicodeObject { unicodeObject_value :: !String } -- should be decoded into a String
   | TrueObject
   | FalseObject
   | ComplexObject { complexObject_real :: !Double, complexObject_imaginary :: !Double }
   | LongObject { longObject_value :: !Integer }
   | Primitive
       { primitiveArity :: Int
       , primitiveName :: String
       , primitiveFun :: PrimFun
       }

instance Show HeapObject where
   show object = "heap object"
