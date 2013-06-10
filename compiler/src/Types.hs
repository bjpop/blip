-----------------------------------------------------------------------------
-- |
-- Module      : Types
-- Copyright   : (c) 2012, 2013 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Type definitions which are used in multiple modules.
--
-----------------------------------------------------------------------------
module Types 
   ( Identifier, CompileConfig (..), VarIndex, IndexedVarSet
   , ConstantID, ConstantCache, CompileState (..), BlockState (..)
   , AnnotatedCode (..), LabelMap, Dumpable (..), VarSet
   , LocalScope (..), NestedScope (..), VarInfo (..)
   , ScopeIdentifier, FrameBlockInfo (..), Context (..), ParameterTypes (..)
   ) where

import Data.Set (Set)
import Blip.Bytecode (Bytecode (..))
import Blip.Marshal (PyObject (..))
import Data.Word (Word32, Word16)
import qualified Data.Map as Map

-- The context in which a variable is used affects the bytecode
-- related to that use.
data Context
   = ModuleContext
   | ClassContext
   | FunctionContext
   deriving (Eq, Ord, Show)

-- information about how a variable is bound plus its offset into
-- the appropriate structure
data VarInfo
   = LocalVar
   | CellVar VarIndex 
   | FreeVar VarIndex 
   | ExplicitGlobal
   | ImplicitGlobal

type VarSet = Set Identifier

-- XXX need to handle keyword only paramters
data ParameterTypes
   = ParameterTypes
     { parameterTypes_pos :: ![Identifier]
     , parameterTypes_varPos :: !(Maybe Identifier)
     , parameterTypes_varKeyword :: !(Maybe Identifier)
     }
   deriving (Eq, Show)

data LocalScope
   = LocalScope
     { localScope_params :: !ParameterTypes
     , localScope_locals :: !VarSet
     , localScope_freeVars :: !VarSet
     , localScope_cellVars :: !VarSet
     , localScope_explicitGlobals :: !VarSet
     }
     deriving Show

-- start and end coordinates of span (row, col, row, col)
type ScopeIdentifier = (Int, Int, Int, Int)

-- mapping from source location to pair of (scope name, local scope)
newtype NestedScope =
   NestedScope (Map.Map ScopeIdentifier (String, LocalScope))
   deriving Show

data Dumpable = DumpScope | DumpAST
   deriving (Eq, Ord, Show)

data AnnotatedCode
   = AnnotatedCode 
     { annotatedCode_bytecode :: Bytecode
     , annotatedCode_labels :: ![Word16]   -- instruction can be labelled zero or more times
     , annotatedCode_index :: !Word16 }    -- byte offset of the instruction within this sequence of bytecode
   deriving Show

type Identifier = String -- a variable name

data CompileConfig =
   CompileConfig
   { compileConfig_magic :: Word32
   , compileConfig_dumps :: Set Dumpable
   }
   deriving (Eq, Show)

type ConstantID = Word16
type ConstantCache = Map.Map PyObject ConstantID 

data CompileState = CompileState
   { state_config :: CompileConfig
   , state_blockState :: BlockState
   , state_filename :: FilePath
   , state_nestedScope :: NestedScope
   }

-- Map from Label to Instruction offset.
-- The same instruction can be labelled multiple times,
-- but each label is attached to exactly one instruction.
type LabelMap = Map.Map Word16 Word16

type VarIndex = Word16
type IndexedVarSet = Map.Map Identifier VarIndex

data BlockState = BlockState 
   { state_label :: !Word16
   , state_instructions :: [AnnotatedCode]
   , state_labelNextInstruction :: [Word16] -- zero or more labels for the next instruction
   , state_constants :: [PyObject] 
   , state_constantCache :: ConstantCache
   , state_nextConstantID :: !ConstantID
   , state_names :: [Identifier]
   , state_nameCache :: IndexedVarSet
   , state_nextNameID :: !VarIndex
   , state_objectName :: String
   , state_instruction_index :: !Word16
   , state_labelMap :: LabelMap
   , state_locals :: VarSet
   , state_fastLocals :: IndexedVarSet
   , state_freeVars :: IndexedVarSet
   , state_cellVars :: IndexedVarSet
   , state_explicitGlobals :: VarSet
   , state_argcount :: !Word32
   , state_flags :: !Word32
   , state_frameBlockStack :: [FrameBlockInfo]
   , state_context :: !Context
   , state_lineNumber :: !Word32
   , state_lineNumberTable :: ![(Word16, Word32)] -- mapping from bytecode offset to source line number
   , state_firstLineNumber :: !Word32
   }
   deriving (Show)

data FrameBlockInfo
   = FrameBlockLoop !Word16
   | FrameBlockExcept 
   | FrameBlockFinallyTry 
   | FrameBlockFinallyEnd
   deriving (Eq, Show)
