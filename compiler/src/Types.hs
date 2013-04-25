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
   , DefinitionScope (..), NestedScope (..), VarInfo (..)
   , ScopeIdentifier, FrameBlockInfo (..)
   ) where

import Data.Set (Set)
import Blip.Bytecode (Bytecode (..))
import Blip.Marshal (PyObject (..))
import Data.Word (Word32, Word16)
import qualified Data.Map as Map
import Language.Python.Common.SrcLocation (SrcSpan)

-- information about how a variable is bound plus its offset into
-- the appropriate structure
data VarInfo
   = LocalVar VarIndex 
   | CellVar VarIndex 
   | FreeVar VarIndex 
   | GlobalVar VarIndex

type VarSet = Set Identifier

data DefinitionScope
   = DefinitionScope
     { definitionScope_params :: ![Identifier]
     , definitionScope_locals :: !VarSet
     , definitionScope_freeVars :: !VarSet
     , definitionScope_cellVars :: !VarSet
     , definitionScope_classLocals :: !VarSet
     }
     deriving Show

type ScopeIdentifier = SrcSpan

newtype NestedScope
   = NestedScope (Map.Map ScopeIdentifier (String, DefinitionScope, NestedScope))
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
   , state_globals :: VarSet
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
   , state_nestedScope :: NestedScope
   , state_locals :: IndexedVarSet
   , state_freeVars :: IndexedVarSet
   , state_cellVars :: IndexedVarSet
   , state_classLocals :: VarSet
   , state_argcount :: !Word32
   , state_flags :: !Word32
   , state_frameBlockStack :: [FrameBlockInfo]
   }
   deriving (Show)

data FrameBlockInfo
   = FrameBlockLoop !Word16
   | FrameBlockExcept 
   | FrameBlockFinallyTry 
   | FrameBlockFinallyEnd
   deriving (Eq, Show)

