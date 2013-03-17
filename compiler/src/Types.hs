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
   (Identifier, CompileConfig (..), NameID, NameCache
   , ConstantID, ConstantCache, CompileState (..), BlockState (..)
   , AnnotatedCode (..), LabelMap, Dumpable (..), VarSet
   , DefinitionScope (..), NestedScope (..)) where

import Data.Set (Set (..))
import Blip.Bytecode (Bytecode (..))
import Blip.Marshal (PyObject (..))
import Data.Word (Word32, Word16)
import qualified Data.Map as Map
import qualified Data.Set as Set

type VarSet = Set Identifier

data DefinitionScope
   = DefinitionScope
     { definitionScope_locals :: !VarSet
     , definitionScope_freeVars :: !VarSet
     , definitionScope_cellVars :: !VarSet
     }
     deriving Show

newtype NestedScope
   = NestedScope (Map.Map Identifier (DefinitionScope, NestedScope))
   deriving Show

data Dumpable = DumpScope {- | something else -}
   deriving (Eq, Ord, Show)

data AnnotatedCode
   = Labelled 
     { annotatedCode_bytecode :: Bytecode
     , annotatedCode_label :: !Word16
     , annotatedCode_index :: !Word16 } 
   | UnLabelled 
     { annotatedCode_bytecode :: Bytecode
     , annotatedCode_index :: !Word16 }
   deriving Show

type Identifier = String -- a variable name

data CompileConfig =
   CompileConfig
   { compileConfig_magic :: Word32
   , compileConfig_dumps :: Set Dumpable
   }
   deriving (Eq, Show)

type NameID = Word16
type NameCache = Map.Map Identifier NameID

type ConstantID = Word16
type ConstantCache = Map.Map PyObject ConstantID 

data CompileState = CompileState
   { state_config :: CompileConfig
   , state_blockState :: BlockState
   , state_filename :: FilePath
   , state_globals :: VarSet
   -- , state_nestedScopeStack :: [(DefinitionScope, NestedScope)]
   }

type LabelMap = Map.Map Word16 Word16

data BlockState = BlockState 
   { state_label :: !Word16
   , state_instructions :: [AnnotatedCode]
   , state_labelNextInstruction :: !(Maybe Word16) -- maybe label the next emitted instruction
   , state_constants :: [PyObject] 
   , state_constantCache :: ConstantCache
   , state_nextConstantID :: !ConstantID
   , state_names :: [Identifier]
   , state_nameCache :: NameCache
   , state_nextNameID :: !NameID
   , state_objectName :: String
   , state_instruction_index :: !Word16
   , state_labelMap :: LabelMap
   , state_nestedScope :: NestedScope
   , state_locals :: VarSet
   , state_freeVars :: VarSet
   , state_cellVars :: VarSet
   }
   deriving (Show)
