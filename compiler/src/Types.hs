module Types 
   (Identifier, CompileConfig (..), NameID, NameMap
   , ConstantID, ConstantMap, CompileState (..), BlockState (..)
   , AnnotatedCode (..), LabelMap) where

import Blip.Bytecode (Bytecode (..))
import Blip.Marshal (PyObject (..))
import Data.Word (Word32, Word16)
import qualified Data.Map as Map
import qualified Data.Set as Set

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
   }
   deriving (Eq, Show)

type NameID = Word16
type NameMap = Map.Map Identifier NameID

type ConstantID = Word16
type ConstantMap = Map.Map PyObject ConstantID 

data CompileState = CompileState
   { state_config :: CompileConfig
   , state_blockState :: BlockState
   , state_filename :: FilePath
   }

type LabelMap = Map.Map Word16 Word16

data BlockState = BlockState 
   { state_label :: !Word16
   , state_instructions :: [AnnotatedCode]
   , state_labelNextInstruction :: !(Maybe Word16) -- maybe label the next emitted instruction
   , state_constants :: ConstantMap
   , state_nextConstantID :: !ConstantID
   , state_names :: NameMap
   , state_nextNameID :: !NameID
   , state_objectName :: String
   , state_instruction_index :: !Word16
   , state_labelMap :: LabelMap
   }
   deriving (Show)
