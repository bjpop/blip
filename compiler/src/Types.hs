module Types 
   (Identifier, CompileConfig (..), NameID, NameMap
   , ConstantID, ConstantMap, CompileState (..), BlockState (..), Labelled (..)) where

import Blip.Bytecode (Bytecode (..))
import Blip.Marshal (PyObject (..))
import Data.Word (Word32, Word16)
import qualified Data.Map as Map
import qualified Data.Set as Set

data Labelled a =
   Labelled a Word16 | UnLabelled a
   deriving (Eq, Show)

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

data BlockState = BlockState 
   { state_label :: !Word16
   , state_instructions :: [Labelled Bytecode]
   , state_labelNextInstruction :: !(Maybe Word16) -- maybe label the next emitted instruction
   , state_constants :: ConstantMap
   , state_nextConstantID :: !ConstantID
   , state_names :: NameMap
   , state_nextNameID :: !NameID
   , state_objectName :: String
   }
   deriving (Show)
