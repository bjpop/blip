module Types 
   (Identifier, BlockID, BlockMap, CompileConfig (..), NameID, NameMap
   , ConstantID, ConstantMap, CompileState (..), BlockState (..), BlockVal (..)) where

import Blip.Bytecode (Bytecode (..))
import Blip.Marshal (PyObject (..))
import Data.Word (Word32, Word16)
import qualified Data.Map as Map
import qualified Data.Set as Set

type Identifier = String -- a variable name

-- this limits us to 2**16 blocks == 65536 blocks, is that too small?
type BlockID = Word16 
-- type BlockMap = Map.Map BlockID [Bytecode]
data BlockVal =
   BlockVal
   { block_code :: [Bytecode]      -- the byte code of the block
   , block_next :: Maybe BlockID   -- possibly jump to a following block
   }
   deriving (Show)
type BlockMap = Map.Map BlockID BlockVal

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
   }

data BlockState = BlockState 
   { state_blockMap :: BlockMap
   , state_nextBlockID :: !BlockID
   , state_currentBlockID :: BlockID
   , state_constants :: ConstantMap
   , state_nextConstantID :: !ConstantID
   , state_names :: NameMap
   , state_nextNameID :: !NameID
   }
   deriving (Show)
