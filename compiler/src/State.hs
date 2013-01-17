module State
   (setBlockState, getBlockState, initBlockState, initState, 
    newBlock, useBlock, setNextBlock, emitCode, emitCodeArg, emitCodeNoArg,
    compileName, compileConstant, reverseBlockMapBytecodes)
   where

import Monad (Compile (..))
import Types
   (Identifier, BlockID, BlockMap, CompileConfig (..), NameID, NameMap
   , ConstantID, ConstantMap, CompileState (..), BlockState (..), BlockVal (..))
import Blip.Bytecode (Bytecode (..), Opcode (..), BytecodeArg (..))
import Blip.Marshal (PyObject (..))
import Data.Word (Word32, Word16)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State.Strict as State hiding (State)
import Control.Monad.State.Class (MonadState (..))

initBlockState :: BlockState
initBlockState = BlockState
   { state_blockMap = Map.singleton 0 initBlockVal
   , state_nextBlockID = 0
   , state_currentBlockID = 0
   , state_constants = Map.empty
   , state_nextConstantID = 0
   , state_names = Map.empty
   , state_nextNameID = 0
   }

initState :: CompileConfig -> CompileState
initState config = CompileState
   { state_config = config
   , state_blockState = initBlockState
   }

-- After compiling a Block the bytecodes are in reverse order.
-- This function puts them back in the correct order.
reverseBlockMapBytecodes :: BlockMap -> BlockMap
reverseBlockMapBytecodes = Map.map revBytecodes
   where
   revBytecodes :: BlockVal -> BlockVal
   revBytecodes blockVal = blockVal { block_code = reverse oldCode }
      where
      oldCode = block_code blockVal

initBlockVal :: BlockVal
initBlockVal = BlockVal { block_code = [], block_next = Nothing }

setBlockState :: BlockState -> Compile ()
setBlockState blockState = do
   oldState <- get
   put $ oldState { state_blockState = blockState }

getBlockState :: (BlockState -> a) -> Compile a 
getBlockState f = gets (f . state_blockState)

modifyBlockState :: (BlockState -> BlockState) -> Compile ()
modifyBlockState f = do
   state <- getBlockState id
   setBlockState $! f state

getNextBlockID :: Compile BlockID
getNextBlockID = getBlockState state_nextBlockID

getCurrentBlockID :: Compile BlockID
getCurrentBlockID = getBlockState state_currentBlockID

getBlockMap :: Compile BlockMap
getBlockMap = getBlockState state_blockMap

lookupBlockMapMaybe :: BlockID -> Compile (Maybe BlockVal)
lookupBlockMapMaybe blockID = do
   blockMap <- getBlockMap
   return $ Map.lookup blockID blockMap

lookupBlock :: BlockID -> Compile BlockVal
lookupBlock blockID = do
   result <- lookupBlockMapMaybe blockID
   case result of
      Nothing -> error $ "lookupBlock failed on blockID: " ++ show blockID
      Just blockVal -> return blockVal

lookupCurrentBlock :: Compile BlockVal
lookupCurrentBlock = do
   currentBlockID <- getCurrentBlockID
   lookupBlock currentBlockID

updateCurrentBlock :: (BlockVal -> BlockVal) -> Compile ()
updateCurrentBlock f = do
   currentBlockID <- getCurrentBlockID
   blockMap <- getBlockMap
   let newBlockMap = Map.adjust f currentBlockID blockMap
   modifyBlockState $ \state -> state { state_blockMap = newBlockMap }

-- doesn't change the current block ID
newBlock :: Compile BlockID
newBlock = do
   blockID <- getNextBlockID
   blockMap <- getBlockMap
   modifyBlockState $ \state -> do
      let newBlockMap = Map.insert blockID initBlockVal blockMap 
      state { state_blockMap = newBlockMap
            , state_nextBlockID = blockID + 1
            }
   return blockID

useBlock :: BlockID -> Compile ()
useBlock blockID
   = modifyBlockState $ \state -> state { state_currentBlockID = blockID }

-- same as compiler_use_next_block
-- set the current block to point to the next blockID
-- then set the next blockID to be the current block ID 
setNextBlock :: BlockID -> Compile ()
setNextBlock nextBlockID = do
   updateCurrentBlock $ \blockVal -> blockVal { block_next = Just nextBlockID }
   useBlock nextBlockID 

emitCodeArg :: Opcode -> Word16 -> Compile ()
emitCodeArg opCode arg = emitCode $ Bytecode opCode (Just $ Arg16 arg)

emitCodeNoArg :: Opcode -> Compile ()
emitCodeNoArg opCode = emitCode $ Bytecode opCode Nothing

-- XXX this might be a bit slow, maybe we should cache the current
-- block in the compiler state, so we don't have to look it up all the time
emitCode :: Bytecode -> Compile ()
emitCode bytecode =
   updateCurrentBlock $ addInstruction bytecode
   where
   addInstruction :: Bytecode -> BlockVal -> BlockVal 
   addInstruction instruction blockVal =
      blockVal { block_code = instruction : oldCode }
      where
      oldCode = block_code blockVal

compileName :: Identifier -> Compile NameID
compileName ident = do
   blockState <- getBlockState id
   let nameMap = state_names blockState
   case Map.lookup ident nameMap of
      Nothing -> do
         let nameID = state_nextNameID blockState
             newNames = Map.insert ident nameID nameMap
         setBlockState $ blockState { state_nextNameID = nameID + 1, state_names = newNames }
         return nameID
      Just nameID -> return nameID

compileConstant :: PyObject -> Compile ConstantID 
compileConstant obj = do
   blockState <- getBlockState id
   let constantMap = state_constants blockState
   case Map.lookup obj constantMap of
      Nothing -> do
         let constantID = state_nextConstantID blockState
             newConstants = Map.insert obj constantID constantMap
         setBlockState $ blockState
            { state_nextConstantID = constantID + 1, state_constants = newConstants }
         return constantID
      Just constantID -> return constantID
