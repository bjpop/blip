-----------------------------------------------------------------------------
-- |
-- Module      : State
-- Copyright   : (c) 2012, 2013 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Management of state for the compiler. There is global state which
-- persist through the whole compilation (such as command line flags), and
-- their is block state, which is local for the compilation of a block
-- of code.
--
-----------------------------------------------------------------------------
module State
   (setBlockState, getBlockState, initBlockState, initState, 
    emitCode, emitCodeNoArg, emitCodeArg, compileName, compileConstant,
    getFileName, newLabel, compileConstantEmit, labelNextInstruction,
    getObjectName, setObjectName, getLastInstruction, getLabelMap)
   where

import Monad (Compile (..))
import Types
   (Identifier, CompileConfig (..), NameID, NameMap
   , ConstantID, ConstantMap, CompileState (..), BlockState (..)
   , AnnotatedCode (..), LabelMap)
import Blip.Bytecode
   (Bytecode (..), Opcode (..), BytecodeArg (..), bytecodeSize)
import Blip.Marshal (PyObject (..))
import Data.Word (Word32, Word16)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State.Strict as State hiding (State)
import Control.Monad.State.Class (MonadState (..))

initBlockState :: BlockState
initBlockState = BlockState
   { state_label = 0
   , state_instructions = []
   , state_labelNextInstruction = Nothing
   , state_constants = Map.empty
   , state_nextConstantID = 0
   , state_names = Map.empty
   , state_nextNameID = 0
   , state_objectName = ""
   , state_instruction_index = 0
   , state_labelMap = Map.empty
   }

incInstructionIndex :: Bytecode -> Compile Word16
incInstructionIndex bytecode = do
   currentIndex <- getBlockState state_instruction_index
   let nextIndex = currentIndex + (fromIntegral $ bytecodeSize bytecode)
   modifyBlockState $ \s -> s { state_instruction_index = nextIndex }
   return currentIndex

initState :: CompileConfig -> FilePath -> CompileState
initState config pyFilename = CompileState
   { state_config = config
   , state_blockState = initBlockState
   , state_filename = pyFilename
   }

-- get the most recently added instruction to the state
getLastInstruction :: Compile (Maybe Bytecode)
getLastInstruction = do
   instructions <- getBlockState state_instructions
   if null instructions
      then return Nothing
      -- instructions are in reverse order, so the most recent
      -- one is at the front of the list
      else return $ Just $ annotatedCode_bytecode $ head instructions

getFileName :: Compile FilePath
getFileName = gets state_filename

getObjectName :: Compile String
getObjectName = getBlockState state_objectName

setObjectName :: String -> Compile ()
setObjectName str = modifyBlockState $ \s -> s { state_objectName = str }

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

newLabel :: Compile Word16
newLabel = do
   currentLabel <- getBlockState state_label
   let newLabel = currentLabel + 1
   modifyBlockState $ \s -> s { state_label = newLabel }
   return currentLabel

labelNextInstruction :: Word16 -> Compile ()
labelNextInstruction label = do
   currentLabelNext <- getBlockState state_labelNextInstruction
   -- an instruction should be labelled at most once.
   case currentLabelNext of
      Just _label -> error "Attempt to label an instruction twice"
      Nothing -> modifyBlockState $ \ s -> s { state_labelNextInstruction = Just label }

emitCodeArg :: Opcode -> Word16 -> Compile ()
emitCodeArg opCode arg = emitCode $ Bytecode opCode (Just $ Arg16 arg)

emitCodeNoArg :: Opcode -> Compile ()
emitCodeNoArg opCode = emitCode $ Bytecode opCode Nothing

emitCode :: Bytecode -> Compile ()
emitCode instruction = do
   -- Attach a label to the instruction if necesary.
   maybeLabel <- getBlockState state_labelNextInstruction
   instructionIndex <- incInstructionIndex instruction
   annotatedInstruction <-
      case maybeLabel of
         Nothing ->
            return $ UnLabelled { annotatedCode_bytecode = instruction, 
                                  annotatedCode_index = instructionIndex }
         Just label -> do
            -- make sure we only use this label once
            modifyBlockState $ \s -> s { state_labelNextInstruction = Nothing }
            updateLabelMap label instructionIndex
            return $ Labelled { annotatedCode_bytecode = instruction 
                              , annotatedCode_label = label
                              , annotatedCode_index = instructionIndex }
   oldInstructions <- getBlockState state_instructions
   modifyBlockState $
      \s -> s { state_instructions = annotatedInstruction : oldInstructions }

getLabelMap :: Compile LabelMap
getLabelMap = getBlockState state_labelMap

updateLabelMap :: Word16 -> Word16 -> Compile ()
updateLabelMap label index = do
   oldLabelMap <- getBlockState state_labelMap
   let newLabelMap = Map.insert label index oldLabelMap
   modifyBlockState $ \s -> s { state_labelMap = newLabelMap }

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

-- XXX probably only want to consider equality on simple constants, not functions
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

compileConstantEmit :: PyObject -> Compile ()
compileConstantEmit obj = do
   constantID <- compileConstant obj
   emitCodeArg LOAD_CONST constantID
