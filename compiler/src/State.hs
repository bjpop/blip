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
-- there is block state, which is local for the compilation of a block
-- of code.
--
-----------------------------------------------------------------------------
module State
   ( setBlockState, getBlockState, initBlockState, initState, modifyBlockState
   , emitCode, emitCodeNoArg, emitCodeArg, compileName, compileConstant
   , getFileName, newLabel, compileConstantEmit, labelNextInstruction
   , getObjectName, setObjectName, getLastInstruction, getLabelMap, getGlobals
   , getNestedScope, ifDump, emptyVarSet, emptyDefinitionScope
   , lookupNestedScope )
   where

import Monad (Compile (..))
import Types
   (Identifier, CompileConfig (..), NameID, NameCache
   , ConstantID, ConstantCache, CompileState (..), BlockState (..)
   , AnnotatedCode (..), LabelMap, Dumpable, VarSet, NestedScope (..)
   , DefinitionScope (..) )
import Blip.Bytecode
   (Bytecode (..), Opcode (..), BytecodeArg (..), bytecodeSize)
import Blip.Marshal (PyObject (..))
import Data.Word (Word32, Word16)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State.Strict as State hiding (State)
import Control.Monad.State.Class (MonadState (..))

emptyVarSet :: VarSet
emptyVarSet = Set.empty

emptyDefinitionScope :: DefinitionScope
emptyDefinitionScope =
   DefinitionScope
   { definitionScope_locals = emptyVarSet
   , definitionScope_freeVars = emptyVarSet
   , definitionScope_cellVars = emptyVarSet
   }

initBlockState :: DefinitionScope -> NestedScope -> BlockState
initBlockState definitionScope nestedScope = BlockState
   { state_label = 0
   , state_instructions = []
   , state_labelNextInstruction = Nothing
   , state_constants = [] 
   , state_constantCache = Map.empty
   , state_nextConstantID = 0
   , state_names = []
   , state_nameCache = Map.empty
   , state_nextNameID = 0
   , state_objectName = ""
   , state_instruction_index = 0
   , state_labelMap = Map.empty
   , state_nestedScope = nestedScope
   , state_locals = definitionScope_locals definitionScope 
   , state_freeVars = definitionScope_freeVars definitionScope 
   , state_cellVars = definitionScope_cellVars definitionScope 
   }

incInstructionIndex :: Bytecode -> Compile Word16
incInstructionIndex bytecode = do
   currentIndex <- getBlockState state_instruction_index
   let nextIndex = currentIndex + (fromIntegral $ bytecodeSize bytecode)
   modifyBlockState $ \s -> s { state_instruction_index = nextIndex }
   return currentIndex

initState :: VarSet -> DefinitionScope -> NestedScope -> CompileConfig -> FilePath -> CompileState
initState globals definitionScope nestedScope config pyFilename = CompileState
   { state_config = config
   , state_blockState = initBlockState definitionScope nestedScope
   , state_filename = pyFilename
   , state_globals = globals
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

ifDump :: Dumpable -> Compile () -> Compile ()
ifDump dumpable action = do
   state <- get
   if dumpable `Set.member` (compileConfig_dumps $ state_config state)
      then action
      else return () 

getGlobals :: Compile VarSet
getGlobals = gets state_globals

-- get the nested scope for the current block
getNestedScope :: Compile NestedScope
getNestedScope = getBlockState state_nestedScope

lookupNestedScope :: Identifier -> Compile (DefinitionScope, NestedScope)
lookupNestedScope name = do
   NestedScope nestedScope <- getNestedScope
   case Map.lookup name nestedScope of
      Just scope -> return scope
      -- this case should never happen
      Nothing -> error $ "no scope found for: " ++ name
   
{-
   nestedScopeStack <- gets state_nestedScopeStack
   case nestedScopeStack of
      (_definitionScope, nestedScope):_rest -> return nestedScope
      -- the nestedScope stack should never be empty
      -- so the following case should never happen
      [] -> error "nested scope stack is empty"
-}

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
   let nameCache = state_nameCache blockState
   case Map.lookup ident nameCache of
      -- We haven't seen this name before
      Nothing -> do
         let nameID = state_nextNameID blockState
             newNameCache = Map.insert ident nameID nameCache
             oldNames = state_names blockState 
         setBlockState $
            blockState { state_nextNameID = nameID + 1
                       , state_nameCache = newNameCache 
                       , state_names = ident : oldNames }
         return nameID
      Just nameID -> return nameID

compileConstant :: PyObject -> Compile ConstantID
-- Code objects are not cached to avoid complex equality comparisons
compileConstant obj@(Code {}) = do
   oldConstants <- getBlockState state_constants
   constantID <- getBlockState state_nextConstantID 
   modifyBlockState $
      \s -> s { state_constants = obj : oldConstants
              , state_nextConstantID = constantID + 1 }
   return constantID
compileConstant obj = do
   blockState <- getBlockState id
   let constantCache = state_constantCache blockState
   case Map.lookup obj constantCache of
      -- We haven't seen this (non-code) constant before
      Nothing -> do
         let constantID = state_nextConstantID blockState
             newConstantCache = Map.insert obj constantID constantCache 
             oldConstants = state_constants blockState
         setBlockState $ blockState
            { state_nextConstantID = constantID + 1
            , state_constantCache = newConstantCache
            , state_constants = obj : oldConstants }
         return constantID
      Just constantID -> return constantID

compileConstantEmit :: PyObject -> Compile ()
compileConstantEmit obj = do
   constantID <- compileConstant obj
   emitCodeArg LOAD_CONST constantID
