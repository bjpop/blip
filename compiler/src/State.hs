{-# LANGUAGE RecordWildCards #-}

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
-- persists through the whole compilation (such as command line flags), and
-- there is block state, which is local for the compilation of a block
-- of code.
--
-----------------------------------------------------------------------------
module State
   ( setBlockState, getBlockState, initBlockState, initState, modifyBlockState
   , emitCode, emitCodeNoArg, emitCodeArg, compileConstant
   , getFileName, newLabel, compileConstantEmit, labelNextInstruction
   , getObjectName, setObjectName, getLabelMap, getGlobals
   , getNestedScope, ifDump, emptyVarSet, emptyDefinitionScope
   , lookupNestedScope, indexedVarSetKeys, lookupVar, lookupGlobalVar
   , emitReadVar, emitWriteVar, lookupClosureVar, setFlag, pushFrameBlock
   , popFrameBlock, peekFrameBlock, withFrameBlock )
   where

import Monad (Compile (..))
import Types
   (Identifier, CompileConfig (..), VarIndex, IndexedVarSet
   , ConstantID, CompileState (..), BlockState (..)
   , AnnotatedCode (..), LabelMap, Dumpable, VarSet, NestedScope (..)
   , DefinitionScope (..), VarInfo (..), ScopeIdentifier, CodeObjectFlagMask 
   , FrameBlockInfo (..))
import Blip.Bytecode
   (Bytecode (..), Opcode (..), BytecodeArg (..), bytecodeSize)
import Blip.Marshal (PyObject (..))
import Data.Word (Word16)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State.Strict as State hiding (State)
import Data.List (sort)
import Data.Bits ((.|.))

emptyVarSet :: VarSet
emptyVarSet = Set.empty

emptyDefinitionScope :: DefinitionScope
emptyDefinitionScope =
   DefinitionScope
   { definitionScope_params = []
   , definitionScope_locals = emptyVarSet
   , definitionScope_freeVars = emptyVarSet
   , definitionScope_cellVars = emptyVarSet
   , definitionScope_classLocals = emptyVarSet
   }

initBlockState :: DefinitionScope -> NestedScope -> BlockState
initBlockState (DefinitionScope {..}) nestedScope = BlockState
   { state_label = 0
   , state_instructions = []
   , state_labelNextInstruction = [] 
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
   , state_locals = makeLocalsIndexedSet 
                       definitionScope_params
                       definitionScope_locals 
   -- the indices for cellvars and freevars are used as offsets into an array
   -- within the code object. The cellvars come first, followed by the
   -- freevars. These indices are used by the LOAD_DEREF and STORE_DEREF
   -- bytecode instructions.
   -- cellvars are indexed from 0 upwards
   , state_cellVars = indexedVarSet 0 $ definitionScope_cellVars 
   -- freevars are indexed from (length cellvars) 
   , state_freeVars = indexedVarSet
                         (fromIntegral $ Set.size definitionScope_cellVars) 
                         definitionScope_freeVars 
   , state_classLocals = definitionScope_classLocals 
   , state_argcount = fromIntegral $ length definitionScope_params
   , state_flags = 0
   , state_frameBlockStack = []
   }

-- Local variables are indexed starting with parameters first, in the order
-- that they appear in the function head, followed by the other
-- locally defined variables, which can appear in any order.
makeLocalsIndexedSet :: [Identifier] -> VarSet -> IndexedVarSet
makeLocalsIndexedSet params locals =
   Map.fromList $ zip (params ++ Set.toList localsNotParams) [0..]
   where
   localsNotParams = locals `Set.difference` Set.fromList params

indexedVarSet :: VarIndex -> VarSet -> IndexedVarSet
indexedVarSet from set =
   Map.fromList $ sort $ zip (Set.toList set) [from..]

indexedVarSetKeys :: IndexedVarSet -> [Identifier]
indexedVarSetKeys = Map.keys

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

lookupNestedScope :: ScopeIdentifier -> Compile (String, DefinitionScope, NestedScope)
lookupNestedScope scopeIdent = do
   NestedScope nestedScope <- getNestedScope
   case Map.lookup scopeIdent nestedScope of
      Just scope -> return scope
      -- this case should never happen
      Nothing -> error $ "no scope found for: " ++ show scopeIdent

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

-- prefix this new label onto the existing ones
labelNextInstruction :: Word16 -> Compile ()
labelNextInstruction newLabel = do
   currentLabels <- getBlockState state_labelNextInstruction
   modifyBlockState $ \ s -> s { state_labelNextInstruction = newLabel : currentLabels }

emitReadVar :: VarInfo -> Compile ()
emitReadVar (LocalVar index) = emitCodeArg LOAD_FAST index
emitReadVar (CellVar index) = emitCodeArg LOAD_DEREF index
emitReadVar (FreeVar index) = emitCodeArg LOAD_DEREF index
emitReadVar (GlobalVar index) = emitCodeArg LOAD_NAME index

emitWriteVar :: VarInfo -> Compile ()
emitWriteVar (LocalVar index) = emitCodeArg STORE_FAST index
emitWriteVar (CellVar index) = emitCodeArg STORE_DEREF index
emitWriteVar (FreeVar index) = emitCodeArg STORE_DEREF index
emitWriteVar (GlobalVar index) = emitCodeArg STORE_NAME index

emitCodeArg :: Opcode -> Word16 -> Compile ()
emitCodeArg opCode arg = emitCode $ Bytecode opCode (Just $ Arg16 arg)

emitCodeNoArg :: Opcode -> Compile ()
emitCodeNoArg opCode = emitCode $ Bytecode opCode Nothing

emitCode :: Bytecode -> Compile ()
emitCode instruction = do
   -- Attach a label to the instruction if necesary.
   labels <- getBlockState state_labelNextInstruction
   -- Ensure current labels are used only once.
   modifyBlockState $ \s -> s { state_labelNextInstruction = [] }
   instructionIndex <- incInstructionIndex instruction
   forM_ labels $ \label -> updateLabelMap label instructionIndex
   let annotatedInstruction =
          AnnotatedCode { annotatedCode_bytecode = instruction
                        , annotatedCode_labels = labels
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

{-

check if var is:

  cellvar
  localvar
  classLocal
  freevar
  globalvar

in that order.

We check cell vars first, because a cellvar is a special type of
local var, so it must take preference.

We check classLocal before freevar because classes can have a locally
defined variable with the same name as a free variable.

-}

lookupVar :: Identifier -> Compile VarInfo
lookupVar identifier = do
   cellvars <- getBlockState state_cellVars
   case Map.lookup identifier cellvars of
      Just index -> return $ CellVar index
      Nothing -> do
         locals <- getBlockState state_locals
         case Map.lookup identifier locals of
            Just index -> return $ LocalVar index
            Nothing -> do
               classLocals <- getBlockState state_classLocals
               if identifier `Set.member` classLocals 
                  then lookupGlobalVar identifier
                  else do
                     freevars <- getBlockState state_freeVars
                     case Map.lookup identifier freevars of
                        Just index -> return $ FreeVar index
                        Nothing -> lookupGlobalVar identifier

{- lookup a variable in cell vars or free vars only.
   We avoid looking in other places because, for example,
   classes can have free variables with the same name as
   locally defined variables, and we don't want to get them
   confused.
-}

lookupClosureVar :: Identifier -> Compile (Maybe VarInfo)
lookupClosureVar identifier = do
   cellvars <- getBlockState state_cellVars
   case Map.lookup identifier cellvars of
      Just index -> return $ Just $ CellVar index
      Nothing -> do
         freevars <- getBlockState state_freeVars
         case Map.lookup identifier freevars of
            Just index -> return $ Just $ FreeVar index
            Nothing -> return Nothing

-- references to globals are recorded in the "names" entry
-- in a code object.
-- XXX this should probably be called lookupNameVar
lookupGlobalVar :: Identifier -> Compile VarInfo
lookupGlobalVar ident = do
   blockState <- getBlockState id
   let nameCache = state_nameCache blockState
   case Map.lookup ident nameCache of
      -- We haven't seen this name before
      Nothing -> do
         let index = state_nextNameID blockState
             newNameCache = Map.insert ident index nameCache
             oldNames = state_names blockState 
         setBlockState $
            blockState { state_nextNameID = index + 1
                       , state_nameCache = newNameCache 
                       , state_names = ident : oldNames }
         return $ GlobalVar index 
      Just index -> return $ GlobalVar index 

-- set a flag in the code object by applying a mask 
setFlag :: CodeObjectFlagMask -> Compile ()
setFlag mask = do
    oldFlags <- getBlockState state_flags 
    let newFlags = oldFlags .|. mask
    modifyBlockState $ \state -> state { state_flags = newFlags }

pushFrameBlock :: FrameBlockInfo -> Compile ()
pushFrameBlock info = do
   oldFrameStack <- getBlockState state_frameBlockStack
   let newFrameStack = info : oldFrameStack
   modifyBlockState $ \state -> state { state_frameBlockStack = newFrameStack }

popFrameBlock :: Compile FrameBlockInfo
popFrameBlock = do
   oldFrameStack <- getBlockState state_frameBlockStack
   case oldFrameStack of
      [] -> error "attempt to pop from an empty frame block stack"
      top:rest -> do
         modifyBlockState $ \state -> state { state_frameBlockStack = rest }
         return top

peekFrameBlock :: Compile (Maybe FrameBlockInfo)
peekFrameBlock = do
   oldFrameStack <- getBlockState state_frameBlockStack
   case oldFrameStack of
      [] -> return Nothing
      top:_rest -> return $ Just top

withFrameBlock :: FrameBlockInfo -> Compile a -> Compile a
withFrameBlock pushedInfo comp = do 
    pushFrameBlock pushedInfo
    result <- comp
    poppedInfo <- popFrameBlock
    if pushedInfo /= poppedInfo
       then error $ "pushed frame block not equal to popped frame block"
       else return result
