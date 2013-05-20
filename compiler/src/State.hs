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
   , getObjectName, setObjectName, getLabelMap
   , getNestedScope, ifDump, emptyVarSet, emptyLocalScope
   , getLocalScope, indexedVarSetKeys, lookupNameVar
   , emitReadVar, emitWriteVar, emitDeleteVar, lookupClosureVar, setFlag
   , pushFrameBlock, popFrameBlock, peekFrameBlock, withFrameBlock 
   , setFastLocals, setArgCount, emptyParameterTypes )
   where

import Monad (Compile (..))
import Types
   ( Identifier, CompileConfig (..), VarIndex, IndexedVarSet
   , ConstantID, CompileState (..), BlockState (..)
   , AnnotatedCode (..), LabelMap, Dumpable, VarSet, NestedScope (..)
   , LocalScope (..), VarInfo (..), ScopeIdentifier
   , FrameBlockInfo (..), Context (..), ParameterTypes (..) )
import Blip.Bytecode
   (Bytecode (..), Opcode (..), BytecodeArg (..), bytecodeSize)
import Blip.Marshal (PyObject (..), CodeObjectFlagMask, co_varargs, co_varkeywords)
import Data.Word (Word16, Word32)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State.Strict as State hiding (State)
import Data.List (sort)
import Data.Bits ((.|.))
import Utils (identsFromParameters, countPosParameters)

emptyVarSet :: VarSet
emptyVarSet = Set.empty

emptyParameterTypes :: ParameterTypes
emptyParameterTypes =
   ParameterTypes { parameterTypes_pos = []
                  , parameterTypes_varPos = Nothing
                  , parameterTypes_varKeyword = Nothing
                  }

emptyLocalScope :: LocalScope
emptyLocalScope =
   LocalScope
   { localScope_params = emptyParameterTypes 
   , localScope_locals = emptyVarSet
   , localScope_freeVars = emptyVarSet
   , localScope_cellVars = emptyVarSet
   , localScope_explicitGlobals = emptyVarSet
   }

initBlockState :: Context -> LocalScope -> BlockState
initBlockState context (LocalScope {..}) = BlockState
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
   , state_locals = localScope_locals
   , state_fastLocals =
        if context == FunctionContext
           then makeLocalsIndexedSet (identsFromParameters localScope_params)
                   localScope_locals 
           else Map.empty
   -- the indices for cellvars and freevars are used as offsets into an array
   -- within the code object. The cellvars come first, followed by the
   -- freevars. These indices are used by the LOAD_DEREF and STORE_DEREF
   -- bytecode instructions.
   -- cellvars are indexed from 0 upwards
   , state_cellVars = indexedVarSet 0 $ localScope_cellVars 
   -- freevars are indexed from (length cellvars) 
   , state_freeVars = indexedVarSet
                         (fromIntegral $ Set.size localScope_cellVars) 
                         localScope_freeVars 
   , state_explicitGlobals = localScope_explicitGlobals
   , state_argcount = fromIntegral $ countPosParameters localScope_params
   , state_flags = varArgsFlags localScope_params 0
   , state_frameBlockStack = []
   , state_context = context
   }

varArgsFlags :: ParameterTypes -> Word32 -> Word32
varArgsFlags (ParameterTypes {..}) flags =
   flags .|. posVarArgsMask .|. keywordVarArgsMask
   where
   posVarArgsMask :: CodeObjectFlagMask
   posVarArgsMask = 
      maybe 0 (const co_varargs) parameterTypes_varPos
   keywordVarArgsMask :: CodeObjectFlagMask
   keywordVarArgsMask =
      maybe 0 (const co_varkeywords) parameterTypes_varPos

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

-- Return the keys of an IndexedVarSet in ascending order of the indices
indexedVarSetKeys :: IndexedVarSet -> [Identifier]
indexedVarSetKeys varset =
   map snd $ sort [ (index, name) | (name, index) <- Map.assocs varset ]

incInstructionIndex :: Bytecode -> Compile Word16
incInstructionIndex bytecode = do
   currentIndex <- getBlockState state_instruction_index
   let nextIndex = currentIndex + (fromIntegral $ bytecodeSize bytecode)
   modifyBlockState $ \s -> s { state_instruction_index = nextIndex }
   return currentIndex

setFastLocals :: [Identifier] -> Compile ()
setFastLocals idents = do
   let localsVarSet = Set.fromList idents
   modifyBlockState $ \s -> s { state_fastLocals = indexedVarSet 0 localsVarSet }

setArgCount :: Word32 -> Compile ()
setArgCount n = modifyBlockState $ \s -> s { state_argcount = n }

initState :: Context       -- module, class or function?
          -> LocalScope    -- local scope of the top-level of the module
          -> NestedScope   -- nested scope of the rest of the module (not at the top-level)
          -> CompileConfig -- configuration options
          -> FilePath      -- file path of the Python source
          -> CompileState
initState context localScope nestedScope config pyFilename = CompileState
   { state_config = config
   , state_blockState = initBlockState context localScope
   , state_filename = pyFilename
   , state_nestedScope = nestedScope
   }

ifDump :: Dumpable -> Compile () -> Compile ()
ifDump dumpable action = do
   state <- get
   if dumpable `Set.member` (compileConfig_dumps $ state_config state)
      then action
      else return () 

-- get the nested scope for the current block
getNestedScope :: Compile NestedScope
getNestedScope = gets state_nestedScope

getLocalScope :: ScopeIdentifier -> Compile (String, LocalScope)
getLocalScope scopeIdent = do
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

{-
          | Free  | Cell  | Local | Explicit Global | Implicit Global
---------------------------------------------------------------------
Class     | Deref | X     | Name  | Global          | Name          
Module    | X     | X     | Name  | X               | Name
Funcition | Deref | Deref | Fast  | Global          | Global

-}

data VarOpcodeType = Deref | Name | Global | Fast

emitReadVar :: Identifier -> Compile ()
emitReadVar ident = do
   (opcodeType, index) <- getVarOpcodeType ident
   case opcodeType of
      Deref -> emitCodeArg LOAD_DEREF index
      Name -> emitCodeArg LOAD_NAME index
      Global -> emitCodeArg LOAD_GLOBAL index
      Fast -> emitCodeArg LOAD_FAST index

emitWriteVar :: Identifier -> Compile ()
emitWriteVar ident = do
   (opcodeType, index) <- getVarOpcodeType ident
   case opcodeType of
      Deref -> emitCodeArg STORE_DEREF index
      Name -> emitCodeArg STORE_NAME index
      Global -> emitCodeArg STORE_GLOBAL index
      Fast -> emitCodeArg STORE_FAST index

emitDeleteVar :: Identifier -> Compile ()
emitDeleteVar ident = do
   (opcodeType, index) <- getVarOpcodeType ident
   case opcodeType of
      Deref -> emitCodeArg DELETE_DEREF index
      Name -> emitCodeArg DELETE_NAME index
      Global -> emitCodeArg DELETE_GLOBAL index
      Fast -> emitCodeArg DELETE_FAST index

getVarOpcodeType :: Identifier -> Compile (VarOpcodeType, VarIndex)
getVarOpcodeType ident = do
   context <- getBlockState state_context
   varInfo <- lookupVar ident
   getVarInContext context varInfo
   where
   getVarInContext :: Context -> VarInfo -> Compile (VarOpcodeType, VarIndex)
   getVarInContext ClassContext info =
      case info of
         FreeVar index -> return (Deref, index)
         LocalVar -> do
            index <- lookupNameVar ident
            return (Name, index)
         ExplicitGlobal -> do
            index <- lookupNameVar ident
            return (Global, index)
         ImplicitGlobal -> do
            index <- lookupNameVar ident
            return (Name, index)
         CellVar _index -> error $ "class with a cell variable: " ++ ident
   getVarInContext ModuleContext info =
      case info of
         LocalVar -> do
            index <- lookupNameVar ident
            return (Name, index)
         ImplicitGlobal -> do
            index <- lookupNameVar ident
            return (Name, index)
         FreeVar _index ->
            error $ "module with a free variable: " ++ ident
         CellVar _index ->
            error $ "module with a cell variable: " ++ ident
         ExplicitGlobal ->
            error $ "module with an explicit global variable: " ++ ident
   getVarInContext FunctionContext info =
      case info of
         FreeVar index -> return (Deref, index)
         CellVar index -> return (Deref, index)
         LocalVar -> do
            fastLocals <- getBlockState state_fastLocals 
            case Map.lookup ident fastLocals of
               Just index -> return (Fast, index)
               Nothing -> error $ "local function variable not in fast locals: " ++ ident
         ExplicitGlobal -> do
            index <- lookupNameVar ident
            return (Global, index)
         ImplicitGlobal -> do
            index <- lookupNameVar ident
            return (Global, index)

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
  freevar
  explicit global
  implicit global 

We check local vars first before free vars because classes can
have a variable with the same name being local and also free.
If a local version of the variable is defined, that is the
one we want to see (not the free variable). If we need to
see the free variable, then we can look it up specially.

If we can't find it defined anywhere then we presume it
to be an implicit global variable.

-}

lookupVar :: Identifier -> Compile VarInfo
lookupVar identifier = do
   -- cell
   cellvars <- getBlockState state_cellVars
   case Map.lookup identifier cellvars of
      Just index -> return $ CellVar index
      Nothing -> do
         -- local
         locals <- getBlockState state_locals
         if identifier `Set.member` locals
            then return LocalVar
            else do
               -- free
               freevars <- getBlockState state_freeVars
               case Map.lookup identifier freevars of
                  Just index -> return $ FreeVar index
                  Nothing -> do
                     -- explicit global
                     explicitGlobals <- getBlockState state_explicitGlobals
                     if identifier `Set.member` explicitGlobals
                        then return ExplicitGlobal
                        -- implicit global 
                        else return ImplicitGlobal

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

-- look up a variable in the "names" vector. Add it if it is not there.
-- return the index of the variable in the vector.
lookupNameVar :: Identifier -> Compile VarIndex
lookupNameVar ident = do
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
         return index 
      Just index -> return index 

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
