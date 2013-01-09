{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies, 
    TypeSynonymInstances, FlexibleInstances, RecordWildCards #-}

module Compile (compileFile, CompileConfig (..)) where

import Scope (Scope (..), empty )
import Blip.Marshal as Blip (writePyc, PycFile (..), PyObject (..))
import Blip.Bytecode (Bytecode (..), BytecodeArg (..), Opcode (..), encode)
import Language.Python.Version3.Parser (parseModule)
import Language.Python.Common.AST as AST
   (ModuleSpan (..), Module (..), StatementSpan (..), Statement (..)
   , ExprSpan (..), Expr (..), Ident (..))
import System.FilePath ((<.>), takeBaseName)
import System.Directory (doesFileExist)
import System.IO (openFile, IOMode(..), Handle, hClose)
import Data.Word (Word32, Word16)
import Control.Monad.State.Strict as State hiding (State)
import Control.Monad.State.Class (MonadState (..))
import Control.Applicative (Applicative (..))
import Data.Traversable as Traversable (Traversable (..))
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as B (empty)
import Data.List (sort)
import Data.Bits ((.&.), shiftR)

type Identifier = String -- a variable name

type BlockID = Integer
type BlockMap = Map.Map BlockID [Bytecode]

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

initBlockState :: BlockState
initBlockState = BlockState
   { state_blockMap = Map.empty
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

newtype Compile a
   = Compile (StateT CompileState IO a)
   deriving (Monad, Functor, MonadIO, Applicative)

instance MonadState CompileState Compile where
   get = Compile get
   put s = Compile $ put s

runCompileMonad :: Compile a -> CompileState -> IO a
runCompileMonad (Compile comp) = evalStateT comp

setBlockState :: BlockState -> Compile ()
setBlockState blockState = do
   oldState <- get
   put $ oldState { state_blockState = blockState }

getBlockState :: Compile BlockState
getBlockState = gets state_blockState

newBlock :: Compile BlockID
newBlock = do
   blockState <- getBlockState
   let blockID = state_nextBlockID blockState
       oldBlockMap = state_blockMap blockState
       newBlockMap = Map.insert blockID [] oldBlockMap
       newBlockState = blockState 
                       { state_blockMap = newBlockMap
                       , state_nextBlockID = blockID + 1
                       }
   setBlockState newBlockState
   return blockID

useBlock :: BlockID -> Compile ()
useBlock blockID = do
   blockState <- getBlockState
   setBlockState $ blockState { state_currentBlockID = blockID }

emitCodeArg :: Opcode -> Word16 -> Compile ()
emitCodeArg opCode arg = emitCode $ Bytecode opCode (Just $ Arg16 arg)

emitCodeNoArg :: Opcode -> Compile ()
emitCodeNoArg opCode = emitCode $ Bytecode opCode Nothing

-- XXX this might be a bit slow, maybe we should cache the current
-- block in the compiler state, so we don't have to look it up all the time
emitCode :: Bytecode -> Compile ()
emitCode bytecode = do
   blockState <- getBlockState
   let blockMap = state_blockMap blockState
       currentBlock = state_currentBlockID blockState
       newBlockMap = Map.insertWith' (++) currentBlock [bytecode] blockMap
   setBlockState $ blockState { state_blockMap = newBlockMap }

compileName :: Identifier -> Compile NameID
compileName ident = do
   blockState <- getBlockState
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
   blockState <- getBlockState
   let constantMap = state_constants blockState
   case Map.lookup obj constantMap of
      Nothing -> do
         let constantID = state_nextConstantID blockState
             newConstants = Map.insert obj constantID constantMap
         setBlockState $ blockState 
            { state_nextConstantID = constantID + 1, state_constants = newConstants }
         return constantID
      Just constantID -> return constantID

compileFile :: CompileConfig -> FilePath -> IO ()
compileFile config path = do
   fileExists <- doesFileExist path
   if not fileExists
      then error $ "Python source file not found: " ++ path
      else do
         fileContents <- readFile path
         pyModule <- parseAndCheckErrors fileContents path
         pyc <- compileModule config pyModule
         let pycFilePath = takeBaseName path <.> ".pyc"
         handle <- openFile pycFilePath WriteMode 
         writePyc handle pyc
         hClose handle

parseAndCheckErrors :: String -> FilePath -> IO ModuleSpan
parseAndCheckErrors fileContents sourceName =
   case parseModule fileContents sourceName of
      Left e -> error $ show e
      Right (pyModule, _comments) -> return pyModule

compileModule :: CompileConfig -> ModuleSpan -> IO PycFile
compileModule config mod = do
   let state = initState config 
   obj <- compiler mod state
   return $ PycFile
      { magic = compileConfig_magic config 
      , modified_time = 12345
      , size = 10
      , object = obj }

compiler :: Compilable a => a -> CompileState -> IO (CompileResult a)
compiler = runCompileMonad . compile

class Compilable a where
   type CompileResult a :: *
   compile :: a -> Compile (CompileResult a)

instance Compilable a => Compilable [a] where
   type CompileResult [a] = [CompileResult a]
   compile = Traversable.mapM compile

instance Compilable a => Compilable (Maybe a) where
   type CompileResult (Maybe a) = Maybe (CompileResult a)
   compile = Traversable.mapM compile

instance Compilable ModuleSpan where
   type CompileResult ModuleSpan = PyObject
   compile (Module stmts) = compile $ Body stmts

-- body of module, function and class
newtype Body = Body [StatementSpan]

instance Compilable Body where
   type CompileResult Body = PyObject
   compile (Body stmts) = do
      setBlockState initBlockState
      Traversable.mapM compile stmts
      returnNone
      state <- getBlockState
      let blockMap = state_blockMap state
          code = assemble blockMap
          stackSize = getMaxStackSize blockMap
      makeObject (state_names state) (state_constants state)
                 code stackSize

-- XXX fixme
assemble :: BlockMap -> [Bytecode]
assemble blockMap =
   case Map.lookup 0 blockMap of
      Just code -> reverse code
      Nothing -> []

-- XXX fixme
getMaxStackSize :: BlockMap -> Integer
getMaxStackSize _ = 10 

makeObject :: NameMap -> ConstantMap -> [Bytecode] -> Integer -> Compile PyObject
makeObject names constants code maxStackSize = do
   let stackSizeLimit = (fromIntegral (maxBound :: Word32) :: Integer)
   if maxStackSize > stackSizeLimit
      -- XXX make a better error message
      then error "Maximum stack size exceeded"
      else do
         let obj = Code
                   { argcount = 0
                   , kwonlyargcount = 0
                   , nlocals = 0
                   , stacksize = fromIntegral maxStackSize 
                   , flags = 0
                   , code = String $ encode code
                   , consts = makeConstants constants
                   , names = makeNames names
                   , varnames = Blip.Tuple []
                   , freevars = Blip.Tuple [] 
                   , cellvars = Blip.Tuple []
                   , filename = Unicode "somefile"
                   , name = Unicode "somename"
                   , firstlineno = 0
                   , lnotab = String B.empty
                   }
         return obj

makeConstants :: ConstantMap -> PyObject
makeConstants constantMap =
   Blip.Tuple $ theObjects
   where
   theObjects = map snd $ sort $ [(constantID, obj) | (obj, constantID) <- Map.toList constantMap]

makeNames :: NameMap -> PyObject
makeNames nameMap = 
   Blip.Tuple $ theObjects
   where
   theObjects = map snd $ sort $ [(nameID, Unicode name) | (name, nameID) <- Map.toList nameMap]

instance Compilable StatementSpan where
   type CompileResult StatementSpan = ()
   compile (Assign [Var ident _] e _) = do
      compile e
      nameID <- compileName $ ident_string ident
      emitCodeArg STORE_NAME nameID
   compile s = error ("Unsupported statement " ++ show s)

instance Compilable ExprSpan where
   type CompileResult ExprSpan = ()
   compile (AST.Int val _ _) = do
       -- XXX should check for overflow
       constID <- compileConstant (Blip.Int $ fromIntegral val)
       emitCodeArg LOAD_CONST constID

returnNone :: Compile ()
returnNone = do
   constID <- compileConstant Blip.None
   emitCodeArg LOAD_CONST constID
   emitCodeNoArg RETURN_VALUE

-- Compute the effect of each opcode on the size of the stack.
-- This is used to compute an upper bound on the size of the stack
-- for each code object. It is safe to over-estimate the size of the
-- effect, but it is unsafe to underestimate it. Over-estimation will
-- potentially result in the stack being bigger than needed, which would
-- waste memory but otherwise be safe. Under-estimation will likely result
-- in the stack being too small and a serious fatal error in the interpreter, such
-- as segmentation fault (or reading/writing some other part of memory).
-- Some opcodes have different effect size depending on other factors, this function
-- convservatively takes the largest possible value.
-- This function is supposed to be identical in behaviour to opcode_stack_effect
-- in Python/compile.c.
codeStackEffect :: Bytecode -> Int
codeStackEffect bytecode@(Bytecode {..}) = 
   case opcode of
      POP_TOP -> -1
      ROT_TWO -> 0
      ROT_THREE -> 0
      DUP_TOP -> 1
      DUP_TOP_TWO -> 2
      UNARY_POSITIVE -> 0
      UNARY_NEGATIVE -> 0
      UNARY_NOT -> 0
      UNARY_INVERT -> 0
      SET_ADD -> -1
      LIST_APPEND -> -1
      MAP_ADD -> -2
      BINARY_POWER -> -1
      BINARY_MULTIPLY -> -1
      BINARY_MODULO -> -1
      BINARY_ADD -> -1
      BINARY_SUBTRACT -> -1
      BINARY_SUBSCR -> -1
      BINARY_FLOOR_DIVIDE -> -1
      BINARY_TRUE_DIVIDE -> -1
      INPLACE_FLOOR_DIVIDE -> -1
      INPLACE_TRUE_DIVIDE -> -1
      INPLACE_ADD -> -1
      INPLACE_SUBTRACT -> -1
      INPLACE_MULTIPLY -> -1
      INPLACE_MODULO -> -1
      STORE_SUBSCR -> -3
      STORE_MAP -> -2
      DELETE_SUBSCR -> -2
      BINARY_LSHIFT -> -1
      BINARY_RSHIFT -> -1
      BINARY_AND -> -1
      BINARY_XOR -> -1
      BINARY_OR -> -1
      INPLACE_POWER -> -1
      GET_ITER -> 0
      PRINT_EXPR -> -1
      LOAD_BUILD_CLASS -> 1
      INPLACE_LSHIFT -> -1
      INPLACE_RSHIFT -> -1
      INPLACE_AND -> -1
      INPLACE_XOR -> -1
      INPLACE_OR -> -1
      BREAK_LOOP -> 0
      SETUP_WITH -> 7
      WITH_CLEANUP -> -1 -- Sometimes more
      STORE_LOCALS -> -1
      RETURN_VALUE -> -1
      IMPORT_STAR -> -1
      YIELD_VALUE -> 0
      YIELD_FROM -> -1
      POP_BLOCK -> 0
      POP_EXCEPT -> 0  -- -3 except if bad bytecode
      END_FINALLY -> -1 -- or -2 or -3 if exception occurred
      STORE_NAME -> -1
      DELETE_NAME -> 0
      UNPACK_SEQUENCE -> withArg $ \oparg -> oparg - 1
      UNPACK_EX -> withArg $ \oparg -> (oparg .&. 0xFF) + (oparg `shiftR` 8)
      FOR_ITER -> 1 -- or -1, at end of iterator
      STORE_ATTR -> -2
      DELETE_ATTR -> -1
      STORE_GLOBAL -> -1
      DELETE_GLOBAL -> 0
      LOAD_CONST -> 1
      LOAD_NAME -> 1
      BUILD_TUPLE -> withArg $ \oparg -> 1 - oparg
      BUILD_LIST -> withArg $ \oparg -> 1 - oparg
      BUILD_SET -> withArg $ \oparg -> 1 - oparg
      BUILD_MAP -> 1
      LOAD_ATTR -> 0
      COMPARE_OP -> -1
      IMPORT_NAME -> -1
      IMPORT_FROM -> 1
      JUMP_FORWARD -> 0
      JUMP_IF_TRUE_OR_POP -> 0 -- -1 if jump not taken
      JUMP_IF_FALSE_OR_POP -> 0 -- ditto
      JUMP_ABSOLUTE -> 0
      POP_JUMP_IF_FALSE -> -1
      POP_JUMP_IF_TRUE -> -1
      LOAD_GLOBAL -> 1
      CONTINUE_LOOP -> 0
      SETUP_LOOP -> 0
      SETUP_EXCEPT -> 6
      SETUP_FINALLY -> 6 -- can push 3 values for the new exception
                         -- plus 3 others for the previous exception state
      LOAD_FAST -> 1
      STORE_FAST -> -1
      DELETE_FAST -> 0
      RAISE_VARARGS -> withArg $ \oparg -> -1 * oparg
      CALL_FUNCTION -> withArg $ \oparg -> -1 * nargs oparg
      CALL_FUNCTION_VAR -> withArg $ \oparg -> (-1 * nargs oparg) - 1
      CALL_FUNCTION_KW -> withArg $ \oparg -> (-1 * nargs oparg) - 1 
      CALL_FUNCTION_VAR_KW -> withArg $ \oparg -> (-1 * nargs oparg) - 2
      MAKE_FUNCTION -> withArg $ \oparg -> -1 - (nargs oparg) - ((oparg `shiftR` 16) .&. 0xffff)
      MAKE_CLOSURE -> withArg $ \oparg -> -2 - (nargs oparg) - ((oparg `shiftR` 16) .&. 0xffff)
      BUILD_SLICE -> withArg $ \oparg -> if oparg == 3 then -2 else -1
      LOAD_CLOSURE -> 1
      LOAD_DEREF -> 1
      STORE_DEREF -> -1
      DELETE_DEREF -> 0
      other -> error $ "unexpected opcode in codeStackEffect: " ++ show bytecode
   where
   -- #define NARGS(o) (((o) % 256) + 2*(((o) / 256) % 256)) 
   nargs :: Int -> Int
   nargs o = (o `mod` 256) + (2 * ((o `div` 256) `mod` 256))
   withArg :: (Int -> Int) -> Int
   withArg f
      = case args of
           Nothing -> error $ "codeStackEffect: " ++ (show opcode) ++ " missing argument"
           Just (Arg16 word16) -> f $ fromIntegral word16
           -- other -> error $ "codeStackEffect unexpected opcode argument: " ++ show other
