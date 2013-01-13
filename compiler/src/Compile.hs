{-# LANGUAGE TypeFamilies, 
    TypeSynonymInstances, FlexibleInstances, RecordWildCards #-}

module Compile (compileFile, CompileConfig (..)) where

import Monad (Compile (..), runCompileMonad, setBlockState, getBlockState)
import StackDepth (maxStackDepth)
import Types
   (Identifier, BlockID, BlockMap, CompileConfig (..), NameID, NameMap
   , ConstantID, ConstantMap, CompileState (..), BlockState (..))
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
import Data.Traversable as Traversable (mapM)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as B (empty)
import Data.List (sort)

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
          stackDepth = maxStackDepth 0 blockMap
      makeObject (state_names state) (state_constants state)
                 code stackDepth

-- XXX fixme
assemble :: BlockMap -> [Bytecode]
assemble blockMap =
   case Map.lookup 0 blockMap of
      Just code -> reverse code
      Nothing -> []

makeObject :: NameMap -> ConstantMap -> [Bytecode] -> Word32 -> Compile PyObject
makeObject names constants code maxStackDepth = do
   if maxStackDepth > maxBound
      -- XXX make a better error message
      then error "Maximum stack depth exceeded"
      else do
         let obj = Code
                   { argcount = 0
                   , kwonlyargcount = 0
                   , nlocals = 0
                   , stacksize = maxStackDepth 
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
makeConstants constantMap = mapToObject constantMap id

makeNames :: NameMap -> PyObject
makeNames nameMap = mapToObject nameMap Unicode

mapToObject :: Map.Map key Word16 -> (key -> PyObject) -> PyObject
mapToObject theMap keyToObj = 
   Blip.Tuple $ theObjects
   where
   theObjects = map snd $ sort $ 
      [(identity, keyToObj key) | (key, identity) <- Map.toList theMap]

instance Compilable StatementSpan where
   type CompileResult StatementSpan = ()
   -- XXX fix multiple assignment
   compile (Assign [Var ident _] e _) = do
      compile e
      nameID <- compileName $ ident_string ident
      emitCodeArg STORE_NAME nameID
   compile s = error ("Unsupported statement " ++ show s)

instance Compilable ExprSpan where
   type CompileResult ExprSpan = ()
   compile (AST.Int {..}) = do
       -- XXX should check for overflow
       constID <- compileConstant (Blip.Int $ fromIntegral int_value)
       emitCodeArg LOAD_CONST constID

returnNone :: Compile ()
returnNone = do
   constID <- compileConstant Blip.None
   emitCodeArg LOAD_CONST constID
   emitCodeNoArg RETURN_VALUE
