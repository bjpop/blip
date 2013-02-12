{-# LANGUAGE TypeFamilies, 
    TypeSynonymInstances, FlexibleInstances, RecordWildCards #-}

module Compile (compileFile, CompileConfig (..)) where

import Prelude hiding (mapM)
import ProgName (progName)
import State
   (setBlockState, getBlockState, initBlockState, initState,
    emitCodeNoArg, emitCodeArg, compileName, compileConstantEmit,
    getFileName, newLabel, labelNextInstruction)
import Assemble (assemble)
import Monad (Compile (..), runCompileMonad)
import Types
   (Identifier, CompileConfig (..), NameID, NameMap
   , ConstantID, ConstantMap, CompileState (..), BlockState (..), Labelled (..))
import Scope (Scope (..), empty )
import Blip.Marshal as Blip (writePyc, PycFile (..), PyObject (..))
import Blip.Bytecode (Bytecode (..), BytecodeArg (..), Opcode (..), encode)
import Language.Python.Version3.Parser (parseModule)
import Language.Python.Common.AST as AST
   (ModuleSpan (..), Module (..), StatementSpan (..), Statement (..)
   , ExprSpan (..), Expr (..), Ident (..), ArgumentSpan (..), Argument (..)
   , OpSpan, Op (..))
import Language.Python.Common (prettyText)
import System.FilePath ((<.>), takeBaseName)
import System.Directory (doesFileExist, getModificationTime, canonicalizePath)
import System.Time (ClockTime (..))
import System.IO (openFile, IOMode(..), Handle, hClose, hFileSize, hGetContents)
import Data.Word (Word32, Word16)
import Data.Traversable as Traversable (mapM)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as B (empty)
import Data.List (sort)
import Control.Monad (unless)
import Control.Exception (try)
import System.IO.Error (IOError, userError, ioError)

compiler :: Compilable a => a -> CompileState -> IO (CompileResult a)
compiler = runCompileMonad . compile

class Compilable a where
   type CompileResult a :: *
   compile :: a -> Compile (CompileResult a)

compileFile :: CompileConfig -> FilePath -> IO ()
compileFile config path = do
   r <- try $ do
      pyHandle <- openFile path ReadMode
      sizeInBytes <- hFileSize pyHandle
      fileContents <- hGetContents pyHandle
      modifiedTime <- getModificationTime path
      let modSeconds = case modifiedTime of TOD secs _picoSecs -> secs
      pyModule <- parseAndCheckErrors fileContents path
      pyc <- compileModule config (fromIntegral modSeconds)
                (fromIntegral sizeInBytes) path pyModule
      let pycFilePath = takeBaseName path <.> ".pyc"
      pycHandle <- openFile pycFilePath WriteMode 
      writePyc pycHandle pyc
      hClose pycHandle
   -- XXX maybe we want more customised error messages for different kinds of
   -- IOErrors?
   case r of
      Left e -> putStrLn $ progName ++ ": " ++ show (e :: IOError)
      Right () -> return ()

parseAndCheckErrors :: String -> FilePath -> IO ModuleSpan
parseAndCheckErrors fileContents sourceName =
   case parseModule fileContents sourceName of
      Left e -> error $ "parse error: " ++ prettyText e
      Right (pyModule, _comments) -> return pyModule

compileModule :: CompileConfig -> Word32 -> Word32 -> FilePath -> ModuleSpan -> IO PycFile
compileModule config pyFileModifiedTime pyFileSizeBytes pyFileName mod = do
   canonicalPath <- canonicalizePath pyFileName
   let state = initState config canonicalPath
   obj <- compiler mod state
   return $ PycFile
      { magic = compileConfig_magic config 
      , modified_time = pyFileModifiedTime 
      , size = pyFileSizeBytes
      , object = obj }

{-
instance Compilable a => Compilable [a] where
   type CompileResult [a] = [CompileResult a]
   compile = mapM compile

instance Compilable a => Compilable (Maybe a) where
   type CompileResult (Maybe a) = Maybe (CompileResult a)
   compile = mapM compile
-}

instance Compilable ModuleSpan where
   type CompileResult ModuleSpan = PyObject
   compile (Module stmts) = compile $ Body stmts

-- body of module, function and class
newtype Body = Body [StatementSpan]

instance Compilable Body where
   type CompileResult Body = PyObject
   compile (Body stmts) = do
      setBlockState initBlockState
      compile $ BodySuite stmts
      -- XXX only return None if necessary
      returnNone
      state <- getBlockState id
      code <- assemble
      let stackDepth = 10
      makeObject (state_names state) (state_constants state)
         code stackDepth

newtype BodySuite = BodySuite [StatementSpan]

instance Compilable BodySuite where
   type CompileResult BodySuite = ()
   compile (BodySuite []) = return () 
   compile (BodySuite (s:ss)) = do
      case s of
         Assign [Var ident _] e _ -> do
            compile e
            nameID <- compileName $ ident_string ident
            emitCodeArg STORE_NAME nameID
         Return { return_expr = Nothing } -> returnNone
         Return { return_expr = Just expr } ->
            compile expr >> emitCodeNoArg RETURN_VALUE
         Pass {} -> return ()
         StmtExpr {..} -> 
            unless (isPureExpr stmt_expr) $ 
               compile stmt_expr >> emitCodeNoArg POP_TOP
         Conditional {..} -> do
            restLabel <- newLabel
            compileGuards restLabel cond_guards 
            compile $ BodySuite cond_else
            labelNextInstruction restLabel
         -- XXX need to handle else block
         While {..} -> do
            startLoop <- newLabel
            endLoop <- newLabel
            emitCodeArg SETUP_LOOP endLoop
            labelNextInstruction startLoop
            compile while_cond
            emitCodeArg POP_JUMP_IF_FALSE endLoop
            compile $ BodySuite while_body
            emitCodeArg JUMP_ABSOLUTE startLoop
            labelNextInstruction endLoop
            emitCodeNoArg POP_BLOCK
         _other -> error ("Unsupported statement " ++ show s) 
      compile $ BodySuite ss

compileGuards:: Word16 -> [(ExprSpan, [StatementSpan])] -> Compile ()
compileGuards _restLabel [] = return ()
compileGuards restLabel ((expr, stmt) : rest) = do
   compile expr
   falseLabel <- newLabel
   emitCodeArg POP_JUMP_IF_FALSE falseLabel
   compile $ BodySuite stmt
   emitCodeArg JUMP_FORWARD restLabel
   labelNextInstruction falseLabel 
   compileGuards restLabel rest

constantToPyObject :: ExprSpan -> PyObject
constantToPyObject (AST.Int {..}) = Blip.Int $ fromIntegral int_value
constantToPyObject (AST.Bool { bool_value = True }) = Blip.TrueObj
constantToPyObject (AST.Bool { bool_value = False }) = Blip.FalseObj
constantToPyObject (AST.None {}) = Blip.None
-- assumes all the tuple elements are constant
-- XXX what about tuples containig lists?
constantToPyObject (AST.Tuple {..}) =
   Blip.Tuple { elements = map constantToPyObject tuple_exprs }

instance Compilable ExprSpan where
   type CompileResult ExprSpan = ()
   compile (Var { var_ident = ident }) = do
      nameID <- compileName $ ident_string ident
      emitCodeArg LOAD_NAME nameID
   compile expr@(AST.Int {..}) =
      compileConstantEmit $ constantToPyObject expr
{-
   -- Float not yet defined in Blip
   compile (AST.Float {..}) =
      compileConstantEmit $ Blip.Float $ float_value
-}
   compile expr@(AST.Bool {..}) =
      compileConstantEmit $ constantToPyObject expr
   compile expr@(AST.None {}) =
      compileConstantEmit $ constantToPyObject expr
   compile (AST.Paren {..}) = compile paren_expr
   compile (AST.CondExpr {..}) = do
      compile ce_condition
      falseLabel <- newLabel
      emitCodeArg POP_JUMP_IF_FALSE falseLabel
      compile ce_true_branch
      restLabel <- newLabel
      emitCodeArg JUMP_FORWARD restLabel
      labelNextInstruction falseLabel 
      compile ce_false_branch
      labelNextInstruction restLabel
   compile expr@(AST.Tuple {..})
      | isPureExpr expr =
           compileConstantEmit $ constantToPyObject expr
      | otherwise = do
           mapM compile tuple_exprs
           emitCodeArg BUILD_TUPLE $ fromIntegral $ length tuple_exprs
   -- XXX Why not handle the constant case like Tuple?
   compile expr@(AST.List {..}) = do
      mapM compile list_exprs
      emitCodeArg BUILD_LIST $ fromIntegral $ length list_exprs
   compile (Yield { yield_expr = Nothing }) =
      compileConstantEmit Blip.None >> emitCodeNoArg YIELD_VALUE
   compile (Yield { yield_expr = Just expr }) =
      compile expr >> emitCodeNoArg YIELD_VALUE 
   compile (Call {..}) = do
      compile call_fun
      mapM compile call_args
      emitCodeArg CALL_FUNCTION $ fromIntegral $ length call_args
   compile (Subscript {..}) = do
      compile subscriptee
      compile subscript_expr
      emitCodeNoArg BINARY_SUBSCR
   compile exp@(BinaryOp {..})
      | isBoolean operator = compileBoolean exp
      | isComparison operator = compileComparison exp
      | otherwise = do 
           compile left_op_arg
           compile right_op_arg
           compileOp operator 
   compile other = error $ "unsupported expr: " ++ show other

instance Compilable ArgumentSpan where
   type CompileResult ArgumentSpan = ()
   compile (ArgExpr {..}) = compile arg_expr
   compile other = error $ "unsupported argument: " ++ show other

isBoolean :: OpSpan -> Bool
isBoolean (And {}) = True
isBoolean (Or {}) = True
isBoolean other = False

isComparison :: OpSpan -> Bool
isComparison (LessThan {}) = True
isComparison (GreaterThan {}) = True
isComparison (Equality {}) = True
isComparison (GreaterThanEquals {}) = True
isComparison (LessThanEquals {}) = True
isComparison (NotEquals  {}) = True
isComparison other = False

compileBoolean :: ExprSpan -> Compile ()
compileBoolean (BinaryOp {..}) = do
   endLabel <- newLabel
   compile left_op_arg
   case operator of
      And {..} -> emitCodeArg POP_JUMP_IF_FALSE endLabel
      Or {..} ->  emitCodeArg POP_JUMP_IF_TRUE endLabel
      other -> error $ "unexpected boolean operator: " ++ show other
   compile right_op_arg
   labelNextInstruction endLabel

compileOp :: OpSpan -> Compile ()
compileOp operator =
   emitCodeNoArg $ case operator of
      BinaryOr {} -> BINARY_OR
      Xor {} -> BINARY_XOR
      BinaryAnd {} -> BINARY_AND
      ShiftLeft {} -> BINARY_LSHIFT
      ShiftRight {} -> BINARY_RSHIFT
      Exponent {} -> BINARY_POWER
      Multiply {} -> BINARY_MULTIPLY
      Plus {} -> BINARY_ADD
      Minus {} -> BINARY_SUBTRACT
      Divide {} -> BINARY_TRUE_DIVIDE
      FloorDivide {} -> BINARY_FLOOR_DIVIDE
      Modulo {} -> BINARY_MODULO

{-
from object.h

#define Py_LT 0
#define Py_LE 1
#define Py_EQ 2
#define Py_NE 3
#define Py_GT 4
#define Py_GE 5
-}

compileComparison :: ExprSpan -> Compile ()
compileComparison (BinaryOp {..}) = do
   compile left_op_arg
   compile right_op_arg
   emitCodeArg COMPARE_OP $ comparisonOpCode operator
   where
   comparisonOpCode :: OpSpan -> Word16
   comparisonOpCode (LessThan {}) = 0 
   comparisonOpCode (LessThanEquals {}) = 1
   comparisonOpCode (Equality {}) = 2 
   comparisonOpCode (NotEquals {}) = 3 
   comparisonOpCode (GreaterThan {}) = 4 
   comparisonOpCode (GreaterThanEquals {}) = 5 
   comparisonOpCode other = error $ "Unexpected comparison operator: " ++ show operator
 
-- True if evaluating an expression has no observable side effect
-- Raising an exception is a side-effect, so variables are not pure.
isPureExpr :: ExprSpan -> Bool
isPureExpr (AST.Int {}) = True
isPureExpr (AST.LongInt {}) = True
isPureExpr (AST.Float {}) = True
isPureExpr (AST.Imaginary {}) = True
isPureExpr (AST.Bool {}) = True
isPureExpr (AST.None {}) = True
isPureExpr (AST.ByteStrings {}) = True
isPureExpr (AST.Strings {}) = True
isPureExpr (AST.UnicodeStrings {}) = True
isPureExpr (AST.Tuple { tuple_exprs = exprs }) = all isPureExpr exprs 
isPureExpr (AST.List { list_exprs = exprs }) = all isPureExpr exprs 
isPureExpr (AST.Set { set_exprs = exprs }) = all isPureExpr exprs 
isPureExpr (AST.Paren { paren_expr = expr }) = isPureExpr expr
isPureExpr (AST.Dictionary { dict_mappings = mappings }) =
   all (\(e1, e2) -> isPureExpr e1 && isPureExpr e2) mappings
-- XXX what about Lambda?
isPureExpr other = False

makeObject :: NameMap -> ConstantMap -> [Bytecode] -> Word32 -> Compile PyObject
makeObject names constants code maxStackDepth = do
   if maxStackDepth > maxBound
      -- XXX make a better error message
      then error "Maximum stack depth exceeded"
      else do
         pyFileName <- getFileName
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
                   , filename = Unicode pyFileName
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

returnNone :: Compile ()
returnNone = compileConstantEmit Blip.None >> emitCodeNoArg RETURN_VALUE
