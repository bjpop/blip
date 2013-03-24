{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances,
    PatternGuards, RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Main
-- Copyright   : (c) 2012, 2013 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Compilation of Python 3 source code into bytecode.
--
-----------------------------------------------------------------------------

module Compile (compileFile) where

import Prelude hiding (mapM)
import Utils (isPureExpr, isPyObjectExpr)
import StackDepth (maxStackDepth)
import ProgName (progName)
import State
   ( setBlockState, getBlockState, initBlockState, initState
   , emitCodeNoArg, emitCodeArg, compileConstantEmit
   , compileConstant, getFileName, newLabel, labelNextInstruction
   , getObjectName, setObjectName, getLastInstruction, getGlobals
   , getNestedScope, ifDump, emptyDefinitionScope, lookupNestedScope
   , indexedVarSetKeys, lookupVar, emitReadVar, emitWriteVar
   , lookupGlobalVar )
import Assemble (assemble)
import Monad (Compile (..), runCompileMonad)
import Types
   ( Identifier, CompileConfig (..)
   , ConstantID, CompileState (..), BlockState (..)
   , AnnotatedCode (..), Dumpable (..), IndexedVarSet, VarInfo (..)
   , ScopeIdentifier (..) )
import Scope (topScope, renderScope)
import Blip.Marshal as Blip (writePyc, PycFile (..), PyObject (..))
import Blip.Bytecode (Bytecode (..), BytecodeArg (..), Opcode (..), encode)
import Language.Python.Version3.Parser (parseModule)
import Language.Python.Common.AST as AST
   ( ModuleSpan (..), Module (..), StatementSpan (..), Statement (..)
   , ExprSpan (..), Expr (..), Ident (..), ArgumentSpan (..), Argument (..)
   , OpSpan, Op (..), SuiteSpan)
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
import Control.Monad (unless, forM_, when)
import Control.Exception (try)
import System.IO.Error (IOError, userError, ioError)
import Control.Monad.Trans (liftIO)

compiler :: Compilable a => a -> CompileState -> IO (CompileResult a)
compiler = runCompileMonad . compile

class Compilable a where
   type CompileResult a :: *
   compile :: a -> Compile (CompileResult a)

instance Compilable a => Compilable [a] where
   type CompileResult [a] = [CompileResult a]
   compile = mapM compile

compileFile :: CompileConfig -> FilePath -> IO ()
compileFile config path = do
   r <- try $ do
      pyHandle <- openFile path ReadMode
      sizeInBytes <- hFileSize pyHandle
      fileContents <- hGetContents pyHandle
      modifiedTime <- getModificationTime path
      let modSeconds = case modifiedTime of TOD secs _picoSecs -> secs
      pyModule <- parseAndCheckErrors fileContents path
      let (globals, nestedScope) = topScope pyModule
      canonicalPath <- canonicalizePath path 
      let state = initState globals emptyDefinitionScope
                     nestedScope config canonicalPath
      pyc <- compileModule state (fromIntegral modSeconds)
                (fromIntegral sizeInBytes) pyModule
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

compileModule :: CompileState
              -> Word32         -- modification time
              -> Word32         -- size in bytes
              -> ModuleSpan     -- AST
              -> IO PycFile
compileModule state pyFileModifiedTime pyFileSizeBytes mod = do
   obj <- compiler mod state
   return $ PycFile
      { magic = compileConfig_magic $ state_config state
      , modified_time = pyFileModifiedTime 
      , size = pyFileSizeBytes
      , object = obj }

instance Compilable ModuleSpan where
   type CompileResult ModuleSpan = PyObject
   compile (Module stmts) = do
      maybeDumpScope 
      setObjectName "<module>"
      compile $ Body stmts

scopeIdentToObjectName :: ScopeIdentifier -> String
scopeIdentToObjectName (FunOrClassIdentifier ident) = ident
-- XXX check if this is a suitable name
scopeIdentToObjectName (LambdaIdentifier _srcSpan) = "<lambda>"

nestedBlock :: ScopeIdentifier -> Compile a -> Compile a
nestedBlock scopeIdent comp = do
   -- save the current block state
   oldBlockState <- getBlockState id
   -- set the new block state to initial values, and the
   -- scope of the current definition
   (definitionScope, nestedScope) <- lookupNestedScope scopeIdent 
   setBlockState $ initBlockState definitionScope nestedScope
   -- set the new object name
   setObjectName $ scopeIdentToObjectName scopeIdent 
   -- run the nested computation
   result <- comp
   -- restore the original block state
   setBlockState oldBlockState
   return result

-- body of module, function and class
newtype Body = Body [StatementSpan]

instance Compilable Body where
   type CompileResult Body = PyObject
   compile (Body stmts) = do
      compile stmts
      -- XXX we could avoid this 'return None' if all branches in the code
      -- ended with a return statement. Can fix this in an optimisation step
      -- with control flow analysis.
      returnNone
      assemble
      makeObject

instance Compilable StatementSpan where
   type CompileResult StatementSpan = ()
   compile (Assign {..}) = do
      compile assign_expr
      compileAssignments assign_to
   compile (Return { return_expr = Nothing }) = returnNone
   compile (Return { return_expr = Just expr }) =  
      compile expr >> emitCodeNoArg RETURN_VALUE
   compile (Pass {}) = return ()
   compile (StmtExpr {..}) = 
      unless (isPureExpr stmt_expr) $ 
         compile stmt_expr >> emitCodeNoArg POP_TOP
   compile (Conditional {..}) = do
      restLabel <- newLabel
      mapM_ (compileGuard restLabel) cond_guards 
      compile cond_else
      labelNextInstruction restLabel
   compile (While {..}) = do
      startLoop <- newLabel
      endLoop <- newLabel
      anchor <- newLabel
      emitCodeArg SETUP_LOOP endLoop
      labelNextInstruction startLoop
      compile while_cond
      emitCodeArg POP_JUMP_IF_FALSE anchor
      compile while_body
      emitCodeArg JUMP_ABSOLUTE startLoop
      labelNextInstruction anchor 
      emitCodeNoArg POP_BLOCK
      compile while_else
      labelNextInstruction endLoop
   compile (Fun {..}) = do
      let funName = ident_string $ fun_name
      varInfo <- lookupVar funName
      funBodyObj <- nestedBlock (FunOrClassIdentifier funName) $ do
         compileFunDocString fun_body
         compile $ Body fun_body
      compileConstantEmit funBodyObj
      compileConstantEmit $ Unicode funName
      emitCodeArg MAKE_FUNCTION 0 -- XXX need to figure out this arg
                                  -- appears to be related to keyword args, and defaults
      emitWriteVar varInfo
   -- XXX assertions appear to be turned off if the code is compiled
   -- for optimisation
   -- If the assertion expression is a tuple of non-zero length, then
   -- it is always True: CPython warns about this
   compile (Assert {..}) = do
      case assert_exprs of
         test_expr:restAssertExprs -> do
            compile test_expr
            end <- newLabel
            emitCodeArg POP_JUMP_IF_TRUE end
            GlobalVar assertionErrorVar <- lookupGlobalVar "AssertionError"
            emitCodeArg LOAD_GLOBAL assertionErrorVar
            case restAssertExprs of
               assertMsgExpr:_ -> do
                  compile assertMsgExpr
                  emitCodeArg CALL_FUNCTION 1
               _other -> return ()
            emitCodeArg RAISE_VARARGS 1
            labelNextInstruction end
         _other -> error "assert with no test"
              
   compile (NonLocal {}) = return ()
   compile (Global {}) = return ()
   compile other = error ("Unsupported statement " ++ show other) 

-- compile multiple possible assignments:
-- x = y = z = rhs
compileAssignments :: [ExprSpan] -> Compile ()
compileAssignments [] = return ()
compileAssignments [e] = compileAssignTo e
compileAssignments (e1:e2:rest) = do
   emitCodeNoArg DUP_TOP
   compileAssignTo e1
   compileAssignments (e2:rest)

-- the lhs of an assignment statement
-- we can assume that the parser has only accepted the appropriate
-- subset of expression types
compileAssignTo :: ExprSpan -> Compile ()
compileAssignTo (Var {..}) = do
   varInfo <- lookupVar $ ident_string var_ident 
   emitWriteVar varInfo
compileAssignTo (Subscript {..}) = do
   compile subscriptee
   compile subscript_expr
   emitCodeNoArg STORE_SUBSCR
-- XXX this can be optimised in places where the rhs is a
-- manifest list or tuple, avoiding the building list/tuple
-- only to deconstruct again
compileAssignTo (AST.Tuple {..}) = do
   emitCodeArg UNPACK_SEQUENCE $ fromIntegral $ length tuple_exprs
   mapM_ compileAssignTo tuple_exprs
compileAssignTo other = error $ "compileAssignTo unsupported: " ++ show other

-- Check for a docstring in the first statement of a function body.
-- The first constant in the corresponding code object is inspected
-- by the interpreter for the docstring. If their is no docstring
-- then the first constant must be None
compileFunDocString :: [StatementSpan] -> Compile ()
compileFunDocString (firstStmt:_stmts)
   | StmtExpr {..} <- firstStmt,
     Strings {} <- stmt_expr
        = do compileConstant $ constantToPyObject stmt_expr
             return ()
   | otherwise = compileConstant Blip.None >> return ()

{-
compileConditional :: Word16 -> [(ExprSpan, [StatementSpan])] -> [StatementSpan] -> Compile ()
-- final guard with no else clause
compileConditional restLabel [(expr, body)] [] = do
   compile expr
   emitCodeArg POP_JUMP_IF_FALSE restLabel
   compile body
   return ()
-- final guard with an non-empty else clause
compileConditional restLabel [(expr, body)] elseClause@(_:_) = do
   compile expr
   falseLabel <- newLabel
   emitCodeArg POP_JUMP_IF_FALSE falseLabel
   compile body
   emitCodeArg JUMP_FORWARD restLabel
   labelNextInstruction falseLabel 
   compile elseClause
   -- XXX this looks bogus, we might need to allow instructions
   -- to be multiply labelled.
   -- this next instruction is just to make sure the else part
   -- has some code in it, so it gets a label
   -- emitCodeArg JUMP_FORWARD restLabel
   labelNextInstruction restLabel
   return ()
-- non-final guard
compileConditional restLabel ((expr, body):guards) elseClause = do
   compile expr
   falseLabel <- newLabel
   emitCodeArg POP_JUMP_IF_FALSE restLabel
   compile body
   emitCodeArg JUMP_FORWARD restLabel
   labelNextInstruction falseLabel 
   compileConditional restLabel guards elseClause
-}

compileGuard :: Word16 -> (ExprSpan, [StatementSpan]) -> Compile ()
compileGuard restLabel (expr, stmts) = do
   compile expr
   falseLabel <- newLabel
   emitCodeArg POP_JUMP_IF_FALSE falseLabel
   compile stmts
   emitCodeArg JUMP_FORWARD restLabel
   labelNextInstruction falseLabel 

constantToPyObject :: ExprSpan -> PyObject
constantToPyObject (AST.Int {..}) = Blip.Int $ fromIntegral int_value
constantToPyObject (AST.Float {..}) = Blip.Float $ float_value 
constantToPyObject (AST.Bool { bool_value = True }) = Blip.TrueObj
constantToPyObject (AST.Bool { bool_value = False }) = Blip.FalseObj
constantToPyObject (AST.None {}) = Blip.None
-- assumes all the tuple elements are constant
-- XXX what about tuples containig lists?
constantToPyObject (AST.Tuple {..}) =
   Blip.Tuple { elements = map constantToPyObject tuple_exprs }
constantToPyObject (AST.Strings {..}) =
   Blip.Unicode { unicode = concat $ map stripQuotes strings_strings }
   where
   -- The strings in the AST retain their original quote marks which
   -- need to be removed.
   stripQuotes :: String -> String
   stripQuotes str
      | length str >= 2 = tail $ init str
      | otherwise = str

instance Compilable ExprSpan where
   type CompileResult ExprSpan = ()
   compile (Var { var_ident = ident }) = do
      varInfo <- lookupVar $ ident_string ident
      emitReadVar varInfo
   compile expr@(AST.Strings {}) =
      compileConstantEmit $ constantToPyObject expr 
   compile expr@(AST.Int {}) =
      compileConstantEmit $ constantToPyObject expr
   compile expr@(AST.Float {}) =
      compileConstantEmit $ constantToPyObject expr
   compile expr@(AST.Bool {}) =
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
      | isPyObjectExpr expr =
           compileConstantEmit $ constantToPyObject expr
      | otherwise = do
           mapM compile tuple_exprs
           emitCodeArg BUILD_TUPLE $ fromIntegral $ length tuple_exprs
   compile expr@(AST.List {..}) = do
      mapM compile list_exprs
      emitCodeArg BUILD_LIST $ fromIntegral $ length list_exprs
   compile expr@(AST.Set {..}) = do
      mapM compile set_exprs
      emitCodeArg BUILD_SET $ fromIntegral $ length set_exprs
   compile expr@(Dictionary {..}) = do
      emitCodeArg BUILD_MAP $ fromIntegral $ length dict_mappings
      forM_ dict_mappings $ \(key, value) -> do
         compile value
         compile key
         emitCodeNoArg STORE_MAP
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
   compile (Lambda {..}) = do
      funBodyObj <- nestedBlock (LambdaIdentifier expr_annot) $ do
         -- make the first constant None, to indicate no doc string
         -- for the lambda
         compileConstant Blip.None
         compile lambda_body
         emitCodeNoArg RETURN_VALUE
         assemble
         makeObject
      compileConstantEmit funBodyObj
      compileConstantEmit $ Unicode "<lambda>"
      emitCodeArg MAKE_FUNCTION 0 -- XXX need to figure out this arg
                                  -- appears to be related to keyword args, and defaults
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
 
makeObject :: Compile PyObject
makeObject = do
   stackDepth <- maxStackDepth 
   names <- getBlockState state_names
   constants <- getBlockState state_constants
   annotatedCode <- getBlockState state_instructions
   freeVars <- getBlockState state_freeVars
   cellVars <- getBlockState state_cellVars
   localVars <- getBlockState state_locals
   argcount <- getBlockState state_argcount
   let code = map annotatedCode_bytecode annotatedCode 
   let localVarNames = map Unicode $ indexedVarSetKeys localVars
   if stackDepth > maxBound
      -- XXX make a better error message
      then error "Maximum stack depth exceeded"
      else do
         pyFileName <- getFileName
         objectName <- getObjectName
         let obj = Code
                   { argcount = argcount
                   , kwonlyargcount = 0
                   , nlocals = fromIntegral $ length localVarNames
                   , stacksize = stackDepth 
                   , flags = 0
                   , code = String $ encode code
                   , consts = makeConstants constants
                   , names = makeNames names
                   , varnames = Blip.Tuple localVarNames
                   , freevars = makeVarSetTuple freeVars
                   , cellvars = makeVarSetTuple cellVars
                   , filename = Unicode pyFileName
                   , name = Unicode objectName
                   , firstlineno = 0
                   , lnotab = String B.empty
                   }
         return obj
   where
   makeVarSetTuple :: IndexedVarSet -> PyObject
   makeVarSetTuple varSet =
      Blip.Tuple $ map Unicode $ indexedVarSetKeys varSet

makeConstants :: [PyObject] -> PyObject
makeConstants = Blip.Tuple . reverse

makeNames :: [Identifier] -> PyObject
makeNames = Blip.Tuple . map Unicode . reverse 

returnNone :: Compile ()
returnNone = compileConstantEmit Blip.None >> emitCodeNoArg RETURN_VALUE

maybeDumpScope :: Compile ()
maybeDumpScope = do
   ifDump DumpScope $ do
      globals <- getGlobals
      nestedScope <- getNestedScope
      liftIO $ putStrLn "Variable Scope:"
      liftIO $ putStrLn $ renderScope (globals, nestedScope)
