{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances,
    PatternGuards, RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Compile
-- Copyright   : (c) 2012, 2013 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Compilation of Python 3 source code into bytecode.
-- 
-- Basic algorithm:
--
-- 1) Parse the source code into an AST.
-- 2) Compute the scope of all variables in the module
--    (one pass over the AST).
-- 3) Compile the AST for the whole module into a (possibly nested)
--    code object (one pass over the AST).
-- 4) Write the code object to a .pyc file.
--
-- The following Python constructs are compiled into code objects:
--    - The top-level of the module.
--    - Function definitions (def and lambda).
--    - Class definitions.
--    - Generators (not yet implemented).
--
-- The statements and expressions in each of the above constructs are
-- recursively compiled into bytecode instructions. Initially, the actual
-- addresses of jump instruction targets are not known. Instead the jump
-- targets are just labels. At the end of the compilation of each
-- construct the labelled instructions are converted into jumps to
-- actual addresses (one pass over the bytecode stream).
-- Also the maximum stack size of each code object is computed (one pass
-- over the bytecode stream).
--
-- We currently make no attempt to optimise the generated code.
--
-- Bytecode is generated directly from the AST, there is no intermediate
-- language, and no explict control-flow graph.
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
   , getObjectName, setObjectName, getGlobals
   , getNestedScope, ifDump, emptyDefinitionScope, lookupNestedScope
   , indexedVarSetKeys, lookupVar, emitReadVar, emitWriteVar
   , lookupGlobalVar, lookupClosureVar )
import Assemble (assemble)
import Monad (Compile (..), runCompileMonad)
import Types
   ( Identifier, CompileConfig (..)
   , ConstantID, CompileState (..), BlockState (..)
   , AnnotatedCode (..), Dumpable (..), IndexedVarSet, VarInfo (..)
   , ScopeIdentifier (..), DefinitionScope (..) )
import Scope (topScope, renderScope)
import Blip.Marshal as Blip (writePyc, PycFile (..), PyObject (..))
import Blip.Bytecode (Bytecode (..), BytecodeArg (..), Opcode (..), encode)
import Language.Python.Version3.Parser (parseModule)
import Language.Python.Common.AST as AST
   ( ModuleSpan (..), Module (..), StatementSpan (..), Statement (..)
   , ExprSpan (..), Expr (..), Ident (..), ArgumentSpan (..), Argument (..)
   , OpSpan, Op (..), SuiteSpan, Handler (..), HandlerSpan, ExceptClause (..)
   , ExceptClauseSpan, ImportItem (..), ImportItemSpan, ImportRelative (..)
   , ImportRelativeSpan, FromItems (..), FromItemsSpan, FromItem (..)
   , FromItemSpan, DecoratorSpan, Decorator (..), ComprehensionSpan
   , Comprehension (..), SliceSpan, Slice (..), AssignOpSpan, AssignOp (..)
   , ParameterSpan, Parameter (..))
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
import Data.List (sort, intersperse)
import Control.Monad (unless, forM_, when, replicateM_, foldM)
import Control.Exception (try)
import System.IO.Error (IOError, userError, ioError)
import Control.Monad.Trans (liftIO)
import Data.Bits ((.|.), shiftL)

-- Compile Python source code to bytecode and write the
-- result out to a .pyc file. The name of the output
-- file is based on the name of the input file. For example
-- the input 'foo.py' will result in an output file called 'foo.pyc'.

compileFile :: CompileConfig -- Configuration options
            -> FilePath      -- The file path of the input Python source
            -> IO ()
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

-- Parse the Python source into an AST, check for any syntax errors.
parseAndCheckErrors :: String -> FilePath -> IO ModuleSpan
parseAndCheckErrors fileContents sourceName =
   case parseModule fileContents sourceName of
      Left e -> error $ "parse error: " ++ prettyText e
      Right (pyModule, _comments) -> return pyModule

compileModule :: CompileState   -- initial compiler state
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

compiler :: Compilable a => a -> CompileState -> IO (CompileResult a)
compiler = runCompileMonad . compile

class Compilable a where
   type CompileResult a :: *
   compile :: a -> Compile (CompileResult a)

instance Compilable a => Compilable [a] where
   type CompileResult [a] = [CompileResult a]
   compile = mapM compile

instance Compilable ModuleSpan where
   type CompileResult ModuleSpan = PyObject
   compile (Module stmts) = do
      maybeDumpScope 
      setObjectName "<module>"
      compileClassModuleDocString stmts
      compile $ Body stmts

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

-- Build an object from all the state computed during compilation, such
-- as the bytecode sequence, variable information and so on.
makeObject :: Compile PyObject
makeObject = do
   annotatedCode <- getBlockState state_instructions
   let stackDepth = maxStackDepth annotatedCode
   names <- getBlockState state_names
   constants <- getBlockState state_constants
   freeVars <- getBlockState state_freeVars
   cellVars <- getBlockState state_cellVars
   localVars <- getBlockState state_locals
   argcount <- getBlockState state_argcount
   let code = map annotatedCode_bytecode annotatedCode 
       localVarNames = map Unicode $ indexedVarSetKeys localVars
       maxStackDepth = maxBound 
   if stackDepth > maxStackDepth
      -- XXX make a better error message
      then error $ "Maximum stack depth " ++ show maxStackDepth ++
                   " exceeded: " ++ show stackDepth
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

instance Compilable StatementSpan where
   type CompileResult StatementSpan = ()
   compile (Assign {..}) = do
      compile assign_expr
      compileAssignments assign_to
   compile (AugmentedAssign {..}) =
      case aug_assign_to of
         Var {..} -> do
            varInfo <- lookupVar $ ident_string var_ident
            emitReadVar varInfo
            compile aug_assign_expr
            compile aug_assign_op
            emitWriteVar varInfo
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
   compile (For {..}) = do
      startLoop <- newLabel
      endLoop <- newLabel
      anchor <- newLabel
      emitCodeArg SETUP_LOOP endLoop
      compile for_generator
      emitCodeNoArg GET_ITER
      labelNextInstruction startLoop
      emitCodeArg FOR_ITER anchor
      let num_targets = length for_targets
      when (num_targets > 1) $ do
         emitCodeArg UNPACK_SEQUENCE $ fromIntegral num_targets
      mapM_ compileAssignTo for_targets 
      compile for_body 
      emitCodeArg JUMP_ABSOLUTE startLoop
      labelNextInstruction anchor
      emitCodeNoArg POP_BLOCK
      compile for_else
      labelNextInstruction endLoop
   compile stmt@(Fun {..}) = compileFun stmt []
   compile stmt@(Class {..}) = compileClass stmt []
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
   -- XXX not complete
   compile (Try {..}) = do
      firstHandler <- newLabel
      emitCodeArg SETUP_EXCEPT firstHandler
      compile try_body
      emitCodeNoArg POP_BLOCK
      end <- newLabel
      emitCodeArg JUMP_FORWARD end
      compileHandlers end firstHandler try_excepts
      labelNextInstruction end
   compile (Import {..}) = mapM_ compile import_items
   -- XXX need to handle from __future__ 
   compile (FromImport {..}) = do
      let level = 0 -- XXX this should be the level of nesting
      compileConstantEmit $ Blip.Int level
      let names = fromItemsIdentifiers from_items 
          namesTuple = Blip.Tuple $ map Unicode names
      compileConstantEmit namesTuple
      compileFromModule from_module
      case from_items of
         ImportEverything {} -> do
            emitCodeNoArg IMPORT_STAR
         FromItems {..} -> do
            forM_ from_items_items $ \FromItem {..} -> do
                GlobalVar index <- lookupGlobalVar $ ident_string from_item_name
                emitCodeArg IMPORT_FROM index
                let storeName = case from_as_name of
                       Nothing -> from_item_name
                       Just asName -> asName
                varInfo <- lookupVar $ ident_string storeName
                emitWriteVar varInfo
            emitCodeNoArg POP_TOP
   -- XXX should check that we are inside a loop
   compile (Break {}) = emitCodeNoArg BREAK_LOOP
   compile (NonLocal {}) = return ()
   compile (Global {}) = return ()
   compile (Decorated {..}) =
      case decorated_def of
         Fun {} -> compileFun decorated_def decorated_decorators
         Class {} -> compileClass decorated_def decorated_decorators
         other -> error $ "Decorated statement is not a function or a class: " ++ prettyText other
   compile other = error $ "Unsupported statement:\n" ++ prettyText other

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
   compile expr@(AST.Imaginary {}) =
      compileConstantEmit $ constantToPyObject expr
   compile expr@(AST.Bool {}) =
      compileConstantEmit $ constantToPyObject expr
   compile expr@(AST.None {}) =
      compileConstantEmit $ constantToPyObject expr
   compile expr@(AST.Ellipsis {}) =
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
   compile (ListComp {..}) =
      compile list_comprehension
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
   -- XXX should handle keyword arguments etc.
   compile (Call {..}) = do
      compile call_fun
      -- mapM compile call_args
      (numPosArgs, numKeyWordArgs) <- compileCallArgs call_args
      let opArg = numPosArgs .|. numKeyWordArgs `shiftL` 8
      emitCodeArg CALL_FUNCTION opArg 
   compile (Subscript {..}) = do
      compile subscriptee
      compile subscript_expr
      emitCodeNoArg BINARY_SUBSCR
   compile (SlicedExpr {..}) = do
      compile slicee
      compileSlices slices
      emitCodeNoArg BINARY_SUBSCR
   -- XXX need to support operator chaining.
   compile exp@(BinaryOp {..})
      | isBoolean operator = compileBoolean exp
      | isComparison operator = compileComparison exp
      | isDot operator = compileDot exp 
      | otherwise = do 
           compile left_op_arg
           compile right_op_arg
           compileOp operator 
   compile (UnaryOp {..}) = do
      compile op_arg
      compileUnaryOp operator
   compile (Lambda {..}) = do
      funBodyObj <- nestedBlock expr_annot $ do
         -- make the first constant None, to indicate no doc string
         -- for the lambda
         compileConstant Blip.None
         compile lambda_body
         emitCodeNoArg RETURN_VALUE
         assemble
         makeObject
      compileClosure "<lambda>" funBodyObj 0
   compile other = error $ "Unsupported expr:\n" ++ prettyText other

instance Compilable AssignOpSpan where
   type CompileResult AssignOpSpan = ()
   compile = emitCodeNoArg . assignOpCode

instance Compilable DecoratorSpan where
   type CompileResult DecoratorSpan = ()
   compile dec@(Decorator {..}) = do
      compileDottedName decorator_name
      let numDecorators = length decorator_args
      when (numDecorators > 0) $ do
          mapM_ compile decorator_args
          emitCodeArg CALL_FUNCTION $ fromIntegral $ length decorator_args 
      where
      compileDottedName (name:rest) = do
         varInfo <- lookupVar $ ident_string name
         emitReadVar varInfo
         forM_ rest $ \var -> do
            GlobalVar index <- lookupGlobalVar $ ident_string var
            emitCodeArg LOAD_ATTR index
      compileDottedName [] =
         error $ "decorator with no name: " ++ prettyText dec

instance Show e => Compilable (ComprehensionSpan e) where
   type CompileResult (ComprehensionSpan e) = ()
   compile s = error $ "compile comprehension incomplete " ++ show s

instance Compilable ArgumentSpan where
   type CompileResult ArgumentSpan = ()
   compile (ArgExpr {..}) = compile arg_expr
   compile other = error $ "Unsupported argument:\n" ++ prettyText other

instance Compilable ImportItemSpan where
   type CompileResult ImportItemSpan = ()
   compile (ImportItem {..}) = do
      compileConstantEmit $ Blip.Int 0 -- this always seems to be zero
      compileConstantEmit Blip.None
      let dottedNames = map ident_string import_item_name
      -- assert (length dottedNames > 0)
      let dottedNameStr =
             concat $ intersperse "." dottedNames
      GlobalVar index <- lookupGlobalVar dottedNameStr
      emitCodeArg IMPORT_NAME index
      storeName <- 
         case import_as_name of
            Nothing -> return $ head import_item_name
            Just asName -> do
               forM_ (tail dottedNames) $ \attribute -> do
                  GlobalVar index <- lookupGlobalVar attribute
                  emitCodeArg LOAD_ATTR index 
               return asName
      varInfo <- lookupVar $ ident_string storeName
      emitWriteVar varInfo

withDecorators :: [DecoratorSpan] -> Compile () -> Compile ()
withDecorators decorators comp = do
   -- push each of the decorators on the stack
   compile decorators
   -- run the enclosed computation
   comp
   -- call each of the decorators
   replicateM_ (length decorators) $ 
      emitCodeArg CALL_FUNCTION 1

nestedBlock :: ScopeIdentifier -> Compile a -> Compile a
nestedBlock scopeIdent comp = do
   -- save the current block state
   oldBlockState <- getBlockState id
   -- set the new block state to initial values, and the
   -- scope of the current definition
   (name, definitionScope, nestedScope) <- lookupNestedScope scopeIdent 
   setBlockState $ initBlockState definitionScope nestedScope
   -- set the new object name
   setObjectName name
   -- run the nested computation
   result <- comp
   -- restore the original block state
   setBlockState oldBlockState
   return result

-- Compile a function definition, possibly with decorators.
compileFun :: StatementSpan -> [DecoratorSpan] -> Compile ()
compileFun (Fun {..}) decorators = do
   let funName = ident_string $ fun_name
   withDecorators decorators $ do
      funBodyObj <- nestedBlock stmt_annot $ do
         compileFunDocString fun_body
         compile $ Body fun_body
      numDefaults <- compileDefaultParams fun_args
      compileClosure funName funBodyObj numDefaults
   varInfo <- lookupVar funName
   emitWriteVar varInfo

-- Compile a class definition, possibly with decorators.
compileClass :: StatementSpan -> [DecoratorSpan] -> Compile ()
compileClass (Class {..}) decorators = do
   let className = ident_string $ class_name
   withDecorators decorators $ do
      classBodyObj <- nestedBlock stmt_annot $ do
         emitCodeArg LOAD_FAST 0
         emitCodeNoArg STORE_LOCALS
         varInfo <- lookupGlobalVar "__name__"
         emitReadVar varInfo
         varInfo <- lookupGlobalVar "__module__"
         emitWriteVar varInfo
         compileConstantEmit $ Unicode className
         varInfo <- lookupGlobalVar "__qualname__"
         emitWriteVar varInfo
         compileClassModuleDocString class_body
         compile $ Body class_body
      emitCodeNoArg LOAD_BUILD_CLASS
      compileClosure className classBodyObj 0
      compileConstantEmit $ Unicode className
      mapM compile class_args
      emitCodeArg CALL_FUNCTION (2 + (fromIntegral $ length class_args))
   varInfo <- lookupVar className
   emitWriteVar varInfo

-- XXX CPython uses a "qualified" name for the code object. For instance
-- nested functions look like "f.<locals>.g", whereas we currently use
-- just "g".

-- The free variables in a code object will either be cell variables
-- or free variables in the enclosing object. If there are no free
-- variables then we can avoid building the closure, and just make the function.
compileClosure :: String -> PyObject -> Word16 -> Compile ()
compileClosure name obj numDefaults = do
   -- get the list of free variables from the code object
   let Blip.Tuple freeVarStringObjs = freevars obj
       freeVarIdentifiers = map unicode freeVarStringObjs
       numFreeVars = length freeVarIdentifiers
   if numFreeVars == 0
      then do
         compileConstantEmit obj 
         compileConstantEmit $ Unicode name
         emitCodeArg MAKE_FUNCTION numDefaults  
      else do
         forM_ freeVarIdentifiers $ \var -> do
            maybeVarInfo <- lookupClosureVar var
            -- we don't use emitReadVar because it would generate
            -- LOAD_DEREF instructions, but we want LOAD_CLOSURE
            -- instead.
            case maybeVarInfo of
               Just (CellVar index) -> emitCodeArg LOAD_CLOSURE index
               Just (FreeVar index) -> emitCodeArg LOAD_CLOSURE index
               Nothing -> error "closure free variable not cell or free var in outer context"
         emitCodeArg BUILD_TUPLE $ fromIntegral numFreeVars
         compileConstantEmit obj 
         compileConstantEmit $ Unicode name
         emitCodeArg MAKE_CLOSURE numDefaults

-- Compile default parameters and return how many there are
compileDefaultParams :: [ParameterSpan] -> Compile Word16
compileDefaultParams = foldM compileParam 0
   where
   compileParam :: Word16 -> ParameterSpan -> Compile Word16
   compileParam count (Param {..}) = do
      case param_default of
         Nothing -> return count
         Just expr -> do
            compile expr
            return $ count + 1
   compileParam count _other = return count

-- Compile a sequence of exception handlers
compileHandlers :: Word16 -> Word16 -> [HandlerSpan] -> Compile ()
compileHandlers end handlerLabel [] = do
   labelNextInstruction handlerLabel
   emitCodeNoArg END_FINALLY
compileHandlers end handlerLabel (Handler {..} : rest) = do
   labelNextInstruction handlerLabel
   nextLabel <- newLabel 
   compileHandlerClause nextLabel handler_clause
   emitCodeNoArg POP_TOP
   compile handler_suite
   emitCodeArg JUMP_FORWARD end
   compileHandlers end nextLabel rest 

-- Compile a 'from module import'.
compileFromModule :: ImportRelativeSpan -> Compile ()
-- XXX what to do about the initial dots?
compileFromModule (ImportRelative {..}) = do
   let moduleName =
          case import_relative_module of
             Nothing -> ""
             Just dottedNames ->
                concat $ intersperse "." $ map ident_string dottedNames
   GlobalVar index <- lookupGlobalVar moduleName 
   emitCodeArg IMPORT_NAME index

fromItemsIdentifiers :: FromItemsSpan -> [Identifier]
fromItemsIdentifiers (ImportEverything {}) = ["*"]
fromItemsIdentifiers (FromItems {..}) =
   map fromItemIdentifier from_items_items
   where
   fromItemIdentifier :: FromItemSpan -> Identifier
   fromItemIdentifier (FromItem {..}) = ident_string $ from_item_name

compileHandlerClause :: Word16 -> ExceptClauseSpan -> Compile ()
compileHandlerClause nextHandler (ExceptClause {..}) = do
   case except_clause of
      Nothing -> emitCodeNoArg POP_TOP >> emitCodeNoArg POP_TOP
      Just (target, asExpr) -> do
         emitCodeNoArg DUP_TOP
         compile target
         emitCodeArg COMPARE_OP exactMatchOp
         emitCodeArg POP_JUMP_IF_FALSE nextHandler
         emitCodeNoArg POP_TOP
         case asExpr of
            Nothing -> emitCodeNoArg POP_TOP
            Just expr -> compileAssignTo expr
   where
   -- The code for an exact match operator.
   exactMatchOp :: Word16
   exactMatchOp = 10

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
compileAssignTo (AST.List {..}) = do
   emitCodeArg UNPACK_SEQUENCE $ fromIntegral $ length list_exprs
   mapM_ compileAssignTo list_exprs
compileAssignTo (AST.Paren {..}) = compileAssignTo paren_expr
compileAssignTo expr@(BinaryOp { operator = Dot {}, right_op_arg = Var {..}}) = do
   compile $ left_op_arg expr
   GlobalVar index <- lookupGlobalVar $ ident_string $ var_ident
   emitCodeArg STORE_ATTR index
compileAssignTo other = error $ "assignment to unexpected expression:\n" ++ prettyText other

-- Check for a docstring in the first statement of a function body.
-- The first constant in the corresponding code object is inspected
-- by the interpreter for the docstring. If there is no docstring
-- then the first constant must be None
compileFunDocString :: [StatementSpan] -> Compile ()
compileFunDocString (firstStmt:_stmts)
   | StmtExpr {..} <- firstStmt,
     Strings {} <- stmt_expr
        = do compileConstant $ constantToPyObject stmt_expr
             return ()
   | otherwise = compileConstant Blip.None >> return ()
compileFunDocString [] = compileConstant Blip.None >> return ()

compileClassModuleDocString :: [StatementSpan] -> Compile ()
compileClassModuleDocString (firstStmt:_stmts)
   | StmtExpr {..} <- firstStmt,
     Strings {} <- stmt_expr
        = do compileConstantEmit $ constantToPyObject stmt_expr
             varInfo <- lookupGlobalVar "__doc__"
             emitWriteVar varInfo
   | otherwise = return ()
compileClassModuleDocString [] = return ()

-- Compile a conditional guard
compileGuard :: Word16 -> (ExprSpan, [StatementSpan]) -> Compile ()
compileGuard restLabel (expr, stmts) = do
   compile expr
   falseLabel <- newLabel
   emitCodeArg POP_JUMP_IF_FALSE falseLabel
   compile stmts
   emitCodeArg JUMP_FORWARD restLabel
   labelNextInstruction falseLabel 

-- Convert a constant expression into the equivalent object. This
-- only works for expressions which have a counterpart in the object
-- representation used in .pyc files.
constantToPyObject :: ExprSpan -> PyObject
constantToPyObject (AST.Int {..}) = Blip.Int $ fromIntegral int_value
constantToPyObject (AST.Float {..}) = Blip.Float $ float_value 
-- XXX we could optimise the case where we have 'float + imaginary j',
-- to generate a Complex number directly, rather than by doing
-- the addition operation.
constantToPyObject (AST.Imaginary {..}) =
   Blip.Complex { real = 0.0, imaginary = imaginary_value }
constantToPyObject (AST.Bool { bool_value = True }) = Blip.TrueObj
constantToPyObject (AST.Bool { bool_value = False }) = Blip.FalseObj
constantToPyObject (AST.None {}) = Blip.None
constantToPyObject (AST.Ellipsis {}) = Blip.Ellipsis
-- assumes all the tuple elements are constant
-- XXX what about tuples containig lists?
constantToPyObject (AST.Tuple {..}) =
   Blip.Tuple { elements = map constantToPyObject tuple_exprs }
constantToPyObject (AST.Strings {..}) =
   Blip.Unicode { unicode = concat $ map stripQuotes strings_strings }
   where
   -- The strings in the AST retain their original quote marks which
   -- need to be removed, we have to remove single or triple quotes.
   -- We assume the parser has correctly matched the quotes.
   -- XXX this is ugly, perhaps we should fix the parser to not
   -- include the quotes.
   stripQuotes :: String -> String
   stripQuotes ('\'':'\'':'\'':rest) = take (length rest - 3) rest
   stripQuotes ('"':'"':'"':rest) = take (length rest - 3) rest
   stripQuotes ('\'':rest) = init rest
   stripQuotes ('"':rest) = init rest
   stripQuotes other = error $ "bad literal string: " ++ other

-- Compile the arguments to a function call and return the number
-- of positional arguments, and the number of keyword arguments.
compileCallArgs :: [ArgumentSpan] -> Compile (Word16, Word16)
compileCallArgs = foldM compileArg (0, 0)
   where
   compileArg :: (Word16, Word16) -> ArgumentSpan -> Compile (Word16, Word16)
   compileArg (posArgs, kwArgs) (ArgExpr {..}) = do
      compile arg_expr
      return (posArgs + 1, kwArgs)
   compileArg (posArgs, kwArgs) (ArgKeyword {..}) = do
      compileConstantEmit $ Unicode $ ident_string arg_keyword
      compile arg_expr
      return (posArgs, kwArgs + 1)
   compileArg _count other =
      error $ "unsupported argument: " ++ prettyText other

-- XXX need to handle extended slices, slice expressions and ellipsis
compileSlices :: [SliceSpan] -> Compile ()
compileSlices [SliceProper {..}] = do
   case slice_lower of
      Nothing -> compileConstantEmit Blip.None
      Just expr -> compile expr
   case slice_upper of
      Nothing -> compileConstantEmit Blip.None
      Just expr -> compile expr
   case slice_stride of
      Nothing -> emitCodeArg BUILD_SLICE 2
      -- Not sure about this, maybe it is None
      Just Nothing -> emitCodeArg BUILD_SLICE 2
      Just (Just expr) -> do
         compile expr
         emitCodeArg BUILD_SLICE 3
compileSlices other = error $ "unsupported slice: " ++ show other

-- Return the opcode for a given assignment operator.
assignOpCode :: AssignOpSpan -> Opcode
assignOpCode assign = 
   case assign of
      PlusAssign {} -> INPLACE_ADD
      MinusAssign {} -> INPLACE_SUBTRACT
      MultAssign {} -> INPLACE_MULTIPLY
      DivAssign {} -> INPLACE_TRUE_DIVIDE
      ModAssign {} -> INPLACE_MODULO
      PowAssign {} -> INPLACE_POWER
      BinAndAssign {} -> INPLACE_AND
      BinOrAssign {} -> INPLACE_OR
      BinXorAssign {} -> INPLACE_XOR
      LeftShiftAssign {} -> INPLACE_LSHIFT
      RightShiftAssign {} -> INPLACE_RSHIFT
      FloorDivAssign {} -> INPLACE_FLOOR_DIVIDE

isDot :: OpSpan -> Bool
isDot (Dot {}) = True
isDot _other = False

isBoolean :: OpSpan -> Bool
isBoolean (And {}) = True
isBoolean (Or {}) = True
isBoolean _other = False

isComparison :: OpSpan -> Bool
isComparison (LessThan {}) = True
isComparison (GreaterThan {}) = True
isComparison (Equality {}) = True
isComparison (GreaterThanEquals {}) = True
isComparison (LessThanEquals {}) = True
isComparison (NotEquals  {}) = True
isComparison other = False

compileDot :: ExprSpan -> Compile ()
compileDot (BinaryOp {..}) = do
   compile left_op_arg
   case right_op_arg of
      Var {..} -> do
         -- the right argument should be treated like a global variable
         GlobalVar varInfo <- lookupGlobalVar $ ident_string var_ident
         emitCodeArg LOAD_ATTR varInfo 
      other -> error $ "right argument of dot operator not a variable:\n" ++ prettyText other

compileBoolean :: ExprSpan -> Compile ()
compileBoolean (BinaryOp {..}) = do
   endLabel <- newLabel
   compile left_op_arg
   case operator of
      And {..} -> emitCodeArg JUMP_IF_FALSE_OR_POP endLabel
      Or {..} ->  emitCodeArg JUMP_IF_TRUE_OR_POP endLabel
      other -> error $ "Unexpected boolean operator:\n" ++ prettyText other
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
      other -> error $ "Unexpected operator:\n" ++ prettyText operator

compileUnaryOp :: OpSpan -> Compile ()
compileUnaryOp operator =
   emitCodeNoArg $ case operator of
      Minus {} -> UNARY_NEGATIVE
      Plus {} -> UNARY_POSITIVE
      Not {} -> UNARY_NOT
      Invert {} -> UNARY_INVERT

{-
from object.h

#define Py_LT 0
#define Py_LE 1
#define Py_EQ 2
#define Py_NE 3
#define Py_GT 4
#define Py_GE 5

and from opcode.h 

enum cmp_op {PyCmp_LT=Py_LT, PyCmp_LE=Py_LE, PyCmp_EQ=Py_EQ, PyCmp_NE=Py_NE, PyCmp_GT=Py_GT, PyCmp_GE=Py_GE,
             PyCmp_IN, PyCmp_NOT_IN, PyCmp_IS, PyCmp_IS_NOT, PyCmp_EXC_MATCH, PyCmp_BAD};
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
   comparisonOpCode (In {}) = 6
   comparisonOpCode (NotIn {}) = 7
   comparisonOpCode (Is {}) = 8
   comparisonOpCode (IsNot {}) = 9
   -- XXX we don't appear to have an exact match operator in the AST
   comparisonOpCode other = error $ "Unexpected comparison operator:\n" ++ prettyText operator
 
-- Emit an instruction that returns the None contant.
returnNone :: Compile ()
returnNone = compileConstantEmit Blip.None >> emitCodeNoArg RETURN_VALUE

-- Print out the variable scope of the module if requested on the command line.
maybeDumpScope :: Compile ()
maybeDumpScope = do
   ifDump DumpScope $ do
      globals <- getGlobals
      nestedScope <- getNestedScope
      liftIO $ putStrLn "Variable Scope:"
      liftIO $ putStrLn $ renderScope (globals, nestedScope)
