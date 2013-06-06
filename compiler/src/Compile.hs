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
--    - Comprehensions.
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
import Desugar (desugarComprehension, desugarWith, resultName)
import Utils 
   ( isPureExpr, isPyObjectExpr, mkAssignVar, mkList
   , mkVar, mkMethodCall, mkStmtExpr, mkSet, mkDict, mkAssign
   , mkSubscript, mkReturn, mkYield, spanToScopeIdentifier )
import StackDepth (maxStackDepth)
import ProgName (progName)
import State
   ( setBlockState, getBlockState, initBlockState, initState
   , emitCodeNoArg, emitCodeArg, compileConstantEmit
   , compileConstant, getFileName, newLabel, labelNextInstruction
   , getObjectName, setObjectName
   , getNestedScope, ifDump, getLocalScope
   , indexedVarSetKeys, emitReadVar, emitWriteVar, emitDeleteVar
   , lookupNameVar, lookupClosureVar, setFlag
   , peekFrameBlock, withFrameBlock, setFastLocals, setArgCount
   , setLineNumber )
import Assemble (assemble)
import Monad (Compile (..), runCompileMonad)
import Types
   ( Identifier, CompileConfig (..)
   , CompileState (..), BlockState (..)
   , AnnotatedCode (..), Dumpable (..), IndexedVarSet, VarInfo (..)
   , FrameBlockInfo (..), Context (..), ParameterTypes (..), LocalScope (..) )
import Scope (topScope, renderScope)
import Blip.Marshal as Blip
   ( writePyc, PycFile (..), PyObject (..), co_generator )
import Blip.Bytecode (Opcode (..), encode)
import Language.Python.Version3.Parser (parseModule)
import Language.Python.Common.AST as AST
   ( Annotated (..), ModuleSpan, Module (..), StatementSpan, Statement (..)
   , ExprSpan, Expr (..), Ident (..), ArgumentSpan, Argument (..)
   , OpSpan, Op (..), Handler (..), HandlerSpan, ExceptClause (..)
   , ExceptClauseSpan, ImportItem (..), ImportItemSpan, ImportRelative (..)
   , ImportRelativeSpan, FromItems (..), FromItemsSpan, FromItem (..)
   , FromItemSpan, DecoratorSpan, Decorator (..), ComprehensionSpan
   , Comprehension (..), SliceSpan, Slice (..), AssignOpSpan, AssignOp (..)
   , ParameterSpan, Parameter (..), RaiseExpr (..), RaiseExprSpan )
import Language.Python.Common (prettyText)
import Language.Python.Common.StringEscape (unescapeString)
import Language.Python.Common.SrcLocation (SrcSpan (..))
import System.FilePath ((<.>), takeBaseName)
-- XXX Commented out to avoid bug in unix package when building on OS X, 
-- The unix package is depended on by the directory package.
-- import System.Directory (getModificationTime, canonicalizePath)
-- import System.Time (ClockTime (..))
import System.IO (openFile, IOMode(..), hClose, hFileSize, hGetContents)
import Data.Word (Word32, Word16)
import Data.Traversable as Traversable (mapM)
import qualified Data.ByteString.Lazy as B (pack)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.List (intersperse)
import Control.Monad (unless, forM_, when, replicateM_, foldM)
import Control.Exception (try)
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
      -- modifiedTime <- getModificationTime path
      -- let modSeconds = case modifiedTime of TOD secs _picoSecs -> secs
      let modSeconds = (0 :: Integer)
      pyModule <- parseAndCheckErrors fileContents path
      (moduleLocals, nestedScope) <- topScope pyModule
      -- canonicalPath <- canonicalizePath path 
      canonicalPath <- return path 
      let state = initState ModuleContext moduleLocals 
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
   compile ast@(Module stmts) = do
      maybeDumpScope 
      maybeDumpAST ast
      setObjectName "<module>"
      compileClassModuleDocString stmts
      compile $ Body stmts

-- body of module, function and class
newtype Body = Body [StatementSpan]

instance Compilable Body where
   type CompileResult Body = PyObject
   compile (Body stmts) = do
      mapM_ compile stmts
      -- XXX we could avoid this 'return None' if all branches in the code
      -- ended with a return statement. Can fix this in an optimisation step
      -- with control flow analysis.
      returnNone
      assemble
      makeObject

-- Build an object from all the state computed during compilation, such
-- as the bytecode sequence, variable information and so on.
-- argcount is the number of arguments, not counting *varargs or **kwargs.
makeObject :: Compile PyObject
makeObject = do
   annotatedCode <- getBlockState state_instructions
   let stackDepth = maxStackDepth annotatedCode
   names <- getBlockState state_names
   constants <- getBlockState state_constants
   freeVars <- getBlockState state_freeVars
   cellVars <- getBlockState state_cellVars
   argcount <- getBlockState state_argcount
   flags <- getBlockState state_flags
   fastLocals <- getBlockState state_fastLocals
   lineNumberTable <- compileLineNumberTable 
   let code = map annotatedCode_bytecode annotatedCode 
       localVarNames = map Unicode $ indexedVarSetKeys fastLocals
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
                   , flags = flags 
                   , code = String $ encode code
                   , consts = makeConstants constants
                   , names = makeNames names
                   , varnames = Blip.Tuple localVarNames
                   , freevars = makeVarSetTuple freeVars
                   , cellvars = makeVarSetTuple cellVars
                   , filename = Unicode pyFileName
                   , name = Unicode objectName
                   , firstlineno = 0
                   -- , lnotab = String B.empty
                   , lnotab = lineNumberTable
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
   compile stmt =
      setLineNumber (annot stmt) >>
      compileStmt stmt
  
compileStmt :: StatementSpan -> Compile ()
compileStmt (Assign {..}) = do
   compile assign_expr
   compileAssignments assign_to
compileStmt (AugmentedAssign {..}) =
   case aug_assign_to of
      Var {..} -> do
         let varIdent = ident_string var_ident
         emitReadVar varIdent
         compile aug_assign_expr
         compile aug_assign_op
         emitWriteVar varIdent
      Subscript {..} -> do
         compile subscriptee
         compile subscript_expr
         emitCodeNoArg DUP_TOP_TWO -- avoids re-doing the above two later when we store
         emitCodeNoArg BINARY_SUBSCR
         compile aug_assign_expr
         compile aug_assign_op
         emitCodeNoArg ROT_THREE
         emitCodeNoArg STORE_SUBSCR
      SlicedExpr {..} -> do
         compile slicee
         compileSlices slices
         emitCodeNoArg DUP_TOP_TWO -- avoids re-doing the above two later when we store
         emitCodeNoArg BINARY_SUBSCR
         compile aug_assign_expr
         compile aug_assign_op
         emitCodeNoArg ROT_THREE
         emitCodeNoArg STORE_SUBSCR
      expr@(BinaryOp { operator = Dot {}, right_op_arg = Var {..}}) -> do
         compile $ left_op_arg expr
         emitCodeNoArg DUP_TOP
         index <- lookupNameVar $ ident_string $ var_ident
         emitCodeArg LOAD_ATTR index 
         compile aug_assign_expr
         compile aug_assign_op
         emitCodeNoArg ROT_TWO
         emitCodeArg STORE_ATTR index 
      other -> error $ "unexpected expression in augmented assignment: " ++ prettyText other
compileStmt (Return { return_expr = Nothing }) = returnNone
compileStmt (Return { return_expr = Just expr }) =  
   compile expr >> emitCodeNoArg RETURN_VALUE
compileStmt (Pass {}) = return ()
compileStmt (StmtExpr {..}) = 
   unless (isPureExpr stmt_expr) $ 
      compile stmt_expr >> emitCodeNoArg POP_TOP
compileStmt (Conditional {..}) = do
   restLabel <- newLabel
   mapM_ (compileGuard restLabel) cond_guards 
   mapM_ compile cond_else
   labelNextInstruction restLabel
compileStmt (While {..}) = do
   startLoop <- newLabel
   endLoop <- newLabel
   anchor <- newLabel
   emitCodeArg SETUP_LOOP endLoop
   withFrameBlock (FrameBlockLoop startLoop) $ do
       labelNextInstruction startLoop
       compile while_cond
       emitCodeArg POP_JUMP_IF_FALSE anchor
       mapM_ compile while_body
       emitCodeArg JUMP_ABSOLUTE startLoop
       labelNextInstruction anchor 
       emitCodeNoArg POP_BLOCK
   mapM_ compile while_else
   labelNextInstruction endLoop
compileStmt (For {..}) = do
   startLoop <- newLabel
   endLoop <- newLabel
   withFrameBlock (FrameBlockLoop startLoop) $ do
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
      mapM_ compile for_body 
      emitCodeArg JUMP_ABSOLUTE startLoop
      labelNextInstruction anchor
      emitCodeNoArg POP_BLOCK
   mapM_ compile for_else
   labelNextInstruction endLoop
compileStmt stmt@(Fun {..}) = compileFun stmt []
compileStmt stmt@(Class {..}) = compileClass stmt []
-- XXX assertions appear to be turned off if the code is compiled
-- for optimisation
-- If the assertion expression is a tuple of non-zero length, then
-- it is always True: CPython warns about this
compileStmt (Assert {..}) = do
   case assert_exprs of
      test_expr:restAssertExprs -> do
         compile test_expr
         end <- newLabel
         emitCodeArg POP_JUMP_IF_TRUE end
         assertionErrorVar <- lookupNameVar "AssertionError"
         emitCodeArg LOAD_GLOBAL assertionErrorVar
         case restAssertExprs of
            assertMsgExpr:_ -> do
               compile assertMsgExpr
               emitCodeArg CALL_FUNCTION 1
            _other -> return ()
         emitCodeArg RAISE_VARARGS 1
         labelNextInstruction end
      _other -> error "assert with no test"
compileStmt stmt@(Try {..}) = compileTry stmt
compileStmt (Import {..}) = mapM_ compile import_items
-- XXX need to handle from __future__ 
compileStmt (FromImport {..}) = do
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
             index <- lookupNameVar $ ident_string from_item_name
             emitCodeArg IMPORT_FROM index
             let storeName = case from_as_name of
                    Nothing -> from_item_name
                    Just asName -> asName
             emitWriteVar $ ident_string storeName
         emitCodeNoArg POP_TOP
-- XXX should check that we are inside a loop
compileStmt (Break {}) = emitCodeNoArg BREAK_LOOP
compileStmt (Continue {}) = do
   maybeFrameBlockInfo <- peekFrameBlock
   case maybeFrameBlockInfo of
      Nothing -> error loopError
      Just (FrameBlockLoop label) -> emitCodeArg JUMP_ABSOLUTE label 
      Just FrameBlockFinallyEnd ->
         error finallyError
      Just _other -> checkFrameBlocks
   where
   -- keep blocking the frame block stack until we either find
   -- a loop entry, otherwise generate an error
   checkFrameBlocks :: Compile ()
   checkFrameBlocks = do
      maybeFrameBlockInfo <- peekFrameBlock
      case maybeFrameBlockInfo of
         Nothing -> error loopError
         Just FrameBlockFinallyEnd -> error finallyError 
         Just (FrameBlockLoop label) ->
            emitCodeArg CONTINUE_LOOP label
         Just _other -> checkFrameBlocks
   loopError = "'continue' not properly in loop"
   finallyError = "'continue' not supported inside 'finally' clause"
compileStmt (NonLocal {}) = return ()
compileStmt (Global {}) = return ()
compileStmt (Decorated {..}) =
   case decorated_def of
      Fun {} -> compileFun decorated_def decorated_decorators
      Class {} -> compileClass decorated_def decorated_decorators
      other -> error $ "Decorated statement is not a function or a class: " ++ prettyText other
compileStmt (Delete {..}) = mapM_ compileDelete del_exprs
compileStmt stmt@(With {..})
   -- desugar with statements containing multiple contexts into nested
   -- with statements containing single contexts
   | length with_context > 1 = compileWith $ desugarWith stmt 
   | otherwise = compileWith stmt
compileStmt (Raise {..}) = compile raise_expr
compileStmt other = error $ "Unsupported statement:\n" ++ prettyText other

instance Compilable ExprSpan where
   type CompileResult ExprSpan = ()
   compile expr = 
      setLineNumber (annot expr) >>
      compileExpr expr

compileExpr :: ExprSpan -> Compile ()
compileExpr (Var { var_ident = ident }) = do
   emitReadVar $ ident_string ident
compileExpr expr@(AST.Strings {}) =
   compileConstantEmit $ constantToPyObject expr 
compileExpr expr@(AST.ByteStrings {}) =
   compileConstantEmit $ constantToPyObject expr 
compileExpr expr@(AST.Int {}) =
   compileConstantEmit $ constantToPyObject expr
compileExpr expr@(AST.Float {}) =
   compileConstantEmit $ constantToPyObject expr
compileExpr expr@(AST.Imaginary {}) =
   compileConstantEmit $ constantToPyObject expr
compileExpr expr@(AST.Bool {}) =
   compileConstantEmit $ constantToPyObject expr
compileExpr expr@(AST.None {}) =
   compileConstantEmit $ constantToPyObject expr
compileExpr expr@(AST.Ellipsis {}) =
   compileConstantEmit $ constantToPyObject expr
compileExpr (AST.Paren {..}) = compile paren_expr
compileExpr (AST.CondExpr {..}) = do
   compile ce_condition
   falseLabel <- newLabel
   emitCodeArg POP_JUMP_IF_FALSE falseLabel
   compile ce_true_branch
   restLabel <- newLabel
   emitCodeArg JUMP_FORWARD restLabel
   labelNextInstruction falseLabel 
   compile ce_false_branch
   labelNextInstruction restLabel
compileExpr expr@(AST.Tuple {..})
   | isPyObjectExpr expr =
        compileConstantEmit $ constantToPyObject expr
   | otherwise = do
        mapM_ compile tuple_exprs
        emitCodeArg BUILD_TUPLE $ fromIntegral $ length tuple_exprs
compileExpr (AST.List {..}) = do
   mapM_ compile list_exprs
   emitCodeArg BUILD_LIST $ fromIntegral $ length list_exprs
compileExpr (AST.Set {..}) = do
   mapM_ compile set_exprs
   emitCodeArg BUILD_SET $ fromIntegral $ length set_exprs
compileExpr (Dictionary {..}) = do
   emitCodeArg BUILD_MAP $ fromIntegral $ length dict_mappings
   forM_ dict_mappings $ \(key, value) -> do
      compile value
      compile key
      emitCodeNoArg STORE_MAP
compileExpr (ListComp {..}) = do
   let initStmt = [mkAssignVar resultName (mkList [])]
       updater = \expr -> mkStmtExpr $ mkMethodCall (mkVar $ resultName) "append" expr
       returnStmt = [mkReturn $ mkVar $ resultName]
   compileComprehension "<listcomp>" initStmt updater returnStmt list_comprehension
compileExpr (SetComp {..}) = do
   let initStmt = [mkAssignVar resultName (mkSet [])]
       updater = \expr -> mkStmtExpr $ mkMethodCall (mkVar $ resultName) "add" expr
       returnStmt = [mkReturn $ mkVar $ resultName]
   compileComprehension "<setcomp>" initStmt updater returnStmt set_comprehension
compileExpr (DictComp {..}) = do
   let initStmt = [mkAssignVar resultName (mkDict [])]
       updater = \(key, val) -> 
          mkAssign (mkSubscript (mkVar $ resultName) key) val
       returnStmt = [mkReturn $ mkVar $ resultName]
   compileComprehension "<dictcomp>" initStmt updater returnStmt dict_comprehension
compileExpr (Generator {..}) = do
   let updater = \expr -> mkStmtExpr $ mkYield expr
   compileComprehension "<gencomp>" [] updater [] gen_comprehension
compileExpr (Yield { yield_expr = Nothing }) =
   compileConstantEmit Blip.None >> emitCodeNoArg YIELD_VALUE >> setFlag co_generator
compileExpr (Yield { yield_expr = Just expr }) =
   compile expr >> emitCodeNoArg YIELD_VALUE >> setFlag co_generator
compileExpr (Call {..}) = do
   compile call_fun
   compileCall 0 call_args
compileExpr (Subscript {..}) = do
   compile subscriptee
   compile subscript_expr
   emitCodeNoArg BINARY_SUBSCR
compileExpr (SlicedExpr {..}) = do
   compile slicee
   compileSlices slices
   emitCodeNoArg BINARY_SUBSCR
compileExpr exp@(BinaryOp {..})
   | isBoolean operator = compileBoolOpExpr exp
   | isComparison operator = compileCompareOpExpr exp
   | isDot operator = compileDot exp 
   | otherwise = do 
        compile left_op_arg
        compile right_op_arg
        compileOp operator 
compileExpr (UnaryOp {..}) = do
   compile op_arg
   compileUnaryOp operator
compileExpr (Lambda {..}) = do
   funBodyObj <- nestedBlock FunctionContext expr_annot $ do
      -- make the first constant None, to indicate no doc string
      -- for the lambda
      _ <- compileConstant Blip.None
      compile lambda_body
      emitCodeNoArg RETURN_VALUE
      assemble
      makeObject
   numDefaults <- compileDefaultParams lambda_args
   compileClosure "<lambda>" funBodyObj numDefaults
compileExpr other = error $ "Unsupported expr:\n" ++ prettyText other

instance Compilable AssignOpSpan where
   type CompileResult AssignOpSpan = ()
   compile = emitCodeNoArg . assignOpCode

instance Compilable DecoratorSpan where
   type CompileResult DecoratorSpan = ()
   compile dec@(Decorator {..}) = do
      compileDottedName decorator_name
      let numDecorators = length decorator_args
      when (numDecorators > 0) $ 
          compileCall 0 decorator_args
      where
      compileDottedName (name:rest) = do
         emitReadVar $ ident_string name
         forM_ rest $ \var -> do
            index <- lookupNameVar $ ident_string var
            emitCodeArg LOAD_ATTR index
      compileDottedName [] =
         error $ "decorator with no name: " ++ prettyText dec

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
      index <- lookupNameVar dottedNameStr
      emitCodeArg IMPORT_NAME index
      storeName <- 
         case import_as_name of
            Nothing -> return $ head import_item_name
            Just asName -> do
               forM_ (tail dottedNames) $ \attribute -> do
                  index <- lookupNameVar attribute
                  emitCodeArg LOAD_ATTR index 
               return asName
      emitWriteVar $ ident_string storeName

instance Compilable RaiseExprSpan where
   type CompileResult RaiseExprSpan = ()
   compile (RaiseV3 maybeRaiseArg) = do
      n <- case maybeRaiseArg of
              Nothing -> return 0
              Just (raiseExpr, maybeFrom) -> do
                 compile raiseExpr
                 case maybeFrom of
                    Nothing -> return 1
                    Just fromExpr -> do
                       compile fromExpr
                       return 2
      emitCodeArg RAISE_VARARGS n 
   compile stmt@(RaiseV2 _) =
      error $ "Python version 2 raise statement encountered: " ++ prettyText stmt

{-
   From CPython compile.c

   Code generated for "try: S except E1 as V1: S1 except E2 as V2: S2 ...":
   (The contents of the value stack is shown in [], with the top
   at the right; 'tb' is trace-back info, 'val' the exception's
   associated value, and 'exc' the exception.)

   Value stack          Label   Instruction     Argument
   []                           SETUP_EXCEPT    L1
   []                           <code for S>
   []                           POP_BLOCK
   []                           JUMP_FORWARD    L0

   [tb, val, exc]       L1:     DUP                             )
   [tb, val, exc, exc]          <evaluate E1>                   )
   [tb, val, exc, exc, E1]      COMPARE_OP      EXC_MATCH       ) only if E1
   [tb, val, exc, 1-or-0]       POP_JUMP_IF_FALSE       L2      )
   [tb, val, exc]               POP
   [tb, val]                    <assign to V1>  (or POP if no V1)
   [tb]                         POP
   []                           <code for S1>
                                POP_EXCEPT
                                JUMP_FORWARD    L0

   [tb, val, exc]       L2:     DUP
   .............................etc.......................

   [tb, val, exc]       Ln+1:   END_FINALLY     # re-raise exception

   []                   L0:     <next statement>

   Of course, parts are not generated if Vi or Ei is not present.
-}

compileTry :: StatementSpan -> Compile ()
compileTry stmt@(Try {..})
   | length try_finally == 0 = compileTryExcept stmt
   | otherwise = compileTryFinally stmt
compileTry other =
   error $ "Unexpected statement when compiling a try-except: " ++ prettyText other 

compileTryFinally :: StatementSpan -> Compile ()
compileTryFinally stmt@(Try {..}) = do
   end <- newLabel
   emitCodeArg SETUP_FINALLY end
   body <- newLabel
   labelNextInstruction body
   withFrameBlock FrameBlockFinallyTry $ do
      if length try_excepts > 0
         then compileTryExcept stmt 
         else mapM_ compile try_body
      emitCodeNoArg POP_BLOCK
   _ <- compileConstantEmit Blip.None
   labelNextInstruction end
   withFrameBlock FrameBlockFinallyEnd $ do
      mapM_ compile try_finally
      emitCodeNoArg END_FINALLY
compileTryFinally other =
   error $ "Unexpected statement when compiling a try-except: " ++ prettyText other 

compileTryExcept :: StatementSpan -> Compile ()
compileTryExcept (Try {..}) = do
   firstHandler <- newLabel                      -- L1
   emitCodeArg SETUP_EXCEPT firstHandler         -- pushes handler onto block stack
   withFrameBlock FrameBlockExcept $ do
      mapM_ compile try_body                     -- <code for S>
      emitCodeNoArg POP_BLOCK                    -- pops handler off block stack
   orElse <- newLabel
   emitCodeArg JUMP_FORWARD orElse 
   end <- newLabel                               -- L0
   compileHandlers end firstHandler try_excepts
   labelNextInstruction orElse
   mapM_ compile try_else
   labelNextInstruction end                      -- L0: <next statement>
compileTryExcept other =
   error $ "Unexpected statement when compiling a try-except: " ++ prettyText other 

-- Compile a sequence of exception handlers
compileHandlers :: Word16 -> Word16 -> [HandlerSpan] -> Compile ()
compileHandlers _end handlerLabel [] = do
   labelNextInstruction handlerLabel             -- Ln+1, # re-raise exception
   emitCodeNoArg END_FINALLY
compileHandlers end handlerLabel (Handler {..} : rest) = do
   labelNextInstruction handlerLabel
   nextLabel <- newLabel 
   compileHandlerClause nextLabel handler_clause
   emitCodeNoArg POP_TOP                         -- pop the traceback (tb) off the stack
   withFrameBlock FrameBlockFinallyTry $ do
      mapM_ compile handler_suite                   -- <code for S1, S2 ..>
      emitCodeNoArg POP_EXCEPT                      -- pop handler off the block stack
   emitCodeArg JUMP_FORWARD end
   compileHandlers end nextLabel rest 

-- enter here with stack == (s ++ [tb, val, exc]), leave with stack == s
compileHandlerClause :: Word16 -> ExceptClauseSpan -> Compile ()
compileHandlerClause nextHandler (ExceptClause {..}) = do
   case except_clause of
      Nothing -> do
         emitCodeNoArg POP_TOP                  -- pop exc off the stack
         emitCodeNoArg POP_TOP                  -- pop val off the stack
      Just (target, asExpr) -> do
         emitCodeNoArg DUP_TOP                  -- duplicate exc on stack
         compile target                         -- <evaluate E1>
         emitCodeArg COMPARE_OP exactMatchOp    -- compare E1 to exc
         emitCodeArg POP_JUMP_IF_FALSE nextHandler -- pop True/False and if no match try next handler
         emitCodeNoArg POP_TOP                  -- pop exc off the stack
         case asExpr of
            Nothing -> emitCodeNoArg POP_TOP    -- pop val off the stack
            -- XXX we should del this name at the end.
            Just expr -> compileAssignTo expr   -- assign the exception to the as name, will remove val from stack
   where
   -- The code for an exact match operator.
   exactMatchOp :: Word16
   exactMatchOp = 10

withDecorators :: [DecoratorSpan] -> Compile () -> Compile ()
withDecorators decorators comp = do
   -- push each of the decorators on the stack
   mapM_ compile decorators
   -- run the enclosed computation
   comp
   -- call each of the decorators
   replicateM_ (length decorators) $ 
      emitCodeArg CALL_FUNCTION 1

nestedBlock :: Context -> SrcSpan -> Compile a -> Compile a
nestedBlock context span comp = do
   -- save the current block state
   oldBlockState <- getBlockState id
   -- set the new block state to initial values, and the
   -- scope of the current definition
   (name, localScope) <- getLocalScope $ spanToScopeIdentifier span 
   setBlockState $ initBlockState context localScope
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
      funBodyObj <- nestedBlock FunctionContext stmt_annot $ do
         compileFunDocString fun_body
         compile $ Body fun_body
      numDefaults <- compileDefaultParams fun_args
      compileClosure funName funBodyObj numDefaults
   emitWriteVar funName
compileFun other _decorators = error $ "compileFun applied to a non function: " ++ prettyText other

-- Compile a class definition, possibly with decorators.
compileClass :: StatementSpan -> [DecoratorSpan] -> Compile ()
compileClass (Class {..}) decorators = do
   let className = ident_string $ class_name
   withDecorators decorators $ do
      classBodyObj <- nestedBlock ClassContext stmt_annot $ do
         -- classes have a special argument called __locals__
         -- it is the only argument they have in the byte code, but it
         -- does not come from the source code, so we have to add it.
         setFastLocals ["__locals__"]
         setArgCount 1
         emitCodeArg LOAD_FAST 0
         emitCodeNoArg STORE_LOCALS
         emitReadVar "__name__"
         emitWriteVar "__module__"
         compileConstantEmit $ Unicode className
         emitWriteVar "__qualname__"
         compileClassModuleDocString class_body
         compile $ Body class_body
      emitCodeNoArg LOAD_BUILD_CLASS
      compileClosure className classBodyObj 0
      compileConstantEmit $ Unicode className
      compileCall 2 class_args
   emitWriteVar className
compileClass other _decorators = error $ "compileClass applied to a non class: " ++ prettyText other

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
               _other -> error $ name ++ " closure free variable not cell or free var in outer context: " ++ var
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

-- Compile a 'from module import'.
compileFromModule :: ImportRelativeSpan -> Compile ()
-- XXX what to do about the initial dots?
compileFromModule (ImportRelative {..}) = do
   let moduleName =
          case import_relative_module of
             Nothing -> ""
             Just dottedNames ->
                concat $ intersperse "." $ map ident_string dottedNames
   index <- lookupNameVar moduleName 
   emitCodeArg IMPORT_NAME index

fromItemsIdentifiers :: FromItemsSpan -> [Identifier]
fromItemsIdentifiers (ImportEverything {}) = ["*"]
fromItemsIdentifiers (FromItems {..}) =
   map fromItemIdentifier from_items_items
   where
   fromItemIdentifier :: FromItemSpan -> Identifier
   fromItemIdentifier (FromItem {..}) = ident_string $ from_item_name

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
compileAssignTo (Var {..}) =
   emitWriteVar $ ident_string var_ident
compileAssignTo (Subscript {..}) = 
   compile subscriptee >>
   compile subscript_expr >>
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
   index <- lookupNameVar $ ident_string $ var_ident
   emitCodeArg STORE_ATTR index
compileAssignTo (SlicedExpr {..}) = do
   compile slicee
   compileSlices slices
   emitCodeNoArg STORE_SUBSCR  
compileAssignTo other = error $ "assignment to unexpected expression:\n" ++ prettyText other

compileDelete :: ExprSpan -> Compile ()
compileDelete (Var {..}) = do
   emitDeleteVar $ ident_string var_ident
compileDelete (Subscript {..}) =
   compile subscriptee >>
   compile subscript_expr >>
   emitCodeNoArg DELETE_SUBSCR
compileDelete (AST.Paren {..}) = compileDelete paren_expr
compileDelete (expr@(BinaryOp { operator = Dot {}, right_op_arg = Var {..}})) = do
   compile $ left_op_arg expr
   index <- lookupNameVar $ ident_string $ var_ident
   emitCodeArg DELETE_ATTR index
compileDelete (SlicedExpr {..}) = do
   compile slicee
   compileSlices slices
   emitCodeNoArg DELETE_SUBSCR  
compileDelete other = error $ "delete of unexpected expression:\n" ++ prettyText other

compileWith :: StatementSpan -> Compile ()
compileWith stmt@(With {..}) = 
   case with_context of
      [(context, maybeAs)] -> do
         blockLabel <- newLabel
         finallyLabel <- newLabel
         compile context
         emitCodeArg SETUP_WITH finallyLabel
         labelNextInstruction blockLabel
         withFrameBlock FrameBlockFinallyTry $ do
            case maybeAs of
               -- Discard result from context.__enter__()
               Nothing -> emitCodeNoArg POP_TOP
               Just expr -> compileAssignTo expr
            mapM_ compile with_body
            emitCodeNoArg POP_BLOCK
         _ <- compileConstantEmit Blip.None
         labelNextInstruction finallyLabel
         withFrameBlock FrameBlockFinallyEnd $ do
            emitCodeNoArg WITH_CLEANUP
            emitCodeNoArg END_FINALLY
      _other -> error $ "compileWith applied to non desugared with statement: " ++ prettyText stmt 
compileWith other = error $ "compileWith applied to non with statement: " ++ prettyText other

-- Check for a docstring in the first statement of a function body.
-- The first constant in the corresponding code object is inspected
-- by the interpreter for the docstring. If there is no docstring
-- then the first constant must be None
compileFunDocString :: [StatementSpan] -> Compile ()
compileFunDocString (firstStmt:_stmts)
   | StmtExpr {..} <- firstStmt,
     Strings {} <- stmt_expr
        = compileConstant (constantToPyObject stmt_expr) >> return ()
   | otherwise = compileConstant Blip.None >> return ()
compileFunDocString [] = compileConstant Blip.None >> return ()

compileClassModuleDocString :: [StatementSpan] -> Compile ()
compileClassModuleDocString (firstStmt:_stmts)
   | StmtExpr {..} <- firstStmt,
     Strings {} <- stmt_expr
        -- XXX what if another __doc__ is in scope?
        = do compileConstantEmit $ constantToPyObject stmt_expr
             emitWriteVar "__doc__"
   | otherwise = return ()
compileClassModuleDocString [] = return ()

-- Compile a conditional guard
compileGuard :: Word16 -> (ExprSpan, [StatementSpan]) -> Compile ()
compileGuard restLabel (expr, stmts) = do
   compile expr
   falseLabel <- newLabel
   emitCodeArg POP_JUMP_IF_FALSE falseLabel
   mapM_ compile stmts
   emitCodeArg JUMP_FORWARD restLabel
   labelNextInstruction falseLabel 

-- Desugar the comprehension into a zero-arity function (body) with
-- a (possibly nested) for loop, then call the function.
compileComprehension
   :: Identifier 
   -> [StatementSpan]
   -> (a -> StatementSpan) 
   -> [StatementSpan]
   -> ComprehensionSpan a
   -> Compile ()
compileComprehension name initStmt updater returnStmt comprehension = do
   let desugaredComp = desugarComprehension initStmt updater returnStmt comprehension 
       comprehensionSpan = comprehension_annot comprehension
   funObj <- nestedBlock
                FunctionContext
                comprehensionSpan 
                (compile $ Body desugaredComp)
   compileClosure name funObj 0
   (_name, localScope) <- getLocalScope $ spanToScopeIdentifier comprehensionSpan
   let parameterNames = parameterTypes_pos $ localScope_params localScope 
   mapM_ emitReadVar parameterNames
   emitCodeArg CALL_FUNCTION $ fromIntegral $ length parameterNames

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
constantToPyObject (AST.Tuple {..}) =
   Blip.Tuple { elements = map constantToPyObject tuple_exprs }
constantToPyObject (AST.Strings {..}) =
   Blip.Unicode { unicode = concat $ map normaliseString strings_strings }
constantToPyObject (AST.ByteStrings {..}) =
   Blip.String { string = fromString $ concat $ map normaliseString byte_string_strings }
constantToPyObject other =
   error $ "constantToPyObject applied to an unexpected expression: " ++ prettyText other

-- The strings in the AST retain their original quote marks which
-- need to be removed, we have to remove single or triple quotes.
-- We assume the parser has correctly matched the quotes.
-- Escaped characters such as \n \t are parsed as multiple characters
-- and need to be converted back into single characters.
normaliseString :: String -> String
normaliseString ('r':'b':rest) = removeQuotes rest
normaliseString ('b':'r':rest) = removeQuotes rest
normaliseString ('b':rest) = unescapeString $ removeQuotes rest
normaliseString ('r':rest) = removeQuotes rest
normaliseString other = unescapeString $ removeQuotes other 

removeQuotes :: String -> String
removeQuotes ('\'':'\'':'\'':rest) = take (length rest - 3) rest
removeQuotes ('"':'"':'"':rest) = take (length rest - 3) rest
removeQuotes ('\'':rest) = init rest
removeQuotes ('"':rest) = init rest
removeQuotes other = error $ "bad literal string: " ++ other

data CallArgs =
   CallArgs
   { callArgs_pos :: !Word16
   , callArgs_keyword :: !Word16
   , callArgs_varPos :: !Bool
   , callArgs_varKeyword :: !Bool
   }

initCallArgs :: CallArgs
initCallArgs =
   CallArgs
   { callArgs_pos = 0
   , callArgs_keyword = 0
   , callArgs_varPos = False
   , callArgs_varKeyword = False
   }

-- Compile the arguments to a call and
-- decide which particular CALL_FUNCTION bytecode to emit.
-- numExtraArgs counts any additional arguments the function
-- might have been applied to, which is necessary for classes
-- which get extra arguments beyond the ones mentioned in the
-- program source.
compileCall :: Word16 -> [ArgumentSpan] -> Compile ()
compileCall numExtraArgs args = do
   CallArgs {..} <- compileCallArgs args 
   let opArg = (callArgs_pos + numExtraArgs) .|. callArgs_keyword `shiftL` 8
   case (callArgs_varPos, callArgs_varKeyword) of
      (False, False) -> emitCodeArg CALL_FUNCTION opArg 
      (True, False) -> emitCodeArg CALL_FUNCTION_VAR opArg 
      (False, True) -> emitCodeArg CALL_FUNCTION_KW opArg 
      (True, True) -> emitCodeArg CALL_FUNCTION_VAR_KW opArg 

-- Compile the arguments to a function call and return the number
-- of positional arguments, and the number of keyword arguments.
compileCallArgs :: [ArgumentSpan] -> Compile CallArgs
compileCallArgs = foldM compileArg initCallArgs 
   where
   compileArg :: CallArgs  -> ArgumentSpan -> Compile CallArgs 
   compileArg callArgs@(CallArgs {..}) (ArgExpr {..}) = do
      compile arg_expr
      return $ callArgs { callArgs_pos = callArgs_pos + 1 }
   compileArg callArgs@(CallArgs {..}) (ArgKeyword {..}) = do
      compileConstantEmit $ Unicode $ ident_string arg_keyword
      compile arg_expr
      return $ callArgs { callArgs_keyword = callArgs_keyword + 1 }
   compileArg callArgs@(CallArgs {..}) (ArgVarArgsPos {..}) = do
      compile arg_expr
      return $ callArgs { callArgs_varPos = True }
   compileArg callArgs@(CallArgs {..}) (ArgVarArgsKeyword {..}) = do
      compile arg_expr
      return $ callArgs { callArgs_varKeyword = True }

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
isComparison (In {}) = True
isComparison (NotIn {}) = True
isComparison (IsNot {}) = True
isComparison (Is {}) = True
isComparison _other = False

compileDot :: ExprSpan -> Compile ()
compileDot (BinaryOp {..}) = do
   compile left_op_arg
   case right_op_arg of
      Var {..} -> do
         -- the right argument should be treated like name variable
         varInfo <- lookupNameVar $ ident_string var_ident
         emitCodeArg LOAD_ATTR varInfo 
      other -> error $ "right argument of dot operator not a variable:\n" ++ prettyText other
compileDot other =
   error $ "compileDot applied to an unexpected expression: " ++ prettyText other

compileBoolOpExpr :: ExprSpan -> Compile ()
compileBoolOpExpr (BinaryOp {..}) = do
   endLabel <- newLabel
   compile left_op_arg
   case operator of
      And {..} -> emitCodeArg JUMP_IF_FALSE_OR_POP endLabel
      Or {..} ->  emitCodeArg JUMP_IF_TRUE_OR_POP endLabel
      other -> error $ "Unexpected boolean operator:\n" ++ prettyText other
   compile right_op_arg
   labelNextInstruction endLabel
compileBoolOpExpr other =
   error $ "compileBoolOpExpr applied to an unexpected expression: " ++ prettyText other

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
      _other -> error $ "Unexpected operator:\n" ++ prettyText operator

compileUnaryOp :: OpSpan -> Compile ()
compileUnaryOp operator =
   emitCodeNoArg $ case operator of
      Minus {} -> UNARY_NEGATIVE
      Plus {} -> UNARY_POSITIVE
      Not {} -> UNARY_NOT
      Invert {} -> UNARY_INVERT
      other ->  error $ "Unexpected unary operator: " ++ prettyText other

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

{- Operator chaining:

   The parser treats comparison operators as left associative.

   So: w < x < y < z is parsed as

   (((w < x) < y) < z)

   We want to compile this to:

         [w]
         [x]
         DUP_TOP   # make a copy of the result of x
         ROT_THREE # put the copy of [x] to the bottom
         <
         JUMP_IF_FALSE_OR_POP cleanup
         [y]
         DUP_TOP   # make a copy of [y]
         ROT_THREE # put the copy of [y] to the bottom
         <
         JUMP_IF_FALSE_OR_POP cleanup
         [z]
         <
         JUMP_FORWARD end
   cleanup:
         ROT_TWO  # put the result of the last comparison on the bottom 
                  # and put the duplicated [y] on the top
         POP_TOP  # remove the duplicated [y] from the top
   end:
         # whatever code follows
-}

compileCompareOpExpr :: ExprSpan -> Compile ()
compileCompareOpExpr expr@(BinaryOp {}) =
   compileChain numOps chain
   where
   chain :: [ChainItem]
   chain = flattenComparisonChain [] expr
   numOps :: Int
   numOps = length chain `div` 2

   compileChain :: Int -> [ChainItem] -> Compile ()
   compileChain numOps (Comparator e1 : internal@(Operator op : Comparator e2 : _rest)) = do
      compile e1
      if numOps == 1
         then do
            compile e2
            emitCodeArg COMPARE_OP $ comparisonOpCode op
         else do
            cleanup <- newLabel
            (lastOp, lastArg) <- compileChainInternal cleanup internal 
            compile lastArg
            emitCodeArg COMPARE_OP $ comparisonOpCode lastOp
            end <- newLabel
            emitCodeArg JUMP_FORWARD end
            labelNextInstruction cleanup
            emitCodeNoArg ROT_TWO
            emitCodeNoArg POP_TOP
            labelNextInstruction end
   compileChain _numOps _items = error $ "bad operator chain: " ++ prettyText expr
   compileChainInternal :: Word16 -> [ChainItem] -> Compile (OpSpan, ExprSpan)
   compileChainInternal _cleanup [Operator op, Comparator exp] = return (op, exp)
   compileChainInternal cleanup (Operator op : Comparator e : rest) = do
      compile e
      emitCodeNoArg DUP_TOP
      emitCodeNoArg ROT_THREE
      emitCodeArg COMPARE_OP $ comparisonOpCode op
      emitCodeArg JUMP_IF_FALSE_OR_POP cleanup
      compileChainInternal cleanup rest
   compileChainInternal _cleanup _other = error $ "bad comparison chain: " ++ prettyText expr 
        
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
   comparisonOpCode operator = error $ "Unexpected comparison operator:\n" ++ prettyText operator
compileCompareOpExpr other = error $ "Unexpected comparison operator:\n" ++ prettyText other 

data ChainItem = Comparator ExprSpan | Operator OpSpan

flattenComparisonChain :: [ChainItem] -> ExprSpan -> [ChainItem] 
flattenComparisonChain acc opExpr@(BinaryOp {..}) 
   | isComparison operator
        = flattenComparisonChain newAcc left_op_arg
   | otherwise = [Comparator opExpr] ++ acc 
   where
   newAcc = [Operator operator, Comparator right_op_arg] ++ acc
flattenComparisonChain acc other = [Comparator other] ++ acc
 
-- Emit an instruction that returns the None contant.
returnNone :: Compile ()
returnNone = compileConstantEmit Blip.None >> emitCodeNoArg RETURN_VALUE

-- Print out the variable scope of the module if requested on the command line.
maybeDumpScope :: Compile ()
maybeDumpScope = 
   ifDump DumpScope $ do
      -- globals <- getGlobals
      nestedScope <- getNestedScope
      -- liftIO $ putStrLn "Variable Scope:"
      liftIO $ putStrLn $ renderScope nestedScope

-- Print out the AST of the module if requested on the command line.
maybeDumpAST :: ModuleSpan -> Compile ()
maybeDumpAST ast = do
   ifDump DumpAST $ do
      liftIO $ putStrLn "Abstract Syntax Tree:"
      liftIO $ putStrLn $ show ast

{- 
   From Cpython: Objects/lnotab_notes.txt

Code objects store a field named co_lnotab.  This is an array of unsigned bytes
disguised as a Python string.  It is used to map bytecode offsets to source code
line #s for tracebacks and to identify line number boundaries for line tracing.

The array is conceptually a compressed list of
    (bytecode offset increment, line number increment)
pairs.  The details are important and delicate, best illustrated by example:

    byte code offset    source code line number
        0                   1
        6                   2
       50                   7
      350                 307
      361                 308

Instead of storing these numbers literally, we compress the list by storing only
the increments from one row to the next.  Conceptually, the stored list might
look like:

    0, 1,  6, 1,  44, 5,  300, 300,  11, 1

The above doesn't really work, but it's a start. Note that an unsigned byte
can't hold negative values, or values larger than 255, and the above example
contains two such values. So we make two tweaks:  


 (a) there's a deep assumption that byte code offsets and their corresponding
 line #s both increase monotonically, and
 (b) if at least one column jumps by more than 255 from one row to the next,
 more than one pair is written to the table. In case #b, there's no way to know
 from looking at the table later how many were written.  That's the delicate
 part.  A user of co_lnotab desiring to find the source line number
 corresponding to a bytecode address A should do something like this

    lineno = addr = 0
    for addr_incr, line_incr in co_lnotab:
        addr += addr_incr
        if addr > A:
            return lineno
        lineno += line_incr

(In C, this is implemented by PyCode_Addr2Line().)  In order for this to work,
when the addr field increments by more than 255, the line # increment in each
pair generated must be 0 until the remaining addr increment is < 256.  So, in
the example above, assemble_lnotab in compile.c should not (as was actually done
until 2.2) expand 300, 300 to
    255, 255, 45, 45,
but to
    255, 0, 45, 255, 0, 45.
-}

-- should produce a bytestring
compileLineNumberTable :: Compile PyObject
compileLineNumberTable = do
   offsetToLine <- reverse `fmap` getBlockState state_lineNumberTable
   let compressedTable = compress (0, 0) offsetToLine 
       bs = B.pack $ concat [ [fromIntegral offset, fromIntegral line] | (offset, line) <- compressedTable ]
   return Blip.String { string = bs }
   where
   compress :: (Word16, Int) -> [(Word16, Int)] -> [(Word16, Int)]
   compress _prev [] = []
   compress (prevOffset, prevLine) (next@(nextOffset, nextLine):rest)
      -- make sure all increments are non-negative
      -- skipping any entries which are less than the predecessor
      | nextLine < prevLine || nextOffset < prevOffset =
           compress (prevOffset, prevLine) rest
      | otherwise = chunkDeltas (offsetDelta, lineDelta) ++ compress next rest
      where 
      offsetDelta = nextOffset - prevOffset
      lineDelta = nextLine - prevLine

-- both offsetDelta and lineDelta must be non-negative
chunkDeltas :: (Word16, Int) -> [(Word16, Int)]
chunkDeltas (offsetDelta, lineDelta)
   | offsetDelta < 256 =
      if lineDelta < 256
         then [(offsetDelta, lineDelta)]
         else (offsetDelta, 255) : chunkDeltas (0, lineDelta - 255)
   | lineDelta < 256 =
      (255, lineDelta) : chunkDeltas (offsetDelta - 255, 0)
   | otherwise = (255, 255) : chunkDeltas (offsetDelta - 255, lineDelta - 255)
