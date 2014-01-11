{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Blip.Compiler.Utils
-- Copyright   : (c) 2012, 2013 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Utility functions which are used in multiple modules, or don't belong
-- anywhere else.
--
-----------------------------------------------------------------------------
module Blip.Compiler.Utils
   ( isJump, isRelativeJump, isAbsoluteJump, isJumpBytecode, isPureExpr
   , isPyObjectExpr, isUnconditionalJump, isConditionalJump, mkVar, mkReturn
   , mkIdent, mkAssign, mkAssignVar, mkList, mkMethodCall, mkStmtExpr, mkSet
   , mkDict , mkSubscript, mkYield, identsFromParameters
   , spanToScopeIdentifier, fromIdentString, countPosParameters
   , maybeToList, getSpanLine )
   where 

import Blip.Bytecode (Opcode (..), Bytecode (..))
import Language.Python.Common.AST as AST
   ( ExprSpan, Expr (..), Statement (..), StatementSpan, Ident (..)
   , IdentSpan, Op (..), OpSpan, Argument (..), ArgumentSpan )
import Language.Python.Common.SrcLocation (SrcSpan (..))
import Blip.Compiler.Types (Identifier, ScopeIdentifier, ParameterTypes (..))

getSpanLine :: SrcSpan -> Maybe Int
getSpanLine (SpanCoLinear {..}) = Just span_row
getSpanLine (SpanMultiLine {..}) = Just span_start_row
getSpanLine (SpanPoint {..}) = Just span_row
getSpanLine SpanEmpty = Nothing

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

fromIdentString :: AST.Ident a -> Identifier
fromIdentString (Ident {..}) = ident_string

spanToScopeIdentifier :: SrcSpan -> ScopeIdentifier
spanToScopeIdentifier (SpanCoLinear {..})
   = (span_row, span_start_column, span_row, span_end_column)
spanToScopeIdentifier (SpanMultiLine {..})
   = (span_start_row, span_start_column, span_end_row, span_end_column)
spanToScopeIdentifier (SpanPoint {..})
   = (span_row, span_column, span_row, span_column)
spanToScopeIdentifier SpanEmpty
   = error "empty source span for scope identifier"

identsFromParameters :: ParameterTypes -> [Identifier]
identsFromParameters (ParameterTypes {..}) =
   parameterTypes_pos ++ maybeToList parameterTypes_varPos ++
   maybeToList parameterTypes_varKeyword
{-
identsFromParameters :: [ParameterSpan] -> [Identifier]
identsFromParameters = concatMap getIdent
   where
   getIdent :: ParameterSpan -> [Identifier]
   getIdent (Param {..}) = [fromIdentString $ param_name]
   getIdent (VarArgsPos {..}) = [fromIdentString $ param_name]
   getIdent (VarArgsKeyword {..}) = [fromIdentString $ param_name]
   getIdent _other = []
-}

countPosParameters :: ParameterTypes -> Int
countPosParameters (ParameterTypes {..}) = length parameterTypes_pos
{-
countPosParameters :: [ParameterSpan] -> Int
countPosParameters = length . filter isPosParameter
   where
   isPosParameter :: ParameterSpan -> Bool
   isPosParameter (Param {}) = True
   isPosParameter _other = False 
-}

-- True if an expression can be represented directly as a PyObject constant.
isPyObjectExpr :: ExprSpan -> Bool
isPyObjectExpr (AST.Int {}) = True
-- XXX not sure about longint
-- isPyObjectExpr (AST.LongInt {}) = True  
isPyObjectExpr (AST.Float {}) = True
-- XXX not sure about imaginary
-- isPyObjectExpr (AST.Imaginary {}) = True
isPyObjectExpr (AST.Bool {}) = True
isPyObjectExpr (AST.None {}) = True
isPyObjectExpr (AST.ByteStrings {}) = True
isPyObjectExpr (AST.Strings {}) = True
isPyObjectExpr (AST.UnicodeStrings {}) = True
isPyObjectExpr (AST.Tuple { tuple_exprs = exprs }) = all isPyObjectExpr exprs
isPyObjectExpr _other = False

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
isPureExpr _other = False

isJumpBytecode :: Bytecode -> Bool
isJumpBytecode (Bytecode {..}) = isJump opcode

-- test if an opcode is a jump instruction
isJump :: Opcode -> Bool
isJump x = isRelativeJump x || isAbsoluteJump x

isRelativeJump :: Opcode -> Bool
isRelativeJump JUMP_FORWARD = True
isRelativeJump SETUP_LOOP = True
isRelativeJump FOR_ITER = True
isRelativeJump SETUP_FINALLY = True
isRelativeJump SETUP_EXCEPT = True
isRelativeJump SETUP_WITH = True
isRelativeJump _ = False

isAbsoluteJump :: Opcode -> Bool
isAbsoluteJump POP_JUMP_IF_FALSE = True
isAbsoluteJump POP_JUMP_IF_TRUE = True
isAbsoluteJump JUMP_ABSOLUTE = True
isAbsoluteJump CONTINUE_LOOP = True
isAbsoluteJump JUMP_IF_FALSE_OR_POP = True
isAbsoluteJump JUMP_IF_TRUE_OR_POP = True
isAbsoluteJump _ = False

isUnconditionalJump :: Opcode -> Bool
isUnconditionalJump JUMP_FORWARD = True
isUnconditionalJump JUMP_ABSOLUTE = True
isUnconditionalJump _other = False

isConditionalJump :: Opcode -> Bool
isConditionalJump = not . isUnconditionalJump

mkIdent :: String -> IdentSpan
mkIdent str = Ident { ident_string = str, ident_annot = SpanEmpty }

mkReturn :: ExprSpan -> StatementSpan
mkReturn expr = Return { return_expr = Just expr, stmt_annot = SpanEmpty }

mkYield :: ExprSpan -> ExprSpan
mkYield expr = Yield { yield_expr = Just expr, expr_annot = SpanEmpty }

mkVar :: IdentSpan -> ExprSpan
mkVar ident = Var { var_ident = ident, expr_annot = SpanEmpty }

mkAssignVar :: IdentSpan -> ExprSpan -> StatementSpan
mkAssignVar ident expr = mkAssign (mkVar ident) expr

mkAssign :: ExprSpan -> ExprSpan -> StatementSpan
mkAssign lhs rhs =
   Assign { assign_to = [lhs]
          , assign_expr = rhs 
          , stmt_annot = SpanEmpty }

mkList :: [ExprSpan] -> ExprSpan
mkList exprs = List { list_exprs = exprs, expr_annot = SpanEmpty }

mkSet :: [ExprSpan] -> ExprSpan
mkSet exprs = Set { set_exprs = exprs, expr_annot = SpanEmpty }

mkDict :: [(ExprSpan, ExprSpan)] -> ExprSpan
mkDict exprs = Dictionary { dict_mappings = exprs, expr_annot = SpanEmpty }

mkMethodCall :: ExprSpan -> String -> ExprSpan -> ExprSpan
mkMethodCall object methodName argument =
   mkCall (mkAttributeLookup object methodName) [argument]

mkAttributeLookup :: ExprSpan -> String -> ExprSpan
mkAttributeLookup object methodName =
   BinaryOp { operator = dot
            , left_op_arg = object
            , right_op_arg = mkVar (mkIdent methodName)
            , expr_annot = SpanEmpty }

dot :: OpSpan
dot = Dot { op_annot = SpanEmpty }

mkCall :: ExprSpan -> [ExprSpan] -> ExprSpan 
mkCall fun args = 
   Call { call_fun = fun
        , call_args = map mkArgument args
        , expr_annot = SpanEmpty }

mkArgument :: ExprSpan -> ArgumentSpan
mkArgument expr = ArgExpr { arg_expr = expr, arg_annot = SpanEmpty }

mkStmtExpr :: ExprSpan -> StatementSpan
mkStmtExpr expr = StmtExpr { stmt_expr = expr, stmt_annot = SpanEmpty }

mkSubscript :: ExprSpan -> ExprSpan -> ExprSpan
mkSubscript object index =
   Subscript { subscriptee = object, subscript_expr = index, expr_annot = SpanEmpty }
