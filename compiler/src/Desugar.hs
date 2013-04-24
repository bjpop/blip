{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Desugar 
-- Copyright   : (c) 2013 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Desugar Python syntax.
--
-- For example, comprehensions are dsugared into functions with for loops
-- in their bodies.
-- 
-- 'with' statements containing multiple context managers are turned into
-- nested 'with' statements with a single context manager each.
--
-----------------------------------------------------------------------------

module Desugar (desugarComprehension, desugarWith) where

import Prelude hiding (mapM)
import Utils (mkVar, mkReturn, mkIdent)
import Language.Python.Common.AST as AST
   ( StatementSpan, Statement (..), ExprSpan, Comprehension (..)
   , ComprehensionSpan, CompFor (..), CompForSpan, CompIf (..), CompIfSpan
   , CompIter (..), CompIterSpan) 
import Language.Python.Common.SrcLocation (SrcSpan (..))
import Language.Python.Common (prettyText)

desugarComprehension :: StatementSpan -> (a -> StatementSpan) -> ComprehensionSpan a -> [StatementSpan]
desugarComprehension initStmt updater (Comprehension {..}) =
   [ initStmt, forLoop, returnStmt ]  
   where
   resultName = mkIdent "$result"
   returnStmt = mkReturn $ mkVar $ resultName
   updateStmt = updater comprehension_expr
   forLoop = desugarCompFor updateStmt comprehension_for

desugarCompFor :: StatementSpan -> CompForSpan -> StatementSpan
desugarCompFor updateStmt (CompFor {..}) =
   For { for_targets = comp_for_exprs
       , for_generator = comp_in_expr
       , for_body = [forBody]
       , for_else = []
       , stmt_annot = SpanEmpty }
   where
   forBody :: StatementSpan
   forBody = case comp_for_iter of
                Nothing -> updateStmt
                Just iter -> desugarCompIter updateStmt iter 

desugarCompIter :: StatementSpan -> CompIterSpan -> StatementSpan
desugarCompIter updateStmt (IterFor {..}) =
   desugarCompFor updateStmt comp_iter_for
desugarCompIter updateStmt (IterIf {..}) =
   desugarCompIf updateStmt comp_iter_if

desugarCompIf :: StatementSpan -> CompIfSpan -> StatementSpan
desugarCompIf updateStmt (CompIf {..}) =
   Conditional { cond_guards = guards
               , cond_else = []
               , stmt_annot = SpanEmpty }
   where
   guards :: [(ExprSpan, [StatementSpan])]
   guards = [(comp_if, [conditionBody])]
   conditionBody =
      case comp_if_iter of
         Nothing -> updateStmt
         Just iter -> desugarCompIter updateStmt iter

desugarWith :: StatementSpan -> StatementSpan
desugarWith stmt@(With {..}) =
   case with_context of
      [] -> error $ "with containing no context manager: " ++ prettyText stmt
      [_] -> stmt
      (context1:context2:rest) ->
         With { with_context = [context1]
              , with_body =
                   [ desugarWith $ With { with_context = context2:rest
                                      , with_body = with_body
                                      , stmt_annot = stmt_annot } ]
              , stmt_annot = stmt_annot }
desugarWith other = error $ "desigarWith applied to non with statement: " ++ prettyText other
