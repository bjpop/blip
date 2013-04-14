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
-----------------------------------------------------------------------------

module Desugar (desugarComprehension) where

import Prelude hiding (mapM)
import Utils (mkVar, mkReturn, mkIdent)
import Language.Python.Common.AST as AST
   ( StatementSpan (..), Statement (..)
   , ExprSpan (..), Expr (..), Ident (..), IdentSpan
   , SuiteSpan, Comprehension (..), ComprehensionSpan, CompFor (..)
   , CompForSpan, CompIf (..), CompIfSpan, CompIter (..), CompIterSpan) 
import Language.Python.Common.SrcLocation (SrcSpan (..))
import Language.Python.Common (prettyText)

type UpdateMaker = ExprSpan -> StatementSpan

-- XXX need to generalise to dict comprehenions
desugarComprehension :: StatementSpan -> UpdateMaker -> ComprehensionSpan ExprSpan -> StatementSpan
desugarComprehension initStmt updater comp@(Comprehension {..}) =
   Fun { fun_name = funName
       , fun_args = []
       , fun_result_annotation = Nothing
       , fun_body = funBody
       , stmt_annot = comprehension_annot }
   where
   funName = mkIdent "$comprehension"
   resultName = mkIdent "$result"
   funBody = [ initStmt, forLoop, returnStmt ]  
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
