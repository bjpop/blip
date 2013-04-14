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

module Desugar (desugarComprehension, CompUpdater) where

import Prelude hiding (mapM)
import Utils (mkVar, mkReturn, mkIdent)
import Language.Python.Common.AST as AST
   ( StatementSpan (..), Statement (..)
   , ExprSpan (..), Expr (..), Ident (..), IdentSpan
   , SuiteSpan, Comprehension (..), ComprehensionSpan, CompFor (..)
   , CompForSpan, CompIf (..), CompIfSpan, CompIter (..), CompIterSpan) 
import Language.Python.Common.SrcLocation (SrcSpan (..))
import Language.Python.Common (prettyText)

type CompUpdater = ExprSpan -> StatementSpan

-- XXX need to generalise to dict comprehenions
desugarComprehension :: StatementSpan -> CompUpdater -> ComprehensionSpan ExprSpan -> [StatementSpan]
desugarComprehension initStmt updater comp@(Comprehension {..}) =
   [ initStmt, forLoop, returnStmt ]  
   where
   funName = mkIdent "$comprehension"
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
