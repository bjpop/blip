{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Scope
-- Copyright   : (c) 2012, 2013 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- A variable can be:
--    global
--    local
--    free
--    cellvar 
--
-- Global variables are either:
--    - defined (assigned) at the top level of a module
--    OR
--    - declared global in a nested scope
--
-- Local variables are either (with respect to the current scope) either:
--    - Assigned in the current local scope AND not declared global or non-local.
--    OR
--    - Parameters to a function definition.
--
-- Free variables are (with respect to the current scope):
--    - Local to an enclosing scope AND either:
--        - Declared non-local in the current scope.
--        OR
--        - Read from but not assigned-to in the current local scope. 
--
-- Cellvars are:
--    - Local to the current scope.
--    AND
--    - Free variables of a scope which is nested from the current scope.
--
-- Cellvars are used to implement closures such that modifications to the
-- variable binding itself are visible in the closure. They are implemented
-- as a pointer to a heap allocated cell, which itself points to a Python
-- object. The extra level of indirection allows the cell to be updated to
-- point to something else.
-- 
-- Algorithm:
--    
--   1) Initialise the current scope to be empty.
--
--   2) Walk over all the statements and expressions in the current scope
--      (but not nested scopes) and collect:
--         - variables declared global
--         - variables declared nonlocal
--         - variables which are assigned to in this scope
--         - variables which are read from in this scope
--         - declarations of sub-scopes (lambdas, function defs, classes)
--
--   3) Recursively compute the free-variables and nested scopes of each
--      sub-scope.
--
--   4) Insert the nested scopes computed in step 3) into the current scope.
--
--   5) Compute the locals, globals, free vars and cellvars of the current
--      scope from the information gathered in steps 2) and 3).
--
--   6) Update the current scope with the information computed in step 5.
--
--   7) Return this current scope and this current free variables.
--
-----------------------------------------------------------------------------

module Scope (topScope, renderScope) where

import Types (Identifier, VarSet, DefinitionScope (..), NestedScope (..))
import Data.Set as Set
   ( Set, empty, singleton, fromList, union, unions, difference
   , intersection, toList, size )
import Data.Map as Map (Map, empty, insert, elems, toList)
import Data.List (foldl')
import Language.Python.Common.AST as AST
   ( Statement (..), StatementSpan, Ident (..), Expr (..), ExprSpan
   , Argument (..), ArgumentSpan, RaiseExpr (..), RaiseExprSpan
   , Slice (..), SliceSpan, ModuleSpan, Module (..), ParameterSpan
   , Parameter (..) )
import Data.Monoid (Monoid (..))
import Control.Monad (foldM)
import Control.Monad.RWS.Strict (RWS (..), local, modify, ask, runRWS)
import Text.PrettyPrint.HughesPJ as Pretty
   (Doc, ($$), nest, text, vcat, hsep, ($+$), (<+>), empty, render)
import Blip.Pretty (Pretty (..), prettyString)
import State (emptyDefinitionScope, emptyVarSet)

type ScopeM = RWS EnclosingVars () GlobalVars 

type GlobalVars = VarSet
type LocalVars = VarSet
type CellVars = VarSet
type FreeVars = VarSet
type EnclosingVars = VarSet

instance Pretty NestedScope where
   pretty (NestedScope scope) =
      vcat $ map prettyLocalScope identsScopes
      where
      identsScopes = Map.toList scope
      prettyLocalScope :: (Identifier, (DefinitionScope, NestedScope)) -> Doc
      prettyLocalScope (identifier, (defScope, nestedScope)) =
         text identifier <+> text "->" $$ 
         nest 5 (pretty defScope $$ pretty nestedScope)

instance Pretty DefinitionScope where
   pretty (DefinitionScope {..}) =
      prettyVarSet "locals:" definitionScope_locals $$
      prettyVarSet "freevars:" definitionScope_freeVars $$
      prettyVarSet "cellvars:" definitionScope_cellVars

prettyVarSet :: String -> VarSet -> Doc
prettyVarSet label varSet
   | Set.size varSet == 0 = Pretty.empty
   | otherwise =
        text label <+>
        (hsep $ map text $ Set.toList varSet)

renderScope :: (GlobalVars, NestedScope) -> String
renderScope = render . prettyScope

prettyScope :: (GlobalVars, NestedScope) -> Doc
prettyScope (globals, nestedScope) =
   prettyVarSet "globals:" globals $$
   text "nested scope:" $+$
      (nest 5 $ pretty nestedScope)

-- class, function def or lambda
data Definition = DefStmt StatementSpan | DefLambda ExprSpan

data Usage =
   Usage
   { usage_params :: !VarSet     -- variables which are parameters to the function
   , usage_assigned :: !VarSet   -- variables assigned to (written to) in this scope
   , usage_nonlocals :: !VarSet  -- variables declared nonlocal in this scope
   , usage_globals :: !VarSet    -- variables declared global in this scope
   , usage_referenced :: !VarSet -- variables referred to (read from) in this scope
   , usage_definitions :: ![Definition] -- locally defined lambdas, classes, functions
   }

emptyNestedScope :: NestedScope
emptyNestedScope = NestedScope Map.empty

topScope :: ModuleSpan -> (GlobalVars, NestedScope)
topScope (Module suite) =
   (usage_assigned, nested)
   where
   Usage {..} = varUsage suite
   (nested, _, _) =
      runRWS (foldM nestedScope emptyNestedScope usage_definitions)
             emptyVarSet emptyVarSet

nestedScope :: NestedScope -> Definition -> ScopeM NestedScope
nestedScope (NestedScope scope) (DefStmt (Fun {..})) = do
   let (Usage {..}) = varUsage fun_args `mappend`
                      varUsage fun_body `mappend`
                      varUsage fun_result_annotation
       locals = (usage_assigned `Set.difference` 
                 usage_globals `Set.difference`
                 usage_nonlocals) `Set.union` usage_params
   thisNestedScope <- local (Set.union locals) $
         foldM nestedScope emptyNestedScope usage_definitions
   modify $ Set.union usage_globals
   enclosingScope <- ask
   let 
       -- get all the variables which are free in the top level of
       -- this current nested scope
       nestedFreeVars = nestedScopeFreeVars thisNestedScope
       -- variables which are free in nested scopes and bound in the current scope
       cellVars = locals `Set.intersection` nestedFreeVars
       -- variables which are referenced in the current scope but not local,
       -- or declared nonlocal and are bound in an enclosing scope 
       -- (hence free in the current scope).
       directFreeVars 
          = ((usage_referenced `Set.difference` locals) `Set.union`
              usage_nonlocals) `Set.intersection` enclosingScope
       -- free variables from nested scopes which are not bound in the
       -- current scope, and thus are free in the current scope
       indirectFreeVars = nestedFreeVars `Set.difference` cellVars
       freeVars = directFreeVars `Set.union` indirectFreeVars
       thisDefinitionScope =
          DefinitionScope
          { definitionScope_locals = locals
          , definitionScope_freeVars = freeVars 
          , definitionScope_cellVars = cellVars }
       newScope = Map.insert (fromIdentString fun_name)
                             (thisDefinitionScope, thisNestedScope)
                             scope
   return $ NestedScope newScope

-- Get all the free variables from all the identifiers at the top level
-- of the nested scope.
nestedScopeFreeVars :: NestedScope -> FreeVars
nestedScopeFreeVars (NestedScope scope)
   = Set.unions $ map getFreeVars $ elems scope
   where
   getFreeVars :: (DefinitionScope, NestedScope) -> FreeVars
   getFreeVars (definitionScope, _) = definitionScope_freeVars definitionScope

instance Monoid Usage where
   mempty = Usage
               { usage_params = Set.empty
               , usage_assigned = Set.empty
               , usage_nonlocals = Set.empty
               , usage_globals = Set.empty
               , usage_referenced = Set.empty
               , usage_definitions = [] }
   mappend x y
      = Usage
        { usage_params = usage_params x `mappend` usage_params y
        , usage_assigned = usage_assigned x `mappend` usage_assigned y
        , usage_nonlocals = usage_nonlocals x `mappend` usage_nonlocals y
        , usage_referenced = usage_referenced x `mappend` usage_referenced y
        , usage_globals = usage_globals x `mappend` usage_globals y
        , usage_definitions = usage_definitions x `mappend` usage_definitions y }

fromIdentString :: AST.Ident a -> Identifier
fromIdentString (Ident {..}) = ident_string

-- determine the set of variables which are either assigned to or explicitly
-- declared global or nonlocal in the current scope.
class VarUsage t where
   varUsage :: t -> Usage

instance VarUsage t => VarUsage [t] where
   varUsage = mconcat . Prelude.map varUsage

instance (VarUsage t1, VarUsage t2) => VarUsage (t1, t2) where
   varUsage (x,y) = varUsage x `mappend` varUsage y

instance VarUsage a => VarUsage (Maybe a) where
   varUsage Nothing = mempty
   varUsage (Just x) = varUsage x

instance VarUsage StatementSpan where
   varUsage (While {..})
      = varUsage while_cond `mappend`
        varUsage while_body `mappend`
        varUsage while_else
   varUsage (For {..})
      = mempty { usage_assigned = assignTargets for_targets } `mappend` 
        varUsage for_generator `mappend`
        varUsage for_body `mappend` 
        varUsage for_else
   -- Any varUsage made inside a function body are not collected.
   -- The function name _is_ collected, because it is assigned in the current scope,
   -- likewise for the class name.
   varUsage stmt@(Fun {..})
      = mempty { usage_assigned = singleVarSet fun_name
               , usage_definitions = [DefStmt stmt] }
   varUsage stmt@(Class {..})
      = mempty { usage_assigned = singleVarSet class_name
               , usage_definitions = [DefStmt stmt] }
   varUsage (Conditional {..})
      = varUsage cond_guards `mappend` varUsage cond_else
   varUsage (Assign {..})
      = mempty { usage_assigned = assignTargets assign_to } `mappend`
        varUsage assign_expr
   -- XXX decorators need handling
   varUsage (Decorated {..})
       = varUsage decorated_def
   -- XXX exception handlers need handling
   varUsage (Try {..})
       = varUsage [try_body, try_else, try_finally]
   varUsage (With {..})
      = varUsage with_context `mappend`
        varUsage with_body
   varUsage (Global {..})
      = mempty { usage_globals = Set.fromList $ Prelude.map fromIdentString global_vars }
   varUsage (NonLocal {..})
      = mempty { usage_nonlocals = Set.fromList $ Prelude.map fromIdentString nonLocal_vars }
   varUsage (StmtExpr {..}) = varUsage stmt_expr
   varUsage (Assert {..}) = varUsage assert_exprs
   varUsage (Return {..}) = varUsage return_expr
   varUsage (Raise {..}) = varUsage raise_expr
   varUsage (Delete {..}) = varUsage del_exprs
   varUsage _other = mempty

instance VarUsage RaiseExprSpan where
   varUsage (RaiseV3 maybeExpr) = varUsage maybeExpr

instance VarUsage ExprSpan where
   varUsage (Var {..}) =
      mempty { usage_referenced = singleVarSet var_ident }
   varUsage (Call {..}) =
      varUsage call_fun `mappend` varUsage call_args 
   varUsage (Subscript {..}) =
      varUsage subscriptee `mappend`
      varUsage subscript_expr
   varUsage (SlicedExpr {..}) =
      varUsage slicee `mappend` varUsage slices
   varUsage (CondExpr {..}) =
      varUsage ce_true_branch `mappend`
      varUsage ce_condition `mappend`
      varUsage ce_false_branch
   varUsage (BinaryOp {..}) =
      varUsage left_op_arg `mappend` varUsage right_op_arg
   varUsage (UnaryOp {..}) = varUsage op_arg
   varUsage expr@(Lambda {..}) = mempty { usage_definitions = [DefLambda expr] }
   varUsage (Tuple {..}) = varUsage tuple_exprs
   varUsage (Yield {..}) = varUsage yield_expr 
   varUsage (Generator {..}) = error "generator not supported in varUsage"
   varUsage (ListComp {..}) = error "list comp not supported in varUsage"
   varUsage (List {..}) = varUsage list_exprs
   varUsage (Dictionary {..}) = varUsage dict_mappings
   varUsage (DictComp {..}) = error "dict comp not supported in varUsage"
   varUsage (Set {..}) = varUsage set_exprs
   varUsage (SetComp {..}) = error "set comp not supported in varUsage"
   varUsage (Starred {..}) = varUsage starred_expr
   varUsage (Paren {..}) = varUsage paren_expr
   varUsage _other = mempty

instance VarUsage ArgumentSpan where
   varUsage (ArgExpr {..}) = varUsage arg_expr
   varUsage (ArgVarArgsPos {..}) = varUsage arg_expr
   varUsage (ArgVarArgsKeyword {..}) = varUsage arg_expr
   varUsage (ArgKeyword {..}) = varUsage arg_expr

instance VarUsage SliceSpan where
   varUsage (SliceProper {..}) =
      varUsage slice_lower `mappend`
      varUsage slice_upper `mappend`
      varUsage slice_stride
   varUsage (SliceExpr {..}) = varUsage slice_expr
   varUsage (SliceEllipsis {}) = mempty

instance VarUsage (ParameterSpan) where
   varUsage (Param {..}) = 
      mempty { usage_params = singleVarSet param_name } `mappend`
      varUsage param_py_annotation `mappend`
      varUsage param_default
   varUsage (VarArgsPos {..}) =
      mempty { usage_params = singleVarSet param_name } `mappend`
      varUsage param_py_annotation
   varUsage (VarArgsKeyword {..}) =
      mempty { usage_params = singleVarSet param_name } `mappend`
      varUsage param_py_annotation
   varUsage _other = mempty 

-- Collect all the variables which are assigned to in a list of expressions (patterns).
-- XXX Incomplete
assignTargets :: [ExprSpan] -> VarSet
assignTargets = foldl' addTarget mempty
   where
   addTarget :: VarSet -> ExprSpan -> VarSet
   addTarget set expr = Set.union (exprVars expr) set
   exprVars :: ExprSpan -> VarSet
   exprVars (Var {..}) = singleVarSet var_ident
   exprVars (List {..}) = Set.unions $ Prelude.map exprVars list_exprs
   exprVars (Tuple {..}) = Set.unions $ Prelude.map exprVars tuple_exprs
   exprVars (Paren {..}) = exprVars paren_expr
   exprVars _other = Set.empty

singleVarSet :: AST.Ident a -> VarSet
singleVarSet = Set.singleton . fromIdentString
