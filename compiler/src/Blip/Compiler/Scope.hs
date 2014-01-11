{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, RecordWildCards, PatternGuards, ExistentialQuantification #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Blip.Compiler.Scope
-- Copyright   : (c) 2012, 2013, 2014 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- A variable can be:
--    explicit global
--    implicit global
--    local
--    free
--    cellvar 
--
-- Global variables are either:
--    - defined (assigned) at the top level of a module
--    OR
--    - declared global in a nested scope
--
-- Local variables are (with respect to the current scope) either:
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
-----------------------------------------------------------------------------

module Blip.Compiler.Scope
   (topScope, renderScope)
   where

import Blip.Compiler.Types
   ( Identifier, VarSet, LocalScope (..)
   , NestedScope (..), ScopeIdentifier, ParameterTypes (..) )
import Data.Set as Set
   ( empty, singleton, fromList, union, difference
   , intersection, toList, size )
import Data.Map as Map (empty, insert, toList, union)
import Data.List (foldl', intersperse)
import Language.Python.Common.AST as AST
   ( Statement (..), StatementSpan, Ident (..), Expr (..), ExprSpan
   , Argument (..), ArgumentSpan, RaiseExpr (..), RaiseExprSpan
   , Slice (..), SliceSpan, ModuleSpan, Module (..), ParameterSpan
   , Parameter (..), Op (..), Comprehension (..), ComprehensionSpan
   , CompIter (..), CompIterSpan, CompFor (..), CompForSpan, CompIf (..)
   , CompIfSpan, Handler (..), HandlerSpan, ExceptClause (..), ExceptClauseSpan  )
import Data.Monoid (Monoid (..))
import Control.Monad (mapAndUnzipM)
import Control.Monad.Reader (ReaderT, local, ask, runReaderT)
import Text.PrettyPrint.HughesPJ as Pretty
   ( Doc, ($$), nest, text, vcat, hsep, ($+$), (<+>), empty
   , render, parens, comma, int, hcat )
import Blip.Pretty (Pretty (..))
import Blip.Compiler.State (emptyVarSet, emptyParameterTypes)
import Blip.Compiler.Utils ( identsFromParameters, spanToScopeIdentifier
             , fromIdentString, maybeToList )

type ScopeM a = ReaderT VarSet IO a

instance Pretty ScopeIdentifier where
   pretty (row1, col1, row2, col2) =
      parens $ hcat $ intersperse comma $ map int [row1, col1, row2, col2]

instance Pretty NestedScope where
   pretty (NestedScope scope) =
      vcat $ map prettyLocalScope identsScopes
      where
      identsScopes = Map.toList scope
      prettyLocalScope :: (ScopeIdentifier, (String, LocalScope)) -> Doc
      prettyLocalScope (span, (identifier, defScope)) =
         text identifier <+> pretty span <+> text "->" $$ 
         nest 5 (pretty defScope)

instance Pretty LocalScope where
   pretty (LocalScope {..}) =
      text "params:" <+> (nest 5 $ pretty localScope_params) $$
      prettyVarSet "locals:" localScope_locals $$
      prettyVarSet "freevars:" localScope_freeVars $$
      prettyVarSet "cellvars:" localScope_cellVars $$
      prettyVarSet "globals:" localScope_explicitGlobals

instance Pretty ParameterTypes where
   pretty (ParameterTypes {..}) =
      prettyVarList "positional:" parameterTypes_pos $$
      prettyVarList "varArgPos:" (maybeToList parameterTypes_varPos) $$
      prettyVarList "varArgKeyword:" (maybeToList parameterTypes_varKeyword)

prettyVarList :: String -> [Identifier] -> Doc
prettyVarList label list 
   | length list == 0 = Pretty.empty
   | otherwise =
        text label <+> (hsep $ map text list)

prettyVarSet :: String -> VarSet -> Doc
prettyVarSet label varSet
   | Set.size varSet == 0 = Pretty.empty
   | otherwise =
        text label <+>
        (hsep $ map text $ Set.toList varSet)

renderScope :: NestedScope -> String
renderScope = render . prettyScope

prettyScope :: NestedScope -> Doc
prettyScope nestedScope =
   text "nested scope:" $+$
      (nest 5 $ pretty nestedScope)

-- class, function, lambda, or comprehension
data Definition
   = DefStmt StatementSpan -- class, or def
   | DefLambda ExprSpan -- lambda
   | forall e . VarUsage e => DefComprehension (ComprehensionSpan e) -- comprehension

data Usage =
   Usage
   { usage_assigned :: !VarSet     -- variables assigned to (written to) in this scope
   , usage_nonlocals :: !VarSet    -- variables declared nonlocal in this scope
   , usage_globals :: !VarSet      -- variables declared global in this scope
   , usage_referenced :: !VarSet   -- variables referred to (read from) in this scope
   , usage_definitions :: ![Definition] -- locally defined lambdas, classes, functions, comprehensions
   }

emptyNestedScope :: NestedScope
emptyNestedScope = NestedScope Map.empty

-- returns the 'local' scope of the top-level of the module and
-- the nested scope of the module (anything not at the top level)
topScope :: ModuleSpan -> IO (LocalScope, NestedScope)
topScope (Module suite) = do
   -- XXX should check that nothing was declared global at the top level
   let Usage {..} = varUsage suite
       moduleLocals =
          LocalScope
          { localScope_params = emptyParameterTypes
          , localScope_locals = usage_assigned
          , localScope_freeVars = Set.empty
          , localScope_cellVars = Set.empty
          , localScope_explicitGlobals = Set.empty }
   (nested, _freeVars) <- runReaderT (foldNestedScopes usage_definitions) emptyVarSet
   return (moduleLocals, nested)

insertNestedScope :: ScopeIdentifier -> (String, LocalScope) -> NestedScope -> NestedScope
insertNestedScope key value (NestedScope scope) = 
   NestedScope $ Map.insert key value scope 

joinNestedScopes :: NestedScope -> NestedScope -> NestedScope
joinNestedScopes (NestedScope scope1) (NestedScope scope2)
   = NestedScope $ Map.union scope1 scope2

joinVarSets :: VarSet -> VarSet -> VarSet
joinVarSets = Set.union

foldNestedScopes :: [Definition] -> ScopeM (NestedScope, VarSet)
foldNestedScopes defs = do
   (scopes, vars) <- mapAndUnzipM buildNestedScope defs
   let joinedScopes = foldl' joinNestedScopes emptyNestedScope scopes
       joinedVars = foldl' joinVarSets emptyVarSet vars
   seq joinedScopes $ seq joinedVars $ return (joinedScopes, joinedVars)

buildNestedScope :: Definition -> ScopeM (NestedScope, VarSet)
buildNestedScope (DefStmt (Fun {..})) = do
   let usage = varUsage fun_body `mappend`
               varUsage fun_result_annotation
       parameterTypes = parseParameterTypes fun_args
   functionNestedScope usage parameterTypes
      (spanToScopeIdentifier stmt_annot) $ fromIdentString fun_name
buildNestedScope (DefLambda (Lambda {..})) = do
   let usage = varUsage lambda_body
       parameterTypes = parseParameterTypes lambda_args
   functionNestedScope usage parameterTypes
      (spanToScopeIdentifier expr_annot) "<lambda>" 

buildNestedScope (DefComprehension (Comprehension {..})) = do
   -- we introduce a new local variable called $result when compiling
   -- comprehensions, when they are desugared into functions
   let resultVarSet = Set.singleton "$result"
       usage = mempty { usage_assigned = resultVarSet
                      , usage_referenced = resultVarSet } `mappend`
               varUsage comprehension_expr `mappend`
               varUsage comprehension_for
   -- Comprehensions are turned into functions whose parameters are the
   -- variables which are free in the comprehension. This is equal
   -- to the variables which are referenced but not assigned.
       parameters = usage_referenced usage `Set.difference` usage_assigned usage
       parameterTypes = emptyParameterTypes { parameterTypes_pos = Set.toList parameters } 
   functionNestedScope usage parameterTypes
      (spanToScopeIdentifier comprehension_annot) "<comprehension>" 

{-
   Classes can have freeVars, but they don't have cellVars.

   We have a problem where a class can have a free variable with the same
   name as a "locally" defined variable. 

	def f():
	   y = 3
	   class C():
	      y = 5
	      def g():
		 nonlocal y
		 print(y)

   The g() method of the C() class prints the value 3, because its free
   variable y is bound in the body of f, not in the class definition.

   The bases of a class are actually in the enclosing scope of the class
   definition.

   We record both instances of the variable, and are careful to disambiguate
   when the variables are looked-up in the scope during compilation.
-}

buildNestedScope (DefStmt (Class {..})) = do
   let Usage {..} = varUsage class_body 
       locals = usage_assigned
   (thisNestedScope, nestedFreeVars) <- foldNestedScopes usage_definitions
   enclosingScope <- ask
   let directFreeVars 
          = ((usage_referenced `Set.difference` locals) `Set.union`
              usage_nonlocals) `Set.intersection` enclosingScope
       freeVars = directFreeVars `Set.union` nestedFreeVars
   let thisLocalScope =
          LocalScope
          { localScope_params = emptyParameterTypes
          , localScope_locals = locals
          , localScope_freeVars = freeVars 
          , localScope_cellVars = Set.empty
          , localScope_explicitGlobals = usage_globals }
   let newScope =
          insertNestedScope (spanToScopeIdentifier stmt_annot)
             (fromIdentString class_name, thisLocalScope)
             thisNestedScope
   return (newScope, freeVars)

buildNestedScope _def =
   error $ "buildNestedScope called on unexpected definition"

functionNestedScope :: Usage 
                    -> ParameterTypes 
                    -> ScopeIdentifier 
                    -> String 
                    -> ScopeM (NestedScope, VarSet)
functionNestedScope (Usage {..}) parameters scopeIdentifier name = do
   let locals = (usage_assigned `Set.difference` 
                 usage_globals `Set.difference`
                 usage_nonlocals) `Set.union` 
                 (Set.fromList $ identsFromParameters parameters)
   (thisNestedScope, nestedFreeVars) <-
      local (Set.union locals) $ foldNestedScopes usage_definitions
   enclosingScope <- ask
   let -- get all the variables which are free in the top level of
       -- this current nested scope
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
       thisLocalScope =
          LocalScope
          { localScope_params = parameters 
          , localScope_locals = locals
          , localScope_freeVars = freeVars 
          , localScope_cellVars = cellVars
          , localScope_explicitGlobals = usage_globals }
   let newScope =
          insertNestedScope scopeIdentifier (name, thisLocalScope) thisNestedScope 
   return (newScope, freeVars)

-- separate the positional parameters from the positional varargs and the
-- keyword varargs
parseParameterTypes :: [ParameterSpan] -> ParameterTypes
parseParameterTypes = parseAcc [] Nothing Nothing
   where
   parseAcc :: [Identifier] -> Maybe Identifier -> Maybe Identifier -> [ParameterSpan] -> ParameterTypes
   parseAcc pos varPos varKeyword [] =
      ParameterTypes { parameterTypes_pos = reverse pos
                     , parameterTypes_varPos = varPos
                     , parameterTypes_varKeyword = varKeyword }
   parseAcc pos varPos varKeyword (param:rest) =
      case param of
         Param {..} -> parseAcc (fromIdentString param_name : pos) varPos varKeyword rest 
         VarArgsPos {..} -> parseAcc pos (Just $ fromIdentString param_name) varKeyword rest
         VarArgsKeyword {..} -> parseAcc pos varPos (Just $ fromIdentString param_name) rest
         _other -> parseAcc pos varPos varKeyword rest

instance Monoid Usage where
   mempty = Usage
               { usage_assigned = Set.empty
               , usage_nonlocals = Set.empty
               , usage_globals = Set.empty
               , usage_referenced = Set.empty
               , usage_definitions = [] }
   mappend x y
      = Usage
        { usage_assigned = usage_assigned x `mappend` usage_assigned y
        , usage_nonlocals = usage_nonlocals x `mappend` usage_nonlocals y
        , usage_referenced = usage_referenced x `mappend` usage_referenced y
        , usage_globals = usage_globals x `mappend` usage_globals y
        , usage_definitions = usage_definitions x `mappend` usage_definitions y }

instance Monoid ParameterTypes where
   mempty =
      ParameterTypes
      { parameterTypes_pos = []
      , parameterTypes_varPos = Nothing
      , parameterTypes_varKeyword = Nothing
      }

   mappend (ParameterTypes pos1 varPos1 varKeyword1)
           (ParameterTypes pos2 varPos2 varKeyword2)
      = ParameterTypes (pos1 `mappend` pos2)
                       (varPos1 `mappend` varPos2)
                       (varKeyword1 `mappend` varKeyword2)

-- determine the set of variables which are either assigned to or explicitly
-- declared global or nonlocal in the current scope.
class VarUsage t where
   varUsage :: t -> Usage

instance VarUsage t => VarUsage [t] where
   varUsage = mconcat . Prelude.map varUsage

instance (VarUsage t1, VarUsage t2) => VarUsage (t1, t2) where
   varUsage (x, y) = varUsage x `mappend` varUsage y

instance VarUsage a => VarUsage (Maybe a) where
   varUsage Nothing = mempty
   varUsage (Just x) = varUsage x

instance VarUsage StatementSpan where
   varUsage (While {..})
      = varUsage while_cond `mappend`
        varUsage while_body `mappend`
        varUsage while_else
   varUsage (For {..})
      = varUsage (AssignTargets $ for_targets) `mappend` 
        varUsage for_generator `mappend`
        varUsage for_body `mappend` 
        varUsage for_else
   -- Any varUsage made inside a function body are not collected.
   -- The function name _is_ collected, because it is assigned in the current scope,
   -- likewise for the class name.
   varUsage stmt@(Fun {..})
      = mempty { usage_assigned = singleVarSet fun_name
               , usage_definitions = [DefStmt stmt] }
   -- the bases of the Class are referenced within the scope that defines the class
   -- as opposed to being referenced in the body of the class
   varUsage stmt@(Class {..})
      = mempty { usage_assigned = singleVarSet class_name
               , usage_definitions = [DefStmt stmt] } `mappend`
        varUsage class_args
   varUsage (Conditional {..})
      = varUsage cond_guards `mappend` varUsage cond_else
   varUsage (Assign {..})
      = varUsage (AssignTargets assign_to) `mappend` varUsage assign_expr
   varUsage (AugmentedAssign {..})
      = varUsage [aug_assign_to] `mappend` varUsage aug_assign_expr
   varUsage (Decorated {..})
       = varUsage decorated_def
   varUsage (Try {..})
       = varUsage try_body `mappend` varUsage  try_excepts `mappend`
         varUsage try_else `mappend`  varUsage try_finally
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

instance VarUsage HandlerSpan where
   varUsage (Handler {..}) = varUsage handler_clause `mappend` varUsage handler_suite

instance VarUsage ExceptClauseSpan where
   varUsage (ExceptClause {..}) =
      case except_clause of
         Nothing -> mempty
         Just (except, maybeAs) ->
            case maybeAs of
               Nothing -> varUsage except
               Just asName -> varUsage except `mappend` (varUsage $ AssignTargets [asName])

instance VarUsage RaiseExprSpan where
   varUsage (RaiseV3 maybeExpr) = varUsage maybeExpr
   -- the parser should never generate the following, but we need
   -- code to make non-exhaustive pattern warnings go away.
   varUsage _other = error $ "varUsage on Python version 2 style raise statement"

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
   -- if it is a dot operator then the right argument must be a global name
   -- but it is not defined in this module so we can ignore it
   varUsage (BinaryOp {..})
      | Dot {} <- operator = varUsage left_op_arg 
      | otherwise = varUsage left_op_arg `mappend` varUsage right_op_arg
   varUsage (UnaryOp {..}) = varUsage op_arg
   varUsage expr@(Lambda {..}) = mempty { usage_definitions = [DefLambda expr] }
   varUsage (Tuple {..}) = varUsage tuple_exprs
   varUsage (Yield {..}) = varUsage yield_expr 
   varUsage (Generator {..}) =
      mempty { usage_definitions = [DefComprehension gen_comprehension] }
   varUsage (ListComp {..}) =
      mempty { usage_definitions = [DefComprehension list_comprehension] }
   varUsage (List {..}) = varUsage list_exprs
   varUsage (Dictionary {..}) = varUsage dict_mappings
   varUsage (DictComp {..}) = 
      mempty { usage_definitions = [DefComprehension dict_comprehension] }
   varUsage (Set {..}) = varUsage set_exprs
   varUsage (SetComp {..}) =
      mempty { usage_definitions = [DefComprehension set_comprehension] } 
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

instance VarUsage a => VarUsage (ComprehensionSpan a) where
   varUsage (Comprehension {..}) = 
      varUsage comprehension_expr `mappend`
      varUsage comprehension_for

instance VarUsage CompForSpan where
   varUsage (CompFor {..}) = 
      varUsage (AssignTargets comp_for_exprs) `mappend` 
      varUsage comp_in_expr `mappend`
      varUsage comp_for_iter

instance VarUsage CompIterSpan where
   varUsage (IterFor {..}) = varUsage comp_iter_for
   varUsage (IterIf {..}) = varUsage comp_iter_if

instance VarUsage CompIfSpan where
   varUsage (CompIf {..}) = 
      varUsage comp_if `mappend`
      varUsage comp_if_iter

newtype AssignTargets = AssignTargets [ExprSpan]

-- Collect all the variables which are assigned to in a list of expressions (patterns).
-- XXX we should support starred assign targets.
instance VarUsage AssignTargets where
   varUsage (AssignTargets exprs) = foldl' addUsage mempty exprs
      where
      addUsage :: Usage -> ExprSpan -> Usage 
      addUsage usage expr = targetUsage expr `mappend` usage
      targetUsage :: ExprSpan -> Usage
      targetUsage (Var {..}) = mempty { usage_assigned = singleVarSet var_ident }
      targetUsage (List {..}) = varUsage $ AssignTargets list_exprs 
      targetUsage (Tuple {..}) = varUsage $ AssignTargets tuple_exprs
      targetUsage (Paren {..}) = targetUsage paren_expr
      -- all variables mentioned in a subscript, attribute lookup
      -- and sliced expr are read from, not written to
      targetUsage expr@(Subscript {..}) = varUsage expr
      targetUsage expr@(BinaryOp{..}) = varUsage expr
      targetUsage expr@(SlicedExpr{..}) = varUsage expr
      targetUsage other = error $ "Unsupported assignTarget: " ++ show other

singleVarSet :: AST.Ident a -> VarSet
singleVarSet = Set.singleton . fromIdentString
