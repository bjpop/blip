{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Blip.Compiler.Desugar 
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

module Blip.Compiler.Desugar (desugarComprehension, resultName, desugarWith) where

import Prelude hiding (mapM)
import Blip.Compiler.Utils (mkIdent)
import Language.Python.Common.AST as AST
   ( StatementSpan, Statement (..), ExprSpan, Comprehension (..)
   , ComprehensionSpan, ComprehensionExprSpan, CompFor (..), CompForSpan, CompIf (..), CompIfSpan
   , CompIter (..), CompIterSpan, IdentSpan ) 
import Language.Python.Common.SrcLocation (SrcSpan (..))
import Language.Python.Common (prettyText)

{-

Desugaring of comprehensions.

Comprehensions are desugared into functions containing for loops.
We use a function because the local variables for a for loop are not
in scope outside the loop (unlike Python's actual for loops). Putting
the loop inside the function gives us the desired scope behaviour.

For example:

    [ x + 1 for x in y if x > 2 ]

becomes:

    def f():
        $result = []
        for x in y:
            if x > 2:
                $result.append(x)
        return $result
    f()

In practice we don't need to generate a name for our function, because
we can just make a function byte code object and then call it directly.

A problem with the above scheme occurs when we have list comprehensions
in the body of a class, which refer to other variables local to the class:

    class C():
        a = [1,2,3]
        b = [ x + 1 for x in a ]

The "obvious" way to desugar that is:

    class C():
        a = [1,2,3]
        def f():
            $result = []
            for x in a:
                $result.append(x)
            return $result
        b = f()

The problem is that the variable 'a' is free in the definition of f.
The scope rules of classes do not allow 'a' to be in scope inside
functions defined in the class (this is different than normal
nested functions). We'd have to refer to the variable as 'C.a'.

We could use the class name to qualify the scope of such free variables.
But another, perhaps simpler way is to provide them as arguments to 
the new function:

    class C():
        a = [1,2,3]
        def f(a):
            $result = []
            for x in a:
                $result.append(x)
            return $result
        b = f(a)

-}

-- Special free variable which cannot appear in the source of the program
-- and is guaranteed to be unique in the comprehension.
-- Nested comprehensions get desugared into nested functions so there
-- is no danger of a name clash.
resultName :: IdentSpan 
resultName = mkIdent "$result"

desugarComprehension
   :: [StatementSpan]      -- Initialiser of the stmt (e.g. $result = [])
   -> (ComprehensionExprSpan -> StatementSpan) -- Update the accumulator (e.g. $result.append(x)) 
   -> [StatementSpan]      -- Return the accumulator (e.g. return $result)
   -> ComprehensionSpan    -- Comprehension to desugar
   -> [StatementSpan]      -- Body of the desugared function
desugarComprehension initStmt updater returnStmt (Comprehension {..}) =
   initStmt ++ [forLoop] ++ returnStmt
   where
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
