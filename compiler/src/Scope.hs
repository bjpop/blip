module Scope (Scope (..), empty) where

data Scope = Scope {}
    deriving (Eq, Show)

empty :: Scope
empty = Scope {}
