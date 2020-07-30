{-# LANGUAGE CPP #-}

module Blip.Interpreter.HashTable.Array
  ( MutableArray
  , newArray
  , readArray
  , writeArray
  ) where


-- import           Control.Monad.ST
#ifdef BOUNDS_CHECKING
import qualified Data.Vector.Mutable as M
import           Data.Vector.Mutable (MVector)
#else
import qualified Data.Primitive.Array as M
-- import           Data.Primitive.Array (MutableArray)
#endif
import Control.Monad.Primitive (RealWorld)


#ifdef BOUNDS_CHECKING

type MutableArray a = MVector RealWorld a

newArray :: Int -> a -> IO (MutableArray a)
newArray = M.replicate

readArray :: MutableArray a -> Int -> IO a
readArray = M.read

writeArray :: MutableArray a -> Int -> a -> IO ()
writeArray = M.write

#else

type MutableArray a = M.MutableArray RealWorld a

newArray :: Int -> a -> IO (MutableArray a)
newArray = M.newArray

readArray :: MutableArray a -> Int -> IO a
readArray = M.readArray

writeArray :: MutableArray a -> Int -> a -> IO ()
writeArray = M.writeArray

#endif
