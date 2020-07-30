{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE MagicHash    #-}

module Blip.Interpreter.HashTable.IntArray
  ( IntArray
  , Elem
  , elemMask
  , primWordToElem
  , elemToInt
  , elemToInt#
  , newArray
  , readArray
  , writeArray
  , length
  , toPtr
  ) where

------------------------------------------------------------------------------
-- import           Control.Monad.ST
import           Data.Bits
import qualified Data.Primitive.ByteArray as A
-- import           Data.Primitive.Types     (Addr (..))
import           GHC.Exts
import           GHC.Word
import           Data.Word (Word8)
import           Prelude                  hiding (length)
-- import           Control.Monad.Primitive (RealWorld)
------------------------------------------------------------------------------


#ifdef BOUNDS_CHECKING
#define BOUNDS_MSG(sz,i) concat [ "[", __FILE__, ":",                         \
                                  show (__LINE__ :: Int),                     \
                                  "] bounds check exceeded: ",                \
                                  "size was ", show (sz), " i was ", show (i) ]

#define BOUNDS_CHECK(arr,i) let sz = (A.sizeofMutableByteArray (arr)          \
                                      `div` wordSizeInBytes) in               \
                            if (i) < 0 || (i) >= sz                           \
                              then error (BOUNDS_MSG(sz,(i)))                 \
                              else return ()
#else
#define BOUNDS_CHECK(arr,i)
#endif


------------------------------------------------------------------------------
newtype IntArray = IA (A.MutableByteArray RealWorld)
type Elem = Word16


------------------------------------------------------------------------------
primWordToElem :: Word# -> Elem
primWordToElem = W16#


------------------------------------------------------------------------------
elemToInt :: Elem -> Int
elemToInt e = let !i# = elemToInt# e
              in (I# i#)


------------------------------------------------------------------------------
elemToInt# :: Elem -> Int#
elemToInt# (W16# w#) = word2Int# w#


------------------------------------------------------------------------------
elemMask :: Int
elemMask = 0xffff


------------------------------------------------------------------------------
wordSizeInBytes :: Int
wordSizeInBytes = bitSize (0::Elem) `div` 8


------------------------------------------------------------------------------
-- | Cache line size, in bytes
cacheLineSize :: Int
cacheLineSize = 64


------------------------------------------------------------------------------
newArray :: Int -> IO IntArray
newArray n = do
    let !sz = n * wordSizeInBytes
    !arr <- A.newAlignedPinnedByteArray sz cacheLineSize
    A.fillByteArray arr 0 sz 0
    return $! IA arr


------------------------------------------------------------------------------
readArray :: IntArray -> Int -> IO Elem
readArray (IA a) idx = do
    BOUNDS_CHECK(a,idx)
    A.readByteArray a idx


------------------------------------------------------------------------------
writeArray :: IntArray -> Int -> Elem -> IO ()
writeArray (IA a) idx val = do
    BOUNDS_CHECK(a,idx)
    A.writeByteArray a idx val


------------------------------------------------------------------------------
length :: IntArray -> Int
length (IA a) = A.sizeofMutableByteArray a `div` wordSizeInBytes


------------------------------------------------------------------------------
toPtr :: IntArray -> Ptr Word8 
toPtr (IA a) = A.mutableByteArrayContents a 
{-
toPtr (IA a) = Ptr a#
  where
    !(Addr !a#) = A.mutableByteArrayContents a
-}
