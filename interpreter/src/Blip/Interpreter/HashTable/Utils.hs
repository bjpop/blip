{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE MagicHash    #-}

module Blip.Interpreter.HashTable.Utils
  ( whichBucket
  , nextBestPrime
  , bumpSize
  , shiftL
  , shiftRL
  , iShiftL
  , iShiftRL
  , nextHighestPowerOf2
  , log2
  , highestBitMask
  , wordSize
  , cacheLineSize
  , numElemsInCacheLine
  , cacheLineIntMask
  , cacheLineIntBits
  , forceSameType
  ) where

import           Data.Bits                        hiding (shiftL)
import           Blip.Interpreter.HashTable.IntArray (Elem)
import           Data.Vector                      (Vector)
import qualified Data.Vector                      as V
#if __GLASGOW_HASKELL__ >= 503
import           GHC.Exts
#else
import qualified Data.Bits
import           Data.Word
#endif


------------------------------------------------------------------------------
wordSize :: Int
wordSize = bitSize (0::Int)


cacheLineSize :: Int
cacheLineSize = 64


numElemsInCacheLine :: Int
numElemsInCacheLine = z
  where
    !z = cacheLineSize `div` (bitSize (0::Elem) `div` 8)


-- | What you have to mask an integer index by to tell if it's
-- cacheline-aligned
cacheLineIntMask :: Int
cacheLineIntMask = z
  where
    !z = numElemsInCacheLine - 1


cacheLineIntBits :: Int
cacheLineIntBits = log2 $ toEnum numElemsInCacheLine


------------------------------------------------------------------------------
{-# INLINE whichBucket #-}
whichBucket :: Int -> Int -> Int
whichBucket !h !sz = o
  where
    !o = h `mod` sz


------------------------------------------------------------------------------
binarySearch :: (Ord e) => Vector e -> e -> Int
binarySearch = binarySearchBy compare
{-# INLINE binarySearch #-}


------------------------------------------------------------------------------
binarySearchBy :: (e -> e -> Ordering)
               -> Vector e
               -> e
               -> Int
binarySearchBy cmp vec e = binarySearchByBounds cmp vec e 0 (V.length vec)
{-# INLINE binarySearchBy #-}


------------------------------------------------------------------------------
binarySearchByBounds :: (e -> e -> Ordering)
                     -> Vector e
                     -> e
                     -> Int
                     -> Int
                     -> Int
binarySearchByBounds cmp vec e = loop
 where
 loop !l !u
   | u <= l    = l
   | otherwise = let e' = V.unsafeIndex vec k
                 in case cmp e' e of
                      LT -> loop (k+1) u
                      EQ -> k
                      GT -> loop l     k
  where k = (u + l) `shiftR` 1
{-# INLINE binarySearchByBounds #-}


------------------------------------------------------------------------------
primeSizes :: Vector Integer
primeSizes = V.fromList [ 19
                        , 31
                        , 37
                        , 43
                        , 47
                        , 53
                        , 61
                        , 67
                        , 79
                        , 89
                        , 97
                        , 107
                        , 113
                        , 127
                        , 137
                        , 149
                        , 157
                        , 167
                        , 181
                        , 193
                        , 211
                        , 233
                        , 257
                        , 281
                        , 307
                        , 331
                        , 353
                        , 389
                        , 409
                        , 421
                        , 443
                        , 467
                        , 503
                        , 523
                        , 563
                        , 593
                        , 631
                        , 653
                        , 673
                        , 701
                        , 733
                        , 769
                        , 811
                        , 877
                        , 937
                        , 1039
                        , 1117
                        , 1229
                        , 1367
                        , 1543
                        , 1637
                        , 1747
                        , 1873
                        , 2003
                        , 2153
                        , 2311
                        , 2503
                        , 2777
                        , 3079
                        , 3343
                        , 3697
                        , 5281
                        , 6151
                        , 7411
                        , 9901
                        , 12289
                        , 18397
                        , 24593
                        , 34651
                        , 49157
                        , 66569
                        , 73009
                        , 98317
                        , 118081
                        , 151051
                        , 196613
                        , 246011
                        , 393241
                        , 600011
                        , 786433
                        , 1050013
                        , 1572869
                        , 2203657
                        , 3145739
                        , 4000813
                        , 6291469
                        , 7801379
                        , 10004947
                        , 12582917
                        , 19004989
                        , 22752641
                        , 25165843
                        , 39351667
                        , 50331653
                        , 69004951
                        , 83004629
                        , 100663319
                        , 133004881
                        , 173850851
                        , 201326611
                        , 293954587
                        , 402653189
                        , 550001761
                        , 702952391
                        , 805306457
                        , 1102951999
                        , 1402951337
                        , 1610612741
                        , 1902802801
                        , 2147483647
                        , 3002954501
                        , 3902954959
                        , 4294967291
                        , 5002902979
                        , 6402754181
                        , 8589934583
                        , 17179869143
                        , 34359738337
                        , 68719476731
                        , 137438953447
                        , 274877906899 ]


------------------------------------------------------------------------------
nextBestPrime :: Int -> Int
nextBestPrime x = fromEnum yi
  where
    xi  = toEnum x
    idx = binarySearch primeSizes xi
    yi  = V.unsafeIndex primeSizes idx


------------------------------------------------------------------------------
bumpSize :: Double -> Int -> Int
bumpSize !maxLoad !s = nextBestPrime $! ceiling (fromIntegral s / maxLoad)


------------------------------------------------------------------------------
shiftL :: Word -> Int -> Word
shiftRL :: Word -> Int -> Word
iShiftL  :: Int -> Int -> Int
iShiftRL  :: Int -> Int -> Int
#if __GLASGOW_HASKELL__
{--------------------------------------------------------------------
  GHC: use unboxing to get @shiftRL@ inlined.
--------------------------------------------------------------------}
{-# INLINE shiftL #-}
shiftL (W# x) (I# i)
  = W# (shiftL# x i)

{-# INLINE shiftRL #-}
shiftRL (W# x) (I# i)
  = W# (shiftRL# x i)

{-# INLINE iShiftL #-}
iShiftL (I# x) (I# i)
  = I# (iShiftL# x i)

{-# INLINE iShiftRL #-}
iShiftRL (I# x) (I# i)
  = I# (iShiftRL# x i)

#else
shiftL x i    = Data.Bits.shiftL x i
shiftRL x i   = shiftR x i
iShiftL x i   = shiftL x i
iShiftRL x i  = shiftRL x i
#endif


------------------------------------------------------------------------------
{-# INLINE nextHighestPowerOf2 #-}
nextHighestPowerOf2 :: Word -> Word
nextHighestPowerOf2 w = highestBitMask (w-1) + 1


------------------------------------------------------------------------------
log2 :: Word -> Int
log2 w = go (nextHighestPowerOf2 w) 0
  where
    go 0 !i  = i-1
    go !n !i = go (shiftRL n 1) (i+1)


------------------------------------------------------------------------------
{-# INLINE highestBitMask #-}
highestBitMask :: Word -> Word
highestBitMask !x0 = case (x0 .|. shiftRL x0 1) of
                      x1 -> case (x1 .|. shiftRL x1 2) of
                       x2 -> case (x2 .|. shiftRL x2 4) of
                        x3 -> case (x3 .|. shiftRL x3 8) of
                         x4 -> case (x4 .|. shiftRL x4 16) of
                          x5 -> x5 .|. shiftRL x5 32


------------------------------------------------------------------------------
forceSameType :: Monad m => a -> a -> m ()
forceSameType _ _ = return ()
{-# INLINE forceSameType #-}
