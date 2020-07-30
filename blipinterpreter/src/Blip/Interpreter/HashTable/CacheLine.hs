{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}

module Blip.Interpreter.HashTable.CacheLine
  ( cacheLineSearch
  , cacheLineSearch2
  , cacheLineSearch3
  , forwardSearch2
  , forwardSearch3
  , isCacheLineAligned
  , advanceByCacheLineSize
  , prefetchRead
  , prefetchWrite
  , bl_abs#
  , sign#
  , mask#
  , maskw#
  ) where

import           Control.Monad

-- XXX reconsider this?
#define NO_C_SEARCH

#if MIN_VERSION_base(4,4,0)
-- import           Control.Monad.ST                 (ST)
-- import           Control.Monad.ST.Unsafe
#else
-- import           Control.Monad.ST
#endif

import           Blip.Interpreter.HashTable.IntArray (Elem, IntArray)
import qualified Blip.Interpreter.HashTable.IntArray as M

#ifndef NO_C_SEARCH
import           Foreign.C.Types
#else
import           Data.Bits
import           Data.Int
import qualified Data.Vector.Unboxed              as U
import           GHC.Int
#endif

import           Blip.Interpreter.HashTable.Utils
import           GHC.Exts

#if __GLASGOW_HASKELL__ >= 707
import           GHC.Exts                         (isTrue#)
#else
isTrue# :: Bool -> Bool
isTrue# = id
#endif

{-# INLINE prefetchRead  #-}
{-# INLINE prefetchWrite #-}
prefetchRead :: IntArray -> Int -> IO ()
prefetchWrite :: IntArray -> Int -> IO ()

#ifndef NO_C_SEARCH
foreign import ccall unsafe "line_search"
  c_lineSearch :: Ptr a -> CInt -> CUShort -> IO CInt

foreign import ccall unsafe "line_search_2"
  c_lineSearch_2 :: Ptr a -> CInt -> CUShort -> CUShort -> IO CInt

foreign import ccall unsafe "line_search_2"
  c_lineSearch_3 :: Ptr a -> CInt -> CUShort -> CUShort -> CUShort -> IO CInt

foreign import ccall unsafe "forward_search_2"
  c_forwardSearch_2 :: Ptr a -> CInt -> CInt -> CUShort -> CUShort -> IO CInt

foreign import ccall unsafe "forward_search_3"
  c_forwardSearch_3 :: Ptr a -> CInt -> CInt -> CUShort -> CUShort -> CUShort
                    -> IO CInt

foreign import ccall unsafe "prefetch_cacheline_read"
  prefetchCacheLine_read :: Ptr a -> CInt -> IO ()

foreign import ccall unsafe "prefetch_cacheline_write"
  prefetchCacheLine_write :: Ptr a -> CInt -> IO ()


fI :: (Num b, Integral a) => a -> b
fI = fromIntegral

-- prefetchRead a i = unsafeIOToST $ prefetchCacheLine_read v x
prefetchRead a i = prefetchCacheLine_read v x
  where
    v   = M.toPtr a
    x   = fI i

-- prefetchWrite a i = unsafeIOToST $ prefetchCacheLine_write v x
prefetchWrite a i = prefetchCacheLine_write v x
  where
    v   = M.toPtr a
    x   = fI i


{-# INLINE forwardSearch2 #-}
forwardSearch2 :: IntArray -> Int -> Int -> Elem -> Elem -> IO Int
forwardSearch2 !vec !start !end !x1 !x2 =
    -- liftM fromEnum $! unsafeIOToST c
    liftM fromEnum $! c
  where
    c = c_forwardSearch_2 (M.toPtr vec) (fI start) (fI end) (fI x1) (fI x2)


{-# INLINE forwardSearch3 #-}
forwardSearch3 :: IntArray -> Int -> Int -> Elem -> Elem -> Elem -> IO Int
forwardSearch3 !vec !start !end !x1 !x2 !x3 =
    -- liftM fromEnum $! unsafeIOToST c
    liftM fromEnum $! c
  where
    c = c_forwardSearch_3 (M.toPtr vec) (fI start) (fI end)
                          (fI x1) (fI x2) (fI x3)


{-# INLINE lineSearch #-}
lineSearch :: IntArray -> Int -> Elem -> IO Int
lineSearch !vec !start !value =
    -- liftM fromEnum $! unsafeIOToST c
    liftM fromEnum $! c
  where
    c = c_lineSearch (M.toPtr vec) (fI start) (fI value)


{-# INLINE lineSearch2 #-}
lineSearch2 :: IntArray -> Int -> Elem -> Elem -> IO Int
lineSearch2 !vec !start !x1 !x2 =
    -- liftM fromEnum $! unsafeIOToST c
    liftM fromEnum $! c
  where
    c = c_lineSearch_2 (M.toPtr vec) (fI start) (fI x1) (fI x2)


{-# INLINE lineSearch3 #-}
lineSearch3 :: IntArray -> Int -> Elem -> Elem -> Elem -> IO Int
lineSearch3 !vec !start !x1 !x2 !x3 =
    -- liftM fromEnum $! unsafeIOToST c
    liftM fromEnum $! c
  where
    c = c_lineSearch_3 (M.toPtr vec) (fI start) (fI x1) (fI x2) (fI x3)
#endif

{-# INLINE advanceByCacheLineSize #-}
advanceByCacheLineSize :: Int -> Int -> Int
advanceByCacheLineSize !(I# start0#) !(I# vecSize#) = out
  where
    !(I# clm#) = cacheLineIntMask
    !clmm#     = not# (int2Word# clm#)
    !start#    = word2Int# (clmm# `and#` int2Word# start0#)
    !(I# nw#)  = numElemsInCacheLine
    !start'#   = start# +# nw#
    !s#        = sign# (vecSize# -# start'# -# 1#)
    !m#        = not# (int2Word# s#)
    !r#        = int2Word# start'# `and#` m#
    !out       = I# (word2Int# r#)


{-# INLINE isCacheLineAligned #-}
isCacheLineAligned :: Int -> Bool
isCacheLineAligned (I# x#) = isTrue# (r# ==# 0#)
  where
    !(I# m#) = cacheLineIntMask
    !mw#     = int2Word# m#
    !w#      = int2Word# x#
    !r#      = word2Int# (mw# `and#` w#)


{-# INLINE sign# #-}
-- | Returns 0 if x is positive, -1 otherwise
sign# :: Int# -> Int#
sign# !x# = x# `uncheckedIShiftRA#` wordSizeMinus1#
  where
    !(I# wordSizeMinus1#) = wordSize-1


{-# INLINE bl_abs# #-}
-- | Abs of an integer, branchless
bl_abs# :: Int# -> Int#
bl_abs# !x# = word2Int# r#
  where
    !m# = sign# x#
    !r# = (int2Word# (m# +# x#)) `xor#` int2Word# m#


{-# INLINE mask# #-}
-- | Returns 0xfff..fff (aka -1) if a# == b#, 0 otherwise.
mask# :: Int# -> Int# -> Int#
mask# !a# !b# = dest#
  where
    !d#    = a# -# b#
    !r#    = bl_abs# d# -# 1#
    !dest# = sign# r#


{- note: this code should be:

mask# :: Int# -> Int# -> Int#
mask# !a# !b# = let !(I# z#) = fromEnum (a# ==# b#)
                    !q#      = negateInt# z#
                in q#

but GHC doesn't properly optimize this as straight-line code at the moment.

-}


{-# INLINE maskw# #-}
maskw# :: Int# -> Int# -> Word#
maskw# !a# !b# = int2Word# (mask# a# b#)


#ifdef NO_C_SEARCH
prefetchRead _ _ = return ()
prefetchWrite _ _ = return ()

{-# INLINE forwardSearch2 #-}
forwardSearch2 :: IntArray -> Int -> Int -> Elem -> Elem -> IO Int
forwardSearch2 !vec !start !end !x1 !x2 = go start end False
  where
    next !i !e !b = let !j = i+1
                    in if j == e
                         then (if b then (-1,e,True) else (0,start,True))
                         else (j,e,b)

    go !i !e !b = do
        h <- M.readArray vec i
        if h == x1 || h == x2
          then return i
          else do
              let (!i',!e',!b') = next i e b
              if (i' < 0) then return (-1) else go i' e' b'


{-# INLINE forwardSearch3 #-}
forwardSearch3 :: IntArray -> Int -> Int -> Elem -> Elem -> Elem -> IO Int
forwardSearch3 !vec !start !end !x1 !x2 !x3 = go start end False
  where
    next !i !e !b = let !j = i+1
                    in if j == e
                         then (if b then (-1,e,True) else (0,start,True))
                         else (j,e,b)

    go !i !e !b = do
        h <- M.readArray vec i
        if h == x1 || h == x2 || h == x3
          then return i
          else do
              let (!i',!e',!b') = next i e b
              if (i' < 0) then return (-1) else go i' e' b'


deBruijnBitPositions :: U.Vector Int8
deBruijnBitPositions =
    U.fromList [
          0,   1, 28,  2, 29, 14, 24,  3, 30, 22, 20, 15, 25, 17,  4,  8,
          31, 27, 13, 23, 21, 19, 16,  7, 26, 12, 18,  6, 11,  5, 10,  9
         ]


{-# INLINE firstBitSet# #-}
-- only works with 32-bit values -- ok for us here
firstBitSet# :: Int# -> Int#
firstBitSet# i# = word2Int# ((or# zeroCase# posw#))
  where
    !zeroCase#   = int2Word# (mask# 0# i#)
    !w#          = int2Word# i#
    !iLowest#    = word2Int# (and# w# (int2Word# (negateInt# i#)))
    !idxW#       = uncheckedShiftRL#
                       (narrow32Word# (timesWord# (int2Word# iLowest#)
                                                  (int2Word# 0x077CB531#)))
                       27#
    !idx         = I# (word2Int# idxW#)
    !(I8# pos8#) = U.unsafeIndex deBruijnBitPositions idx
    !posw#       = int2Word# pos8#

#endif


#ifdef NO_C_SEARCH
lineResult# :: Word#    -- ^ mask
            -> Int      -- ^ start value
            -> Int
lineResult# bitmask# (I# start#) = I# (word2Int# rv#)
  where
    !p#   = firstBitSet# (word2Int# bitmask#)
    !mm#  = maskw# p# (-1#)
    !nmm# = not# mm#
    !rv#  = mm# `or#` (nmm# `and#` (int2Word# (start# +# p#)))
{-# INLINE lineResult# #-}


-- Required: unlike in C search, required that the start index is
-- cache-line-aligned and array has at least 32 elements after the start index
lineSearch :: IntArray        -- ^ vector to search
           -> Int               -- ^ start index
           -> Elem              -- ^ value to search for
           -> IO Int          -- ^ dest index where it can be found, or
                                -- \"-1\" if not found
lineSearch !vec !start !elem1 = do
    let !(I# v1#) = fromIntegral elem1

    !(I# x1#) <- liftM fromIntegral $ M.readArray vec start
    let !p1# = (and# (maskw# x1# v1#) (int2Word# 0x1#))

    !(I# x2#) <- liftM fromIntegral $ M.readArray vec $! start + 1
    let !p2# = or# p1# (and# (maskw# x2# v1#) (int2Word# 0x2#))

    !(I# x3#) <- liftM fromIntegral $ M.readArray vec $! start + 2
    let !p3# = or# p2# (and# (maskw# x3# v1#) (int2Word# 0x4#))

    !(I# x4#) <- liftM fromIntegral $ M.readArray vec $! start + 3
    let !p4# = or# p3# (and# (maskw# x4# v1#) (int2Word# 0x8#))

    !(I# x5#) <- liftM fromIntegral $ M.readArray vec $! start + 4
    let !p5# = or# p4# (and# (maskw# x5# v1#) (int2Word# 0x10#))

    !(I# x6#) <- liftM fromIntegral $ M.readArray vec $! start + 5
    let !p6# = or# p5# (and# (maskw# x6# v1#) (int2Word# 0x20#))

    !(I# x7#) <- liftM fromIntegral $ M.readArray vec $! start + 6
    let !p7# = or# p6# (and# (maskw# x7# v1#) (int2Word# 0x40#))

    !(I# x8#) <- liftM fromIntegral $ M.readArray vec $! start + 7
    let !p8# = or# p7# (and# (maskw# x8# v1#) (int2Word# 0x80#))

    !(I# x9#) <- liftM fromIntegral $ M.readArray vec $! start + 8
    let !p9# = or# p8# (and# (maskw# x9# v1#) (int2Word# 0x100#))

    !(I# x10#) <- liftM fromIntegral $ M.readArray vec $! start + 9
    let !p10# = or# p9# (and# (maskw# x10# v1#) (int2Word# 0x200#))

    !(I# x11#) <- liftM fromIntegral $ M.readArray vec $! start + 10
    let !p11# = or# p10# (and# (maskw# x11# v1#) (int2Word# 0x400#))

    !(I# x12#) <- liftM fromIntegral $ M.readArray vec $! start + 11
    let !p12# = or# p11# (and# (maskw# x12# v1#) (int2Word# 0x800#))

    !(I# x13#) <- liftM fromIntegral $ M.readArray vec $! start + 12
    let !p13# = or# p12# (and# (maskw# x13# v1#) (int2Word# 0x1000#))

    !(I# x14#) <- liftM fromIntegral $ M.readArray vec $! start + 13
    let !p14# = or# p13# (and# (maskw# x14# v1#) (int2Word# 0x2000#))

    !(I# x15#) <- liftM fromIntegral $ M.readArray vec $! start + 14
    let !p15# = or# p14# (and# (maskw# x15# v1#) (int2Word# 0x4000#))

    !(I# x16#) <- liftM fromIntegral $ M.readArray vec $! start + 15
    let !p16# = or# p15# (and# (maskw# x16# v1#) (int2Word# 0x8000#))

    !(I# x17#) <- liftM fromIntegral $ M.readArray vec $! start + 16
    let !p17# = or# p16# (and# (maskw# x17# v1#) (int2Word# 0x10000#))

    !(I# x18#) <- liftM fromIntegral $ M.readArray vec $! start + 17
    let !p18# = or# p17# (and# (maskw# x18# v1#) (int2Word# 0x20000#))

    !(I# x19#) <- liftM fromIntegral $ M.readArray vec $! start + 18
    let !p19# = or# p18# (and# (maskw# x19# v1#) (int2Word# 0x40000#))

    !(I# x20#) <- liftM fromIntegral $ M.readArray vec $! start + 19
    let !p20# = or# p19# (and# (maskw# x20# v1#) (int2Word# 0x80000#))

    !(I# x21#) <- liftM fromIntegral $ M.readArray vec $! start + 20
    let !p21# = or# p20# (and# (maskw# x21# v1#) (int2Word# 0x100000#))

    !(I# x22#) <- liftM fromIntegral $ M.readArray vec $! start + 21
    let !p22# = or# p21# (and# (maskw# x22# v1#) (int2Word# 0x200000#))

    !(I# x23#) <- liftM fromIntegral $ M.readArray vec $! start + 22
    let !p23# = or# p22# (and# (maskw# x23# v1#) (int2Word# 0x400000#))

    !(I# x24#) <- liftM fromIntegral $ M.readArray vec $! start + 23
    let !p24# = or# p23# (and# (maskw# x24# v1#) (int2Word# 0x800000#))

    !(I# x25#) <- liftM fromIntegral $ M.readArray vec $! start + 24
    let !p25# = or# p24# (and# (maskw# x25# v1#) (int2Word# 0x1000000#))

    !(I# x26#) <- liftM fromIntegral $ M.readArray vec $! start + 25
    let !p26# = or# p25# (and# (maskw# x26# v1#) (int2Word# 0x2000000#))

    !(I# x27#) <- liftM fromIntegral $ M.readArray vec $! start + 26
    let !p27# = or# p26# (and# (maskw# x27# v1#) (int2Word# 0x4000000#))

    !(I# x28#) <- liftM fromIntegral $ M.readArray vec $! start + 27
    let !p28# = or# p27# (and# (maskw# x28# v1#) (int2Word# 0x8000000#))

    !(I# x29#) <- liftM fromIntegral $ M.readArray vec $! start + 28
    let !p29# = or# p28# (and# (maskw# x29# v1#) (int2Word# 0x10000000#))

    !(I# x30#) <- liftM fromIntegral $ M.readArray vec $! start + 29
    let !p30# = or# p29# (and# (maskw# x30# v1#) (int2Word# 0x20000000#))

    !(I# x31#) <- liftM fromIntegral $ M.readArray vec $! start + 30
    let !p31# = or# p30# (and# (maskw# x31# v1#) (int2Word# 0x40000000#))

    !(I# x32#) <- liftM fromIntegral $ M.readArray vec $! start + 31
    let !p32# = or# p31# (and# (maskw# x32# v1#) (int2Word# 0x80000000#))

    return $! lineResult# p32# start


-- Required: unlike in C search, required that the start index is
-- cache-line-aligned and array has at least 32 elements after the start index
lineSearch2 :: IntArray        -- ^ vector to search
            -> Int               -- ^ start index
            -> Elem              -- ^ value to search for
            -> Elem              -- ^ second value to search for
            -> IO Int          -- ^ dest index where it can be found, or
                                -- \"-1\" if not found
lineSearch2 !vec !start !elem1 !elem2 = do
    let !(I# v1#) = fromIntegral elem1
    let !(I# v2#) = fromIntegral elem2

    !(I# x1#) <- liftM fromIntegral $ M.readArray vec start
    let !p1# = (and# (int2Word# 0x1#)
                     (or# (maskw# x1# v1#) (maskw# x1# v2#)))
    !(I# x2#) <- liftM fromIntegral $ M.readArray vec $! start + 1
    let !p2# = or# p1# (and# (int2Word# 0x2#)
                             (or# (maskw# x2# v1#) (maskw# x2# v2#)))
    !(I# x3#) <- liftM fromIntegral $ M.readArray vec $! start + 2
    let !p3# = or# p2# (and# (int2Word# 0x4#)
                             (or# (maskw# x3# v1#) (maskw# x3# v2#)))
    !(I# x4#) <- liftM fromIntegral $ M.readArray vec $! start + 3
    let !p4# = or# p3# (and# (int2Word# 0x8#)
                             (or# (maskw# x4# v1#) (maskw# x4# v2#)))
    !(I# x5#) <- liftM fromIntegral $ M.readArray vec $! start + 4
    let !p5# = or# p4# (and# (int2Word# 0x10#)
                             (or# (maskw# x5# v1#) (maskw# x5# v2#)))
    !(I# x6#) <- liftM fromIntegral $ M.readArray vec $! start + 5
    let !p6# = or# p5# (and# (int2Word# 0x20#)
                             (or# (maskw# x6# v1#) (maskw# x6# v2#)))
    !(I# x7#) <- liftM fromIntegral $ M.readArray vec $! start + 6
    let !p7# = or# p6# (and# (int2Word# 0x40#)
                             (or# (maskw# x7# v1#) (maskw# x7# v2#)))
    !(I# x8#) <- liftM fromIntegral $ M.readArray vec $! start + 7
    let !p8# = or# p7# (and# (int2Word# 0x80#)
                             (or# (maskw# x8# v1#) (maskw# x8# v2#)))
    !(I# x9#) <- liftM fromIntegral $ M.readArray vec $! start + 8
    let !p9# = or# p8# (and# (int2Word# 0x100#)
                             (or# (maskw# x9# v1#) (maskw# x9# v2#)))
    !(I# x10#) <- liftM fromIntegral $ M.readArray vec $! start + 9
    let !p10# = or# p9# (and# (int2Word# 0x200#)
                             (or# (maskw# x10# v1#) (maskw# x10# v2#)))
    !(I# x11#) <- liftM fromIntegral $ M.readArray vec $! start + 10
    let !p11# = or# p10# (and# (int2Word# 0x400#)
                             (or# (maskw# x11# v1#) (maskw# x11# v2#)))
    !(I# x12#) <- liftM fromIntegral $ M.readArray vec $! start + 11
    let !p12# = or# p11# (and# (int2Word# 0x800#)
                             (or# (maskw# x12# v1#) (maskw# x12# v2#)))
    !(I# x13#) <- liftM fromIntegral $ M.readArray vec $! start + 12
    let !p13# = or# p12# (and# (int2Word# 0x1000#)
                             (or# (maskw# x13# v1#) (maskw# x13# v2#)))
    !(I# x14#) <- liftM fromIntegral $ M.readArray vec $! start + 13
    let !p14# = or# p13# (and# (int2Word# 0x2000#)
                             (or# (maskw# x14# v1#) (maskw# x14# v2#)))
    !(I# x15#) <- liftM fromIntegral $ M.readArray vec $! start + 14
    let !p15# = or# p14# (and# (int2Word# 0x4000#)
                             (or# (maskw# x15# v1#) (maskw# x15# v2#)))
    !(I# x16#) <- liftM fromIntegral $ M.readArray vec $! start + 15
    let !p16# = or# p15# (and# (int2Word# 0x8000#)
                             (or# (maskw# x16# v1#) (maskw# x16# v2#)))
    !(I# x17#) <- liftM fromIntegral $ M.readArray vec $! start + 16
    let !p17# = or# p16# (and# (int2Word# 0x10000#)
                             (or# (maskw# x17# v1#) (maskw# x17# v2#)))
    !(I# x18#) <- liftM fromIntegral $ M.readArray vec $! start + 17
    let !p18# = or# p17# (and# (int2Word# 0x20000#)
                             (or# (maskw# x18# v1#) (maskw# x18# v2#)))
    !(I# x19#) <- liftM fromIntegral $ M.readArray vec $! start + 18
    let !p19# = or# p18# (and# (int2Word# 0x40000#)
                             (or# (maskw# x19# v1#) (maskw# x19# v2#)))
    !(I# x20#) <- liftM fromIntegral $ M.readArray vec $! start + 19
    let !p20# = or# p19# (and# (int2Word# 0x80000#)
                             (or# (maskw# x20# v1#) (maskw# x20# v2#)))
    !(I# x21#) <- liftM fromIntegral $ M.readArray vec $! start + 20
    let !p21# = or# p20# (and# (int2Word# 0x100000#)
                             (or# (maskw# x21# v1#) (maskw# x21# v2#)))
    !(I# x22#) <- liftM fromIntegral $ M.readArray vec $! start + 21
    let !p22# = or# p21# (and# (int2Word# 0x200000#)
                             (or# (maskw# x22# v1#) (maskw# x22# v2#)))
    !(I# x23#) <- liftM fromIntegral $ M.readArray vec $! start + 22
    let !p23# = or# p22# (and# (int2Word# 0x400000#)
                             (or# (maskw# x23# v1#) (maskw# x23# v2#)))
    !(I# x24#) <- liftM fromIntegral $ M.readArray vec $! start + 23
    let !p24# = or# p23# (and# (int2Word# 0x800000#)
                             (or# (maskw# x24# v1#) (maskw# x24# v2#)))
    !(I# x25#) <- liftM fromIntegral $ M.readArray vec $! start + 24
    let !p25# = or# p24# (and# (int2Word# 0x1000000#)
                             (or# (maskw# x25# v1#) (maskw# x25# v2#)))
    !(I# x26#) <- liftM fromIntegral $ M.readArray vec $! start + 25
    let !p26# = or# p25# (and# (int2Word# 0x2000000#)
                             (or# (maskw# x26# v1#) (maskw# x26# v2#)))
    !(I# x27#) <- liftM fromIntegral $ M.readArray vec $! start + 26
    let !p27# = or# p26# (and# (int2Word# 0x4000000#)
                             (or# (maskw# x27# v1#) (maskw# x27# v2#)))
    !(I# x28#) <- liftM fromIntegral $ M.readArray vec $! start + 27
    let !p28# = or# p27# (and# (int2Word# 0x8000000#)
                             (or# (maskw# x28# v1#) (maskw# x28# v2#)))
    !(I# x29#) <- liftM fromIntegral $ M.readArray vec $! start + 28
    let !p29# = or# p28# (and# (int2Word# 0x10000000#)
                             (or# (maskw# x29# v1#) (maskw# x29# v2#)))
    !(I# x30#) <- liftM fromIntegral $ M.readArray vec $! start + 29
    let !p30# = or# p29# (and# (int2Word# 0x20000000#)
                             (or# (maskw# x30# v1#) (maskw# x30# v2#)))
    !(I# x31#) <- liftM fromIntegral $ M.readArray vec $! start + 30
    let !p31# = or# p30# (and# (int2Word# 0x40000000#)
                             (or# (maskw# x31# v1#) (maskw# x31# v2#)))
    !(I# x32#) <- liftM fromIntegral $ M.readArray vec $! start + 31
    let !p32# = or# p31# (and# (int2Word# 0x80000000#)
                             (or# (maskw# x32# v1#) (maskw# x32# v2#)))

    return $! lineResult# p32# start


-- Required: unlike in C search, required that the start index is
-- cache-line-aligned and array has at least 32 elements after the start index
lineSearch3 :: IntArray        -- ^ vector to search
            -> Int               -- ^ start index
            -> Elem              -- ^ value to search for
            -> Elem              -- ^ second value to search for
            -> Elem              -- ^ third value to search for
            -> IO Int          -- ^ dest index where it can be found, or
                                -- \"-1\" if not found
lineSearch3 !vec !start !elem1 !elem2 !elem3 = do
    let !(I# v1#) = fromIntegral elem1
    let !(I# v2#) = fromIntegral elem2
    let !(I# v3#) = fromIntegral elem3

    !(I# x1#) <- liftM fromIntegral $ M.readArray vec start
    let !p1# = (and# (int2Word# 0x1#)
                     (or# (maskw# x1# v1#)
                          (or# (maskw# x1# v2#) (maskw# x1# v3#))))

    !(I# x2#) <- liftM fromIntegral $ M.readArray vec $! start + 1
    let !p2# = or# p1# (and# (int2Word# 0x2#)
                             (or# (maskw# x2# v1#)
                                  (or# (maskw# x2# v2#) (maskw# x2# v3#))))

    !(I# x3#) <- liftM fromIntegral $ M.readArray vec $! start + 2
    let !p3# = or# p2# (and# (int2Word# 0x4#)
                             (or# (maskw# x3# v1#)
                                  (or# (maskw# x3# v2#) (maskw# x3# v3#))))

    !(I# x4#) <- liftM fromIntegral $ M.readArray vec $! start + 3
    let !p4# = or# p3# (and# (int2Word# 0x8#)
                             (or# (maskw# x4# v1#)
                                  (or# (maskw# x4# v2#) (maskw# x4# v3#))))

    !(I# x5#) <- liftM fromIntegral $ M.readArray vec $! start + 4
    let !p5# = or# p4# (and# (int2Word# 0x10#)
                             (or# (maskw# x5# v1#)
                                  (or# (maskw# x5# v2#) (maskw# x5# v3#))))

    !(I# x6#) <- liftM fromIntegral $ M.readArray vec $! start + 5
    let !p6# = or# p5# (and# (int2Word# 0x20#)
                             (or# (maskw# x6# v1#)
                                  (or# (maskw# x6# v2#) (maskw# x6# v3#))))

    !(I# x7#) <- liftM fromIntegral $ M.readArray vec $! start + 6
    let !p7# = or# p6# (and# (int2Word# 0x40#)
                             (or# (maskw# x7# v1#)
                                  (or# (maskw# x7# v2#) (maskw# x7# v3#))))

    !(I# x8#) <- liftM fromIntegral $ M.readArray vec $! start + 7
    let !p8# = or# p7# (and# (int2Word# 0x80#)
                             (or# (maskw# x8# v1#)
                                  (or# (maskw# x8# v2#) (maskw# x8# v3#))))

    !(I# x9#) <- liftM fromIntegral $ M.readArray vec $! start + 8
    let !p9# = or# p8# (and# (int2Word# 0x100#)
                             (or# (maskw# x9# v1#)
                                  (or# (maskw# x9# v2#) (maskw# x9# v3#))))

    !(I# x10#) <- liftM fromIntegral $ M.readArray vec $! start + 9
    let !p10# = or# p9# (and# (int2Word# 0x200#)
                             (or# (maskw# x10# v1#)
                                  (or# (maskw# x10# v2#) (maskw# x10# v3#))))

    !(I# x11#) <- liftM fromIntegral $ M.readArray vec $! start + 10
    let !p11# = or# p10# (and# (int2Word# 0x400#)
                             (or# (maskw# x11# v1#)
                                  (or# (maskw# x11# v2#) (maskw# x11# v3#))))

    !(I# x12#) <- liftM fromIntegral $ M.readArray vec $! start + 11
    let !p12# = or# p11# (and# (int2Word# 0x800#)
                             (or# (maskw# x12# v1#)
                                  (or# (maskw# x12# v2#) (maskw# x12# v3#))))

    !(I# x13#) <- liftM fromIntegral $ M.readArray vec $! start + 12
    let !p13# = or# p12# (and# (int2Word# 0x1000#)
                             (or# (maskw# x13# v1#)
                                  (or# (maskw# x13# v2#) (maskw# x13# v3#))))

    !(I# x14#) <- liftM fromIntegral $ M.readArray vec $! start + 13
    let !p14# = or# p13# (and# (int2Word# 0x2000#)
                             (or# (maskw# x14# v1#)
                                  (or# (maskw# x14# v2#) (maskw# x14# v3#))))

    !(I# x15#) <- liftM fromIntegral $ M.readArray vec $! start + 14
    let !p15# = or# p14# (and# (int2Word# 0x4000#)
                             (or# (maskw# x15# v1#)
                                  (or# (maskw# x15# v2#) (maskw# x15# v3#))))

    !(I# x16#) <- liftM fromIntegral $ M.readArray vec $! start + 15
    let !p16# = or# p15# (and# (int2Word# 0x8000#)
                             (or# (maskw# x16# v1#)
                                  (or# (maskw# x16# v2#) (maskw# x16# v3#))))

    !(I# x17#) <- liftM fromIntegral $ M.readArray vec $! start + 16
    let !p17# = or# p16# (and# (int2Word# 0x10000#)
                             (or# (maskw# x17# v1#)
                                  (or# (maskw# x17# v2#) (maskw# x17# v3#))))

    !(I# x18#) <- liftM fromIntegral $ M.readArray vec $! start + 17
    let !p18# = or# p17# (and# (int2Word# 0x20000#)
                             (or# (maskw# x18# v1#)
                                  (or# (maskw# x18# v2#) (maskw# x18# v3#))))

    !(I# x19#) <- liftM fromIntegral $ M.readArray vec $! start + 18
    let !p19# = or# p18# (and# (int2Word# 0x40000#)
                             (or# (maskw# x19# v1#)
                                  (or# (maskw# x19# v2#) (maskw# x19# v3#))))

    !(I# x20#) <- liftM fromIntegral $ M.readArray vec $! start + 19
    let !p20# = or# p19# (and# (int2Word# 0x80000#)
                             (or# (maskw# x20# v1#)
                                  (or# (maskw# x20# v2#) (maskw# x20# v3#))))

    !(I# x21#) <- liftM fromIntegral $ M.readArray vec $! start + 20
    let !p21# = or# p20# (and# (int2Word# 0x100000#)
                             (or# (maskw# x21# v1#)
                                  (or# (maskw# x21# v2#) (maskw# x21# v3#))))

    !(I# x22#) <- liftM fromIntegral $ M.readArray vec $! start + 21
    let !p22# = or# p21# (and# (int2Word# 0x200000#)
                             (or# (maskw# x22# v1#)
                                  (or# (maskw# x22# v2#) (maskw# x22# v3#))))

    !(I# x23#) <- liftM fromIntegral $ M.readArray vec $! start + 22
    let !p23# = or# p22# (and# (int2Word# 0x400000#)
                             (or# (maskw# x23# v1#)
                                  (or# (maskw# x23# v2#) (maskw# x23# v3#))))

    !(I# x24#) <- liftM fromIntegral $ M.readArray vec $! start + 23
    let !p24# = or# p23# (and# (int2Word# 0x800000#)
                             (or# (maskw# x24# v1#)
                                  (or# (maskw# x24# v2#) (maskw# x24# v3#))))

    !(I# x25#) <- liftM fromIntegral $ M.readArray vec $! start + 24
    let !p25# = or# p24# (and# (int2Word# 0x1000000#)
                             (or# (maskw# x25# v1#)
                                  (or# (maskw# x25# v2#) (maskw# x25# v3#))))

    !(I# x26#) <- liftM fromIntegral $ M.readArray vec $! start + 25
    let !p26# = or# p25# (and# (int2Word# 0x2000000#)
                             (or# (maskw# x26# v1#)
                                  (or# (maskw# x26# v2#) (maskw# x26# v3#))))

    !(I# x27#) <- liftM fromIntegral $ M.readArray vec $! start + 26
    let !p27# = or# p26# (and# (int2Word# 0x4000000#)
                             (or# (maskw# x27# v1#)
                                  (or# (maskw# x27# v2#) (maskw# x27# v3#))))

    !(I# x28#) <- liftM fromIntegral $ M.readArray vec $! start + 27
    let !p28# = or# p27# (and# (int2Word# 0x8000000#)
                             (or# (maskw# x28# v1#)
                                  (or# (maskw# x28# v2#) (maskw# x28# v3#))))

    !(I# x29#) <- liftM fromIntegral $ M.readArray vec $! start + 28
    let !p29# = or# p28# (and# (int2Word# 0x10000000#)
                             (or# (maskw# x29# v1#)
                                  (or# (maskw# x29# v2#) (maskw# x29# v3#))))

    !(I# x30#) <- liftM fromIntegral $ M.readArray vec $! start + 29
    let !p30# = or# p29# (and# (int2Word# 0x20000000#)
                             (or# (maskw# x30# v1#)
                                  (or# (maskw# x30# v2#) (maskw# x30# v3#))))

    !(I# x31#) <- liftM fromIntegral $ M.readArray vec $! start + 30
    let !p31# = or# p30# (and# (int2Word# 0x40000000#)
                             (or# (maskw# x31# v1#)
                                  (or# (maskw# x31# v2#) (maskw# x31# v3#))))

    !(I# x32#) <- liftM fromIntegral $ M.readArray vec $! start + 31
    let !p32# = or# p31# (and# (int2Word# 0x80000000#)
                             (or# (maskw# x32# v1#)
                                  (or# (maskw# x32# v2#) (maskw# x32# v3#))))

    return $! lineResult# p32# start


------------------------------------------------------------------------------
-- | Search through a mutable vector for a given int value. The number of
-- things to search for must be at most the number of things remaining in the
-- vector.
naiveSearch :: IntArray       -- ^ vector to search
            -> Int              -- ^ start index
            -> Int              -- ^ number of things to search
            -> Elem             -- ^ value to search for
            -> IO Int         -- ^ dest index where it can be found, or
                                -- \"-1\" if not found
naiveSearch !vec !start !nThings !value = go start
  where
    !doneIdx = start + nThings

    go !i | i >= doneIdx = return (-1)
          | otherwise = do
        x <- M.readArray vec i
        if x == value then return i else go (i+1)
{-# INLINE naiveSearch #-}


------------------------------------------------------------------------------
naiveSearch2 :: IntArray       -- ^ vector to search
             -> Int              -- ^ start index
             -> Int              -- ^ number of things to search
             -> Elem             -- ^ value to search for
             -> Elem             -- ^ value 2 to search for
             -> IO Int         -- ^ dest index where it can be found, or
                                -- \"-1\" if not found
naiveSearch2 !vec !start !nThings !value1 !value2 = go start
  where
    !doneIdx = start + nThings

    go !i | i >= doneIdx = return (-1)
          | otherwise = do
        x <- M.readArray vec i
        if x == value1 || x == value2 then return i else go (i+1)
{-# INLINE naiveSearch2 #-}


------------------------------------------------------------------------------
naiveSearch3 :: IntArray       -- ^ vector to search
             -> Int              -- ^ start index
             -> Int              -- ^ number of things to search
             -> Elem             -- ^ value to search for
             -> Elem             -- ^ value 2 to search for
             -> Elem             -- ^ value 3 to search for
             -> IO Int         -- ^ dest index where it can be found, or
                                -- \"-1\" if not found
naiveSearch3 !vec !start !nThings !value1 !value2 !value3 = go start
  where
    !doneIdx = start + nThings

    go !i | i >= doneIdx = return (-1)
          | otherwise = do
        x <- M.readArray vec i
        if x == value1 || x == value2 || x == value3
          then return i
          else go (i+1)
{-# INLINE naiveSearch3 #-}

-- end #if NO_C_SEARCH
#endif

------------------------------------------------------------------------------
-- | Search through a mutable vector for a given int value, cache-line aligned.
-- If the start index is cache-line aligned, and there is more than a
-- cache-line's room between the start index and the end of the vector, we will
-- search the cache-line all at once using an efficient branchless
-- bit-twiddling technique. Otherwise, we will use a typical loop.
--
cacheLineSearch :: IntArray        -- ^ vector to search
                -> Int               -- ^ start index
                -> Elem              -- ^ value to search for
                -> IO Int          -- ^ dest index where it can be found, or
                                     -- \"-1\" if not found
cacheLineSearch !vec !start !value = do
#ifdef NO_C_SEARCH
    let !vlen  = M.length vec
    let !st1   = vlen - start
    let !nvlen = numElemsInCacheLine - st1
    let adv    = (start + cacheLineIntMask) .&. complement cacheLineIntMask
    let st2    = adv - start


    if nvlen > 0 || not (isCacheLineAligned start)
      then naiveSearch vec start (min st1 st2) value
      else lineSearch vec start value
#else
    lineSearch vec start value
#endif
{-# INLINE cacheLineSearch #-}


------------------------------------------------------------------------------
-- | Search through a mutable vector for one of two given int values,
-- cache-line aligned.  If the start index is cache-line aligned, and there is
-- more than a cache-line's room between the start index and the end of the
-- vector, we will search the cache-line all at once using an efficient
-- branchless bit-twiddling technique. Otherwise, we will use a typical loop.
--
cacheLineSearch2 :: IntArray        -- ^ vector to search
                 -> Int               -- ^ start index
                 -> Elem              -- ^ value to search for
                 -> Elem              -- ^ value 2 to search for
                 -> IO Int          -- ^ dest index where it can be found, or
                                     -- \"-1\" if not found
cacheLineSearch2 !vec !start !value !value2 = do
#ifdef NO_C_SEARCH
    let !vlen  = M.length vec
    let !st1   = vlen - start
    let !nvlen = numElemsInCacheLine - st1
    let adv    = (start + cacheLineIntMask) .&. complement cacheLineIntMask
    let st2    = adv - start

    if nvlen > 0 || not (isCacheLineAligned start)
      then naiveSearch2 vec start (min st1 st2) value value2
      else lineSearch2 vec start value value2
#else
    lineSearch2 vec start value value2
#endif
{-# INLINE cacheLineSearch2 #-}


------------------------------------------------------------------------------
-- | Search through a mutable vector for one of three given int values,
-- cache-line aligned.  If the start index is cache-line aligned, and there is
-- more than a cache-line's room between the start index and the end of the
-- vector, we will search the cache-line all at once using an efficient
-- branchless bit-twiddling technique. Otherwise, we will use a typical loop.
--
cacheLineSearch3 :: IntArray        -- ^ vector to search
                 -> Int               -- ^ start index
                 -> Elem              -- ^ value to search for
                 -> Elem              -- ^ value 2 to search for
                 -> Elem              -- ^ value 3 to search for
                 -> IO Int          -- ^ dest index where it can be found, or
                                     -- \"-1\" if not found
cacheLineSearch3 !vec !start !value !value2 !value3 = do
#ifdef NO_C_SEARCH
    let !vlen  = M.length vec
    let !st1   = vlen - start
    let !nvlen = numElemsInCacheLine - st1
    let adv    = (start + cacheLineIntMask) .&. complement cacheLineIntMask
    let st2    = adv - start

    if nvlen > 0 || not (isCacheLineAligned start)
      then naiveSearch3 vec start (min st1 st2) value value2 value3
      else lineSearch3 vec start value value2 value3
#else
    lineSearch3 vec start value value2 value3
#endif
{-# INLINE cacheLineSearch3 #-}
