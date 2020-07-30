-----------------------------------------------------------------------------
-- |
-- Module      : Blip.MarshalDouble
-- Copyright   : (c) 2012, 2013 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Convert Haskell Doubles to and from a list of bytes.
-- 
-- XXX not sure if this is complete or immune from endian issues.
--
-----------------------------------------------------------------------------

module Blip.MarshalDouble (doubleToBytes, bytesToDouble) where

import Foreign.Storable (peek, peekByteOff, pokeByteOff)
import Foreign.Marshal.Utils (with)
import Foreign.Marshal.Alloc (alloca)
import Data.Word (Word8)
import Control.Monad (forM, forM_)
import System.IO.Unsafe (unsafePerformIO)

doubleToBytes :: Double -> [Word8]
doubleToBytes = unsafePerformIO . doubleToBytesIO

doubleToBytesIO :: Double -> IO [Word8]
doubleToBytesIO d =
   with d $ \ptr ->
       forM [0..7] $ \index -> 
          peekByteOff ptr index

bytesToDouble :: [Word8] -> Double
bytesToDouble = unsafePerformIO . bytesToDoubleIO

bytesToDoubleIO :: [Word8] -> IO Double
bytesToDoubleIO bs =
   alloca $ \ptr -> do
      forM_ (zip [0..7] bs) $ \(index, byte) -> 
         pokeByteOff ptr index byte
      peek ptr
