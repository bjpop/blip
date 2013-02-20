module Blip.MarshalDouble (doubleToBytes, bytesToDouble) where

import Foreign.Storable (peek, peekByteOff, pokeByteOff)
import Foreign.Marshal.Utils (with)
import Foreign.Marshal.Alloc (alloca)
import Data.Word (Word8)
import Control.Monad (forM)
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
      forM (zip [0..7] bs) $ \(index, byte) -> 
         pokeByteOff ptr index byte
      peek ptr
