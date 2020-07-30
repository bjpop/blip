-----------------------------------------------------------------------------
-- |
-- Module      : Blip.Bytecode
-- Copyright   : (c) 2012, 2013 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Python 3 bytecode.
--
-----------------------------------------------------------------------------
module Blip.Bytecode
   ( decode, encode, Opcode (..), Bytecode (..),
     BytecodeArg (..), BytecodeSeq (..), bytecodeSize
   , word8ToOpcode, hasArg, word8sToWord16 ) where

import Data.Word (Word8, Word16)
import Data.ByteString.Lazy as B 
   ( ByteString, unpack, pack )
import Data.Map as Map hiding (map)
import Data.Bits ((.&.), shiftL, shiftR)
import Text.PrettyPrint
   ( text, (<+>), Doc, int, vcat )
import Blip.Pretty (Pretty (..))

data Opcode
   = POP_TOP               -- 1
   | ROT_TWO               -- 2
   | ROT_THREE             -- 3
   | DUP_TOP               -- 4
   | DUP_TOP_TWO           -- 5
   | NOP                   -- 9
   | UNARY_POSITIVE        -- 10
   | UNARY_NEGATIVE        -- 11
   | UNARY_NOT             -- 12
   | UNARY_INVERT          -- 15
   | BINARY_POWER          -- 19
   | BINARY_MULTIPLY       -- 20
   | BINARY_MODULO         -- 22
   | BINARY_ADD            -- 23
   | BINARY_SUBTRACT       -- 24
   | BINARY_SUBSCR         -- 25
   | BINARY_FLOOR_DIVIDE   -- 26
   | BINARY_TRUE_DIVIDE    -- 27
   | INPLACE_FLOOR_DIVIDE  -- 28
   | INPLACE_TRUE_DIVIDE   -- 29
   | STORE_MAP             -- 54
   | INPLACE_ADD           -- 55
   | INPLACE_SUBTRACT      -- 56
   | INPLACE_MULTIPLY      -- 57
   | INPLACE_MODULO        -- 59
   | STORE_SUBSCR          -- 60
   | DELETE_SUBSCR         -- 61
   | BINARY_LSHIFT         -- 62
   | BINARY_RSHIFT         -- 63
   | BINARY_AND            -- 64
   | BINARY_XOR            -- 65
   | BINARY_OR             -- 66
   | INPLACE_POWER         -- 67
   | GET_ITER              -- 68
   | STORE_LOCALS          -- 69
   | PRINT_EXPR            -- 70
   | LOAD_BUILD_CLASS      -- 71
   | YIELD_FROM            -- 72
   | INPLACE_LSHIFT        -- 75
   | INPLACE_RSHIFT        -- 76
   | INPLACE_AND           -- 77
   | INPLACE_XOR           -- 78
   | INPLACE_OR            -- 79
   | BREAK_LOOP            -- 80
   | WITH_CLEANUP          -- 81
   | RETURN_VALUE          -- 83
   | IMPORT_STAR           -- 84
   | YIELD_VALUE           -- 86
   | POP_BLOCK             -- 87
   | END_FINALLY           -- 88
   | POP_EXCEPT            -- 89
   -- | HAVE_ARGUMENT         -- 90    Opcodes from here have an argument: 
   | STORE_NAME            -- 90    Index in name list 
   | DELETE_NAME           -- 91    "" 
   | UNPACK_SEQUENCE       -- 92    Number of sequence items 
   | FOR_ITER              -- 93
   | UNPACK_EX             -- 94    Num items before variable part + (Num items after variable part << 8) 
   | STORE_ATTR            -- 95    Index in name list 
   | DELETE_ATTR           -- 96    "" 
   | STORE_GLOBAL          -- 97    "" 
   | DELETE_GLOBAL         -- 98    "" 
   | LOAD_CONST            -- 100   Index in const list 
   | LOAD_NAME             -- 101   Index in name list 
   | BUILD_TUPLE           -- 102   Number of tuple items 
   | BUILD_LIST            -- 103   Number of list items 
   | BUILD_SET             -- 104   Number of set items 
   | BUILD_MAP             -- 105   Always zero for now 
   | LOAD_ATTR             -- 106   Index in name list 
   | COMPARE_OP            -- 107   Comparison operator 
   | IMPORT_NAME           -- 108   Index in name list 
   | IMPORT_FROM           -- 109   Index in name list 
   | JUMP_FORWARD          -- 110   Number of bytes to skip 
   | JUMP_IF_FALSE_OR_POP  -- 111   Target byte offset from beginning of code 
   | JUMP_IF_TRUE_OR_POP   -- 112   "" 
   | JUMP_ABSOLUTE         -- 113   "" 
   | POP_JUMP_IF_FALSE     -- 114   "" 
   | POP_JUMP_IF_TRUE      -- 115   "" 
   | LOAD_GLOBAL           -- 116   Index in name list 
   | CONTINUE_LOOP         -- 119   Start of loop (absolute) 
   | SETUP_LOOP            -- 120   Target address (relative) 
   | SETUP_EXCEPT          -- 121   "" 
   | SETUP_FINALLY         -- 122   "" 
   | LOAD_FAST             -- 124   Local variable number 
   | STORE_FAST            -- 125   Local variable number 
   | DELETE_FAST           -- 126   Local variable number 
   | RAISE_VARARGS         -- 130   Number of raise arguments (1, 2 or 3) 
   | CALL_FUNCTION         -- 131   #args + (#kwargs<<8)   CALL_FUNCTION_XXX opcodes defined below depend on this definition 
   | MAKE_FUNCTION         -- 132   #defaults + #kwdefaults<<8 + #annotations<<16 
   | BUILD_SLICE           -- 133   Number of items 
   | MAKE_CLOSURE          -- 134   same as MAKE_FUNCTION 
   | LOAD_CLOSURE          -- 135   Load free variable from closure 
   | LOAD_DEREF            -- 136   Load and dereference from closure cell  
   | STORE_DEREF           -- 137   Store into cell  
   | DELETE_DEREF          -- 138   Delete closure cell  

   {- The next 3 opcodes must be contiguous and satisfy
      (CALL_FUNCTION_VAR - CALL_FUNCTION) & 3 == 1 -}

   | CALL_FUNCTION_VAR     -- 140   #args + (#kwargs<<8) 
   | CALL_FUNCTION_KW      -- 141   #args + (#kwargs<<8) 
   | CALL_FUNCTION_VAR_KW  -- 142   #args + (#kwargs<<8) 

   | SETUP_WITH            -- 143
   | EXTENDED_ARG          -- 144   Support for opargs more than 16 bits long 
   | LIST_APPEND           -- 145
   | SET_ADD               -- 146
   | MAP_ADD               -- 147

   {- EXCEPT_HANDLER is a special, implicit block type which is created when
      entering an except handler. It is not an opcode but we define it here
      as we want it to be available to both frameobject.c and ceval.c, while
      remaining private. -}

   {- EXCEPT_HANDLER        -- 257 -}
   deriving (Eq, Ord, Show)

opcodeToWord8 :: Map.Map Opcode Word8
opcodeToWord8 = Map.fromList opcodeList

word8ToOpcode :: Map.Map Word8 Opcode
word8ToOpcode = Map.fromList [ (y, x) | (x, y) <- opcodeList ]

opcodeList :: [(Opcode, Word8)]
opcodeList = [
   (POP_TOP, 1),
   (ROT_TWO, 2),
   (ROT_THREE, 3),
   (DUP_TOP, 4),
   (DUP_TOP_TWO, 5),
   (NOP, 9),
   (UNARY_POSITIVE, 10),
   (UNARY_NEGATIVE, 11),
   (UNARY_NOT, 12),
   (UNARY_INVERT, 15),
   (BINARY_POWER, 19),
   (BINARY_MULTIPLY, 20),
   (BINARY_MODULO, 22),
   (BINARY_ADD, 23),
   (BINARY_SUBTRACT, 24),
   (BINARY_SUBSCR, 25),
   (BINARY_FLOOR_DIVIDE, 26),
   (BINARY_TRUE_DIVIDE, 27),
   (INPLACE_FLOOR_DIVIDE, 28),
   (INPLACE_TRUE_DIVIDE, 29),
   (STORE_MAP, 54),
   (INPLACE_ADD, 55),
   (INPLACE_SUBTRACT, 56),
   (INPLACE_MULTIPLY, 57),
   (INPLACE_MODULO, 59),
   (STORE_SUBSCR, 60),
   (DELETE_SUBSCR, 61),
   (BINARY_LSHIFT, 62),
   (BINARY_RSHIFT, 63),
   (BINARY_AND, 64),
   (BINARY_XOR, 65),
   (BINARY_OR, 66),
   (INPLACE_POWER, 67),
   (GET_ITER, 68),
   (STORE_LOCALS, 69),
   (PRINT_EXPR, 70),
   (LOAD_BUILD_CLASS, 71),
   (YIELD_FROM, 72),
   (INPLACE_LSHIFT, 75),
   (INPLACE_RSHIFT, 76),
   (INPLACE_AND, 77),
   (INPLACE_XOR, 78),
   (INPLACE_OR, 79),
   (BREAK_LOOP, 80),
   (WITH_CLEANUP, 81),
   (RETURN_VALUE, 83),
   (IMPORT_STAR, 84),
   (YIELD_VALUE, 86),
   (POP_BLOCK, 87),
   (END_FINALLY, 88),
   (POP_EXCEPT, 89),
   -- (HAVE_ARGUMENT, 90),
   (STORE_NAME, 90),
   (DELETE_NAME, 91),
   (UNPACK_SEQUENCE, 92),
   (FOR_ITER, 93),
   (UNPACK_EX, 94),
   (STORE_ATTR, 95),
   (DELETE_ATTR, 96),
   (STORE_GLOBAL, 97),
   (DELETE_GLOBAL, 98),
   (LOAD_CONST, 100),
   (LOAD_NAME, 101),
   (BUILD_TUPLE, 102),
   (BUILD_LIST, 103),
   (BUILD_SET, 104),
   (BUILD_MAP, 105),
   (LOAD_ATTR, 106),
   (COMPARE_OP, 107),
   (IMPORT_NAME, 108),
   (IMPORT_FROM, 109),
   (JUMP_FORWARD, 110),
   (JUMP_IF_FALSE_OR_POP, 111),
   (JUMP_IF_TRUE_OR_POP, 112),
   (JUMP_ABSOLUTE, 113),
   (POP_JUMP_IF_FALSE, 114),
   (POP_JUMP_IF_TRUE, 115),
   (LOAD_GLOBAL, 116),
   (CONTINUE_LOOP, 119),
   (SETUP_LOOP, 120),
   (SETUP_EXCEPT, 121),
   (SETUP_FINALLY, 122),
   (LOAD_FAST, 124),
   (STORE_FAST, 125),
   (DELETE_FAST, 126),
   (RAISE_VARARGS, 130),
   (CALL_FUNCTION, 131),
   (MAKE_FUNCTION, 132),
   (BUILD_SLICE, 133),
   (MAKE_CLOSURE, 134),
   (LOAD_CLOSURE, 135),
   (LOAD_DEREF, 136),
   (STORE_DEREF, 137),
   (DELETE_DEREF, 138),
   (CALL_FUNCTION_VAR, 140),
   (CALL_FUNCTION_KW, 141),
   (CALL_FUNCTION_VAR_KW, 142),
   (SETUP_WITH, 143),
   (EXTENDED_ARG, 144),
   (LIST_APPEND, 145),
   (SET_ADD, 146),
   (MAP_ADD, 147)
   -- (EXCEPT_HANDLER, 257)
   ]


data BytecodeArg
   = Arg16 !Word16
   -- Arg32 Word32 etcetera
   deriving Show

instance Pretty BytecodeArg where
   pretty (Arg16 w) = pretty w

data Bytecode =
   Bytecode 
   { opcode :: !Opcode
   , args :: !(Maybe BytecodeArg)
   }
   deriving Show

instance Pretty Bytecode where
   pretty (Bytecode { opcode = o, args = a }) = (text $ show o) <+> pretty a

-- Number of bytes in a bytecode, counting the opcode plus any arguments
bytecodeSize :: Bytecode -> Int
bytecodeSize (Bytecode { args = a }) =
   case a of
      Nothing -> 1
      Just (Arg16 _) -> 3

newtype BytecodeSeq = BytecodeSeq [Bytecode]

-- Print a sequence of bytecode with byte offsets
instance Pretty BytecodeSeq where
   pretty (BytecodeSeq bcs) =
      vcat $ map prettyBcOffset bcsOffsets
      where
      offsets = scanl (\x y -> x + bytecodeSize y) 0 bcs
      bcsOffsets = zip offsets bcs
      prettyBcOffset :: (Int, Bytecode) -> Doc
      prettyBcOffset (i, bc) = int i <+> pretty bc

-- XXX is there a nicer way to define this?
hasArg :: Opcode -> Bool
hasArg = (>= STORE_NAME)

-- XXX probably should have Error type
decode :: ByteString -> [Bytecode]
decode = decodeWords . B.unpack

-- XXX probably should have error type
encode :: [Bytecode] -> ByteString
encode = B.pack . concatMap encodeBytecode

encodeBytecode :: Bytecode -> [Word8]
encodeBytecode (Bytecode opcode arg)
   = case Map.lookup opcode opcodeToWord8 of
        Nothing -> error ("bad opcode: " ++ show opcode)
        Just w1 ->
           case arg of
              Nothing -> [w1]
              Just (Arg16 arg16) -> 
                 let (w2, w3) = word16ToWord8s arg16
                 in [w1, w2, w3]

decodeWords :: [Word8] -> [Bytecode]
decodeWords [] = []
decodeWords (w:ws) =
   case Map.lookup w word8ToOpcode of
      Nothing -> error ("bad opcode: " ++ show (w:ws))
      Just opcode ->
         if hasArg opcode
            then
               case ws of
                  w1:w2:rest ->
                     let arg16 = Arg16 $ word8sToWord16 w1 w2 in
                     Bytecode opcode (Just arg16) : decodeWords rest
                  _other ->
                     error ("truncated bytecode stream: " ++ show (w:ws))
            else
               Bytecode opcode Nothing : decodeWords ws

word8sToWord16 :: Word8 -> Word8 -> Word16
word8sToWord16 w1 w2 = (fromIntegral w1) + (fromIntegral w2 `shiftL` 8)

word16ToWord8s :: Word16 -> (Word8, Word8)
word16ToWord8s w1 = (w2, w3)
   where
   w2 = fromIntegral (w1 .&. 255)
   w3 = fromIntegral (w1 `shiftR` 8)
