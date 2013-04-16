{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      : StackDepth
-- Copyright   : (c) 2012, 2013 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Compute an upper bound on the stack usage of a block of bytecode. 
--  It is safe to make the stack too big (but
-- it would waste memory), but if it is too small then the interpreter will
-- probably crash (or worse, keep running in an undefined state).
--
-----------------------------------------------------------------------------

module StackDepth (maxStackDepth) where
 
import Types (AnnotatedCode (..))
import Utils (isJumpBytecode, isRelativeJump, isConditionalJump)
import Blip.Bytecode (Bytecode (..), BytecodeArg (..), Opcode (..), bytecodeSize)
import Data.Word (Word32, Word16)
import Control.Monad.RWS.Strict (RWS, runRWS, ask, local, gets, modify, when)
import qualified Data.Map as Map
import qualified Data.Set as Set (insert, member, Set, empty)
import Data.Bits ((.&.), shiftR)

type StackDepth = Word32
type InstructionIndex = Word16
type InstructionSeen = Set.Set InstructionIndex
-- Mapping from byte address (jump target) to sequence of bytecode
-- from that address onwards.
type BytecodeMap = Map.Map InstructionIndex [AnnotatedCode]
type StackDepthCache = Map.Map InstructionIndex StackDepth
type CalcStackDepth = RWS InstructionSeen () StackDepthState

maxStackDepth :: [AnnotatedCode] -> StackDepth
maxStackDepth code = 
   stackDepth_maxDepth finalState
   where
   (_, finalState, _) = runRWS (maxStackDepthM 0 code)
                               Set.empty $ initStackDepthState $
                               makeBytecodeMap code

makeBytecodeMap :: [AnnotatedCode] -> BytecodeMap
makeBytecodeMap = makeBytecodeMapAcc Map.empty
   where
   makeBytecodeMapAcc :: BytecodeMap -> [AnnotatedCode] -> BytecodeMap
   makeBytecodeMapAcc map [] = map
   makeBytecodeMapAcc map code@(instruction@(AnnotatedCode {..}) : rest)
      | isLabelled instruction = do
          let newMap = Map.insert annotatedCode_index code map
          makeBytecodeMapAcc newMap rest
      | otherwise = makeBytecodeMapAcc map rest

data StackDepthState =
   StackDepthState
   { stackDepth_bytecodeMap :: BytecodeMap
   , stackDepth_maxDepth :: !StackDepth
   , stackDepth_cache :: StackDepthCache 
   }

initStackDepthState :: BytecodeMap -> StackDepthState
initStackDepthState bytecodeMap =
   StackDepthState
   { stackDepth_bytecodeMap = bytecodeMap
   , stackDepth_maxDepth = 0 
   , stackDepth_cache = Map.empty }


isLabelled :: AnnotatedCode -> Bool
isLabelled (AnnotatedCode {..}) = not $ null annotatedCode_labels

isLoopBack :: InstructionIndex -> CalcStackDepth Bool
isLoopBack index = do
   seen <- ask
   return (index `Set.member` seen)

-- record that we've visited this jump target at this depth
-- in case we visit it again in any path. There is no point
-- traversing further if the previous visit was at an equal
-- or greater depth.

visitedDeeper :: InstructionIndex -> StackDepth -> CalcStackDepth Bool
visitedDeeper index newDepth = do
   stackDepthCache <- gets stackDepth_cache 
   case Map.lookup index stackDepthCache of
       -- not been here before at any depth
       Nothing -> return False
       Just oldDepth -> return (oldDepth >= newDepth)

recordDepth :: InstructionIndex -> StackDepth -> CalcStackDepth ()
recordDepth index depth = do
   stackDepthCache <- gets stackDepth_cache
   let newCache = Map.insert index depth stackDepthCache
   modify $ \s -> s { stackDepth_cache = newCache }

maxStackDepthM :: StackDepth -> [AnnotatedCode] -> CalcStackDepth ()
maxStackDepthM _depth [] = return ()
maxStackDepthM depth code@(instruction@(AnnotatedCode {..}) : _rest) = do
   -- check if this instruction is a jump target
   if isLabelled instruction
      then do
         seenBeforeOnPath <- isLoopBack annotatedCode_index 
         seenDeeper <- visitedDeeper annotatedCode_index depth
         if seenBeforeOnPath || seenDeeper
            -- we've seen this instruction before on this path, or
            -- we've visisted it on any path at this depth or
            -- deeper, no point in going further down this path.
            then return ()
            else local (Set.insert annotatedCode_index) $ do
                    recordDepth annotatedCode_index depth
                    maxStackDepthFurther depth code
      else
         maxStackDepthFurther depth code
   where
   maxStackDepthFurther :: StackDepth -> [AnnotatedCode] -> CalcStackDepth ()
   maxStackDepthFurther depth (instruction@(AnnotatedCode {..}) : rest) = do
       let newDepth = depth + codeStackEffect annotatedCode_bytecode
       updateMaxDepth newDepth
       when (isJumpBytecode annotatedCode_bytecode) $
           -- follow the path of the jump
           maxStackDepthJump newDepth instruction
       -- follow the remaining instructions
       -- unless the current instruction is an unconditional jump
       when (isConditionalBytecode annotatedCode_bytecode) $
          maxStackDepthM newDepth rest
   maxStackDepthFurther _depth [] =
      error $ "maxStackDepthFurther called on empty sequence of code"

isConditionalBytecode :: Bytecode -> Bool
isConditionalBytecode (Bytecode {..}) = isConditionalJump opcode

{-
   from CPython, compile.c:

        if (instr->i_opcode == FOR_ITER) {
                target_depth = depth-2;
            } else if (instr->i_opcode == SETUP_FINALLY ||
                       instr->i_opcode == SETUP_EXCEPT) {
                target_depth = depth+3;
                if (target_depth > maxdepth)
                    maxdepth = target_depth;
            }
-}

maxStackDepthJump :: StackDepth -> AnnotatedCode -> CalcStackDepth ()
maxStackDepthJump depth instruction@(AnnotatedCode {..}) = do
   let targetDepth =
          case opcode annotatedCode_bytecode of
             FOR_ITER -> depth - 2
             SETUP_FINALLY -> depth + 3
             SETUP_EXCEPT -> depth + 3
             _other -> depth
   updateMaxDepth targetDepth 
   code <- getJumpToCode instruction
   maxStackDepthM targetDepth code 

getJumpToCode :: AnnotatedCode -> CalcStackDepth [AnnotatedCode]
getJumpToCode instruction@(AnnotatedCode {..}) = do
   let jumpTarget = 
          if isRelativeJump $ opcode annotatedCode_bytecode 
             then relativeTarget instruction
             else absoluteTarget instruction
   bytecodeMap <- gets stackDepth_bytecodeMap
   case Map.lookup jumpTarget bytecodeMap of
       Nothing -> error $ "Jump to uknown target: " ++ show instruction
       Just code -> return code

relativeTarget :: AnnotatedCode -> InstructionIndex
relativeTarget instruction@(AnnotatedCode {..}) =
   target + (annotatedCode_index + instructionSize)
   where
   instructionSize = fromIntegral $ bytecodeSize annotatedCode_bytecode
   target = getJumpTarget instruction

absoluteTarget :: AnnotatedCode -> InstructionIndex
absoluteTarget instruction@(AnnotatedCode {..}) 
   = getJumpTarget instruction

getJumpTarget :: AnnotatedCode -> InstructionIndex
getJumpTarget instruction@(AnnotatedCode {..}) =
   case args annotatedCode_bytecode of
      Nothing -> error $ "Jump instruction without argument: " ++ show instruction
      Just (Arg16 label) -> label

updateMaxDepth :: StackDepth -> CalcStackDepth ()
updateMaxDepth depth = do
   currentMaxDepth <- gets stackDepth_maxDepth
   when (depth > currentMaxDepth) $ 
      modify $ \s -> s { stackDepth_maxDepth = depth }

-- Compute the effect of each opcode on the depth of the stack.
-- This is used to compute an upper bound on the depth of the stack
-- for each code object. It is safe to over-estimate the depth of the
-- effect, but it is unsafe to underestimate it. Over-estimation will
-- potentially result in the stack being bigger than needed, which would
-- waste memory but otherwise be safe. Under-estimation will likely result
-- in the stack being too small and a serious fatal error in the interpreter, such
-- as segmentation fault (or reading/writing some other part of memory).
-- Some opcodes have different effect on depth depending on other factors, this function
-- convservatively takes the largest possible value.
-- This function is supposed to be identical in behaviour to opcode_stack_effect
-- in Python/compile.c.

codeStackEffect :: Bytecode -> StackDepth
codeStackEffect bytecode@(Bytecode {..}) = 
   case opcode of
      POP_TOP -> -1
      ROT_TWO -> 0
      ROT_THREE -> 0
      DUP_TOP -> 1
      DUP_TOP_TWO -> 2
      UNARY_POSITIVE -> 0
      UNARY_NEGATIVE -> 0
      UNARY_NOT -> 0
      UNARY_INVERT -> 0
      SET_ADD -> -1
      LIST_APPEND -> -1
      MAP_ADD -> -2
      BINARY_POWER -> -1
      BINARY_MULTIPLY -> -1
      BINARY_MODULO -> -1
      BINARY_ADD -> -1
      BINARY_SUBTRACT -> -1
      BINARY_SUBSCR -> -1
      BINARY_FLOOR_DIVIDE -> -1
      BINARY_TRUE_DIVIDE -> -1
      INPLACE_FLOOR_DIVIDE -> -1
      INPLACE_TRUE_DIVIDE -> -1
      INPLACE_ADD -> -1
      INPLACE_SUBTRACT -> -1
      INPLACE_MULTIPLY -> -1
      INPLACE_MODULO -> -1
      STORE_SUBSCR -> -3
      STORE_MAP -> -2
      DELETE_SUBSCR -> -2
      BINARY_LSHIFT -> -1
      BINARY_RSHIFT -> -1
      BINARY_AND -> -1
      BINARY_XOR -> -1
      BINARY_OR -> -1
      INPLACE_POWER -> -1
      GET_ITER -> 0
      PRINT_EXPR -> -1
      LOAD_BUILD_CLASS -> 1
      INPLACE_LSHIFT -> -1
      INPLACE_RSHIFT -> -1
      INPLACE_AND -> -1
      INPLACE_XOR -> -1
      INPLACE_OR -> -1
      BREAK_LOOP -> 0
      SETUP_WITH -> 7
      WITH_CLEANUP -> -1 -- Sometimes more
      STORE_LOCALS -> -1
      RETURN_VALUE -> -1
      IMPORT_STAR -> -1
      YIELD_VALUE -> 0
      YIELD_FROM -> -1
      POP_BLOCK -> 0
      POP_EXCEPT -> 0  -- -3 except if bad bytecode
      END_FINALLY -> -1 -- or -2 or -3 if exception occurred
      STORE_NAME -> -1
      DELETE_NAME -> 0
      UNPACK_SEQUENCE -> withArg $ \oparg -> oparg - 1
      UNPACK_EX -> withArg $ \oparg -> (oparg .&. 0xFF) + (oparg `shiftR` 8)
      FOR_ITER -> 1 -- or -1, at end of iterator
      STORE_ATTR -> -2
      DELETE_ATTR -> -1
      STORE_GLOBAL -> -1
      DELETE_GLOBAL -> 0
      LOAD_CONST -> 1
      LOAD_NAME -> 1
      BUILD_TUPLE -> withArg $ \oparg -> 1 - oparg
      BUILD_LIST -> withArg $ \oparg -> 1 - oparg
      BUILD_SET -> withArg $ \oparg -> 1 - oparg
      BUILD_MAP -> 1
      LOAD_ATTR -> 0
      COMPARE_OP -> -1
      IMPORT_NAME -> -1
      IMPORT_FROM -> 1
      JUMP_FORWARD -> 0
      JUMP_IF_TRUE_OR_POP -> 0 -- -1 if jump not taken
      JUMP_IF_FALSE_OR_POP -> 0 -- ditto
      JUMP_ABSOLUTE -> 0
      POP_JUMP_IF_FALSE -> -1
      POP_JUMP_IF_TRUE -> -1
      LOAD_GLOBAL -> 1
      CONTINUE_LOOP -> 0
      SETUP_LOOP -> 0
      SETUP_EXCEPT -> 6
      SETUP_FINALLY -> 6 -- can push 3 values for the new exception
                         -- plus 3 others for the previous exception state
      LOAD_FAST -> 1
      STORE_FAST -> -1
      DELETE_FAST -> 0
      RAISE_VARARGS -> withArg $ \oparg -> -1 * oparg
      CALL_FUNCTION -> withArg $ \oparg -> -1 * nargs oparg
      CALL_FUNCTION_VAR -> withArg $ \oparg -> (-1 * nargs oparg) - 1
      CALL_FUNCTION_KW -> withArg $ \oparg -> (-1 * nargs oparg) - 1 
      CALL_FUNCTION_VAR_KW -> withArg $ \oparg -> (-1 * nargs oparg) - 2
      MAKE_FUNCTION -> withArg $ \oparg -> -1 - (nargs oparg) - ((oparg `shiftR` 16) .&. 0xffff)
      MAKE_CLOSURE -> withArg $ \oparg -> -2 - (nargs oparg) - ((oparg `shiftR` 16) .&. 0xffff)
      BUILD_SLICE -> withArg $ \oparg -> if oparg == 3 then -2 else -1
      LOAD_CLOSURE -> 1
      LOAD_DEREF -> 1
      STORE_DEREF -> -1
      DELETE_DEREF -> 0
      _other -> error $ "unexpected opcode in codeStackEffect: " ++ show bytecode
   where
   -- #define NARGS(o) (((o) % 256) + 2*(((o) / 256) % 256)) 
   nargs :: Word32 -> Word32
   nargs o = (o `mod` 256) + (2 * ((o `div` 256) `mod` 256))
   withArg :: (Word32 -> Word32) -> Word32
   withArg f
      = case args of
           Nothing -> error $ "codeStackEffect: " ++ (show opcode) ++ " missing argument"
           Just (Arg16 word16) -> f $ fromIntegral word16
           -- other -> error $ "codeStackEffect unexpected opcode argument: " ++ show other
