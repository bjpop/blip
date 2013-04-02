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
-- Compute an upper bound on the stack usage of a block of bytecode. Code
-- objects in CPython bytecode have their own stack, so the compiler must
-- say how big it needs to be. It is safe to make the stack too big (but
-- it would waste memory), but if it is too small then the interpreter will
-- probably crash (or worse, keep running in an undefined state).
--
-----------------------------------------------------------------------------

module StackDepth (maxStackDepth) where
 
import Monad (Compile (..))
import Utils (isJump)
import Blip.Bytecode (Bytecode (..), BytecodeArg (..), Opcode (..))
import Data.Word (Word32, Word16)
import Control.Monad.RWS.Strict (RWS (..), runRWS, ask, local, gets, modify, when, unless)
import qualified Data.Map as Map
import qualified Data.Set as Set (insert, member, Set, empty)
import Data.Bits ((.&.), shiftR)

maxStackDepth :: Compile Word32
maxStackDepth = return 100

type BytecodeMap = Map.Map Word16 [Bytecode]

type InstructionSeen = Set.Set Word16

data StackDepthState =
   StackDepthState
   { stackDepth_bytecodeMap :: BytecodeMap
   , stackDepth_maxDepth :: !Word32
   }

type StackDepth = RWS InstructionSeen () StackDepthState

{-
maxStackDepth :: [AnnotatedCode] -> Word32
maxStackDepth code = 
-}

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

codeStackEffect :: Bytecode -> Word32
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
      other -> error $ "unexpected opcode in codeStackEffect: " ++ show bytecode
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

{-
-- XXX should really type synonym for stack size Word32

-- the highest stack starting depth for each block
type StartDepthMap = Map.Map BlockID Word32

-- Blocks we've already seen in a depth-first path in the control flow graph
type BlockSeen = Set.Set BlockID

data StackDepthState =
   StackDepthState
   { stackDepth_blockMap :: BlockMap
   , stackDepth_maxDepth :: !Word32
   , stackDepth_startDepthMap :: StartDepthMap
   }

type StackDepth = RWS BlockSeen () StackDepthState

-- This function plays the same role as stackdepth and stackdepth_walk from
-- CPython compile.c
-- XXX This code is more complex than it ought to be. A clean solution needs
-- a nicer representation of the control flow graph.
maxStackDepth :: BlockID -> BlockMap -> Word32
maxStackDepth blockID blockMap =
   stackDepth_maxDepth finalState
   where
   (_, finalState, _) = runRWS (maxStackDepthM 0 blockID)
                               Set.empty initStackDepthState 
   initStackDepthState =
      StackDepthState
      { stackDepth_blockMap = blockMap
      , stackDepth_maxDepth = 0
      , stackDepth_startDepthMap = Map.empty
      }
   -- have to be careful that depth does not underflow back to maxbound :: Word32
   maxStackDepthM :: Word32 -> BlockID -> StackDepth ()
   maxStackDepthM depth blockID = do
      seen <- ask
      if blockID `Set.member` seen
         -- we've seen this block in this path, don't go down further
         then return ()
         -- we haven't seen this block in this path, keep going
         else do
            -- check if we previously saw this block at a deeper start depth
            wasDeeper <- wasSeenDeeper blockID depth
            if wasDeeper
               -- we saw this block at a deeper depth already, the current
               -- path can't go any deeper, so don't process it again
               -- (optimisation to avoid redundantly following paths that
               -- can't change the result)
               then return ()
               else
                  -- we haven't seen this block before in this path,
                  -- or at a start depth at least this deep in any path 
                  local (Set.insert blockID) $ do 
                     blockMap <- gets stackDepth_blockMap
                     case Map.lookup blockID blockMap of
                        Nothing -> return () -- XXX should this be an error?
                        Just blockVal -> do
                           -- compute max stack depth of the bytecodes in this block
                           -- and any blocks jumped to by the bytecode.
                           maybeDepth <- maxStackDepthBytecodes depth $ block_code blockVal 
                           -- handle the next block
                           case maybeDepth of
                              -- remaining code is dead due to previous Jump op
                              -- see "goto out" in stackdepth_walk in CPython stackdepth_walk
                              Nothing -> return ()
                              Just nextDepth ->
                                 case block_next blockVal of
                                    -- no next block
                                    Nothing -> return ()
                                    Just nextBlockID -> maxStackDepthM nextDepth nextBlockID
   -- check if this block was seen at or above this depth previously.
   -- if not, record this new depth as its start depth
   wasSeenDeeper :: BlockID -> Word32 -> StackDepth Bool
   wasSeenDeeper blockID depth = do
      startDepthMap <- gets stackDepth_startDepthMap
      -- update the start depth for this block to the new depth
      let updateStartMap = do
             let newDepthMap = Map.insert blockID depth startDepthMap
             modify $ \state -> state { stackDepth_startDepthMap = newDepthMap }
      case Map.lookup blockID startDepthMap of
         -- never seen this block before
         Nothing -> updateStartMap >> return False
         -- seen it before
         Just startDepth ->
            if startDepth >= depth
               -- already seen it at at least this depth
               then return True
               -- was not previously deeper
               else updateStartMap >> return False
   maxStackDepthBytecodes :: Word32 -> [Bytecode] -> StackDepth (Maybe Word32)
   -- return Nothing if the remaining code is dead, otherwise return
   -- the current stack depth
   maxStackDepthBytecodes depth [] = return $ Just depth
   maxStackDepthBytecodes depth (bytecode@(Bytecode {..}) : rest) = do
      modify $ updateMaxDepth newDepth
      when (isJump opcode) $ maxStackDepthJump newDepth opcode args 
      -- remaining code is dead if instruction is a non-conditional jump
      if (opcode == JUMP_ABSOLUTE || opcode == JUMP_FORWARD)
         then return Nothing
         else maxStackDepthBytecodes newDepth rest
      where
      newDepth = depth + codeStackEffect bytecode
   updateMaxDepth :: Word32 -> StackDepthState -> StackDepthState
   updateMaxDepth newDepth state
      | newDepth > maxDepth = state { stackDepth_maxDepth = newDepth }
      | otherwise = state
      where
      maxDepth = stackDepth_maxDepth state
   maxStackDepthJump :: Word32 -> Opcode -> Maybe BytecodeArg -> StackDepth ()
   maxStackDepthJump _depth _opcode Nothing =
      error "jump instruction without argument"
   maxStackDepthJump depth opcode (Just (Arg16 oparg)) =
      maxStackDepthJumpOp depth opcode oparg
   maxStackDepthJumpOp :: Word32 -> Opcode -> BlockID -> StackDepth ()
   maxStackDepthJumpOp depth FOR_ITER blockID =
      maxStackDepthM (depth - 2) blockID
   maxStackDepthJumpOp depth SETUP_FINALLY blockID =
      maxStackDepthJumpSetup depth blockID
   maxStackDepthJumpOp depth SETUP_EXCEPT blockID =
      maxStackDepthJumpSetup depth blockID
   maxStackDepthJumpOp depth _otherOp blockID =
      maxStackDepthM depth blockID
   maxStackDepthJumpSetup :: Word32 -> BlockID -> StackDepth ()
   maxStackDepthJumpSetup depth blockID = do
      let newDepth = depth + 3
      modify $ updateMaxDepth newDepth
      maxStackDepthM newDepth blockID
-}
