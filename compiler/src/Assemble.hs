{-# LANGUAGE RecordWildCards #-}

module Assemble (assemble) where
 
import Utils (isJump)
import Types (BlockID, BlockMap, BlockVal (..))
import Blip.Bytecode (Bytecode (..), BytecodeArg (..), Opcode (..))
import Data.Word (Word32, Word16)
import Control.Monad.State.Strict (State(..), execState, gets, put, when, modify)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List as List (foldl')

-- Blocks we've already seen in a depth-first path in the control flow graph
type BlockSeen = Set.Set BlockID
type BlockSequence = [(BlockID, [Bytecode])]
data DFSState = DFSState { dfs_seen :: BlockSeen, dfs_sequence :: BlockSequence }
type DFS = State DFSState

assemble :: BlockMap -> [Bytecode]
assemble blockMap =
   assembleSequence jumpMap blockSequence
   where
   jumpMap = makeJumpMap blockSequence
   blockSequence = dfs 0 blockMap

dfs :: BlockID -> BlockMap -> BlockSequence
dfs blockID blockMap = 
   reverse $ dfs_sequence finalState
   where
   finalState = execState (dfsM blockID blockMap) initState
   initState = DFSState { dfs_seen = Set.empty, dfs_sequence = [] }
   dfsM :: BlockID -> BlockMap -> DFS () 
   dfsM blockID blockMap = do
      seen <- gets dfs_seen 
      if blockID `Set.member` seen
         then return ()
         else do
            modify $ \state -> state { dfs_seen = Set.insert blockID seen }
            case Map.lookup blockID blockMap of
               Nothing -> return () -- XXX is this an error?
               Just blockVal -> do
                  -- XXX handle block_next
                  let bytecode = block_code blockVal
                  updateSequence blockID bytecode
                  dfsBytecode blockMap bytecode
   updateSequence :: BlockID -> [Bytecode] -> DFS ()
   updateSequence blockID bytecode = do
      sequence <- gets dfs_sequence
      modify $ \state -> state { dfs_sequence = (blockID, bytecode) : sequence }
   dfsBytecode :: BlockMap -> [Bytecode] -> DFS ()
   dfsBytecode blockMap [] = return ()
   dfsBytecode blockMap (bytecode@(Bytecode {..}) : rest) = do
      when (isJump opcode) $ dfsBytecodeJump blockMap args
      dfsBytecode blockMap rest
   dfsBytecodeJump :: BlockMap -> Maybe BytecodeArg -> DFS ()
   dfsBytecodeJump blockMap Nothing = error "jump instruction without argument"
   dfsBytecodeJump blockMap (Just (Arg16 oparg)) = dfsM oparg blockMap

type JumpMap = Map.Map BlockID Word16

makeJumpMap :: BlockSequence -> JumpMap
makeJumpMap sequence = snd $ List.foldl' updateAccum (0, Map.empty) sequence
   where
   updateAccum :: (Word16, JumpMap) -> (BlockID, [Bytecode]) -> (Word16, JumpMap)
   updateAccum (offset, jumpMap) (blockID, bytecode) =
      -- XXX should check for overflow of Word16
      (offset + fromIntegral (length bytecode), Map.insert blockID offset jumpMap)

assembleSequence :: JumpMap -> BlockSequence -> [Bytecode]
assembleSequence _jumpMap [] = []
assembleSequence jumpMap ((_blockID, bytecode) : rest) 
   = assembleBytecode jumpMap bytecode ++ assembleSequence jumpMap rest
   where
   assembleBytecode :: JumpMap -> [Bytecode] -> [Bytecode]
   assembleBytecode _jumpMap [] = []
   assembleBytecode jumpMap (bytecode@(Bytecode {..}) : rest)
      | isJump opcode = assembleBytecodeJump jumpMap opcode args : assembleBytecode jumpMap rest
      | otherwise = bytecode : assembleBytecode jumpMap rest
   assembleBytecodeJump :: JumpMap -> Opcode -> Maybe BytecodeArg -> Bytecode
   assembleBytecodeJump _jumpMap _opcode Nothing = error "jump instruction without argument"
   assembleBytecodeJump jumpMap opcode (Just (Arg16 oparg)) =
      case Map.lookup oparg jumpMap of
         Nothing -> error "jump target not known"
         Just offset -> Bytecode { opcode = opcode, args = Just (Arg16 offset) }
