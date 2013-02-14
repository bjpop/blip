{-# LANGUAGE RecordWildCards #-}

module Assemble (assemble) where
 
import Utils (isJump, unlabel)
import Types (BlockState (..), Labelled (..))
import State (getBlockState)
import Blip.Bytecode (Bytecode (..), BytecodeArg (..), Opcode (..), bytecodeSize)
import Control.Monad.Trans (liftIO)
import Monad (Compile (..))
import Data.Map as Map (Map, insert, empty, lookup)
import Data.Word (Word16)
import Data.List as List (foldl')

assemble :: Compile [Bytecode]
assemble = do
   labelledCode <- getBlockState state_instructions
   liftIO $ putStrLn $ unlines $ map show $ reverse labelledCode
   let (labelMap, indexedBytecode) = codeOffsets $ reverse labelledCode
       finalBytecode = applyLabelMap labelMap indexedBytecode
   liftIO $ print labelMap
   liftIO $ putStrLn $ unlines $ map show $ indexedBytecode
   return finalBytecode

type LabelMap = Map.Map Word16 Word16
type IndexedBytecode = [(Bytecode, Word16)]

-- Build a mapping from label to offset, and a list of bytecodes paired with their offset.
codeOffsets :: [Labelled Bytecode] -> (LabelMap, IndexedBytecode) 
codeOffsets code = (labelMap, reverse indexedBytecode)
   where
   (_index, labelMap, indexedBytecode) =
      List.foldl' updateAccum (0, Map.empty, []) code
   updateAccum :: (Word16, LabelMap, IndexedBytecode) -> 
                  Labelled Bytecode -> (Word16, LabelMap, IndexedBytecode)
   updateAccum (offset, labelMap, indexedBytecode) (Labelled code label) =
      (newOffset offset code,
       Map.insert label offset labelMap,
       (code, offset) : indexedBytecode)
   updateAccum (offset, labelMap, indexedBytecode) (UnLabelled code) =
      (newOffset offset code, labelMap, (code, offset) : indexedBytecode)
   newOffset :: Word16 -> Bytecode -> Word16
   newOffset old code = old + fromIntegral (bytecodeSize code)

{-
-- calculate how big a bytecode is in 16 bit words
codeSize :: Bytecode -> Word16
codeSize (Bytecode { args = Nothing }) = 1
codeSize (Bytecode { args = Just _ }) = 2
-}

applyLabelMap :: LabelMap -> IndexedBytecode -> [Bytecode]
applyLabelMap labelMap code =
   map fixLabel code
   where
   fixLabel :: (Bytecode, Word16) -> Bytecode
   fixLabel indexedCode@(code@(Bytecode {..}), index)
      | isJump opcode = fixJump indexedCode
      | otherwise = code 
   fixJump :: (Bytecode, Word16) -> Bytecode
   fixJump (code@(Bytecode {..}), index) =
      case args of
         Nothing -> error $ "Jump instruction without argument: " ++ show code 
         Just (Arg16 label) -> 
            case Map.lookup label labelMap of
               Nothing -> error $ "Jump instruction to unknown target label: " ++ show code
               Just target ->
                  case opcode of
                     JUMP_FORWARD -> relativeTarget code index target
                     -- SETUP_LOOP -> code { args = Just $ Arg16 target }
                     SETUP_LOOP -> relativeTarget code index target
                     POP_JUMP_IF_FALSE -> code { args = Just $ Arg16 target }
                     POP_JUMP_IF_TRUE -> code { args = Just $ Arg16 target }
                     JUMP_ABSOLUTE -> code { args = Just $ Arg16 target }
                     JUMP_IF_FALSE_OR_POP -> code { args = Just $ Arg16 target }
                     JUMP_IF_TRUE_OR_POP -> code { args = Just $ Arg16 target }

-- XXX not sure if this calculation is right
relativeTarget :: Bytecode -> Word16 -> Word16 -> Bytecode
relativeTarget code@(Bytecode {..}) index target
   = code { args = Just $ Arg16 newTarget } 
   where
   newTarget = target - (index + (fromIntegral $ bytecodeSize code))

{-
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
-}
