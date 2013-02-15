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
   -- liftIO $ putStrLn $ unlines $ map show $ reverse labelledCode
   let (labelMap, indexedBytecode) = codeOffsets $ reverse labelledCode
       finalBytecode = applyLabelMap labelMap indexedBytecode
   -- liftIO $ print labelMap
   -- liftIO $ putStrLn $ unlines $ map show $ indexedBytecode
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
                     SETUP_LOOP -> relativeTarget code index target
                     POP_JUMP_IF_FALSE -> absoluteTarget code target 
                     POP_JUMP_IF_TRUE -> absoluteTarget code target 
                     JUMP_ABSOLUTE -> absoluteTarget code target 
                     JUMP_IF_FALSE_OR_POP -> absoluteTarget code target
                     JUMP_IF_TRUE_OR_POP -> absoluteTarget code target 

relativeTarget :: Bytecode -> Word16 -> Word16 -> Bytecode
relativeTarget code@(Bytecode {..}) index target
   = code { args = Just $ Arg16 newTarget } 
   where
   newTarget = target - (index + (fromIntegral $ bytecodeSize code))

absoluteTarget :: Bytecode -> Word16 -> Bytecode
absoluteTarget code@(Bytecode {..}) target
   = code { args = Just $ Arg16 target }
