{-# LANGUAGE RecordWildCards #-}

module Assemble (assemble) where
 
import Utils (isJumpBytecode)
import Types (BlockState (..), AnnotatedCode (..))
import State (getBlockState)
import Blip.Bytecode (Bytecode (..), BytecodeArg (..), Opcode (..), bytecodeSize)
import Control.Monad.Trans (liftIO)
import Monad (Compile (..))
import Data.Map as Map (Map, insert, empty, lookup)
import Data.Word (Word16)
import Data.List as List (foldl')

assemble :: Compile [Bytecode]
assemble = do
   annotatedCode <- reverse `fmap` getBlockState state_instructions
   let labelMap = codeOffsets annotatedCode 
       finalBytecode = applyLabelMap labelMap annotatedCode
   return finalBytecode

type LabelMap = Map.Map Word16 Word16

-- Build a mapping from label to offset, and a list of bytecodes paired with their offset.
codeOffsets :: [AnnotatedCode] -> LabelMap
codeOffsets code = List.foldl' updateAccum Map.empty code
   where
   updateAccum :: LabelMap -> AnnotatedCode -> LabelMap
   updateAccum labelMap (Labelled {..}) =
      Map.insert annotatedCode_label annotatedCode_index labelMap
   updateAccum labelMap (UnLabelled {..}) = labelMap

applyLabelMap :: LabelMap -> [AnnotatedCode] -> [Bytecode]
applyLabelMap labelMap code =
   map fixJumpTarget code
   where
   fixJumpTarget :: AnnotatedCode -> Bytecode
   fixJumpTarget annotatedCode =
      case opcode bytecode of
         JUMP_FORWARD -> relativeTarget bytecode index jumpTarget 
         SETUP_LOOP -> relativeTarget bytecode index jumpTarget
         POP_JUMP_IF_FALSE -> absoluteTarget bytecode jumpTarget 
         POP_JUMP_IF_TRUE -> absoluteTarget bytecode jumpTarget 
         JUMP_ABSOLUTE -> absoluteTarget bytecode jumpTarget 
         JUMP_IF_FALSE_OR_POP -> absoluteTarget bytecode jumpTarget
         JUMP_IF_TRUE_OR_POP -> absoluteTarget bytecode jumpTarget 
         other -> bytecode
      where
      bytecode = annotatedCode_bytecode annotatedCode
      index = annotatedCode_index annotatedCode
      jumpTarget =
         case args bytecode of
            Nothing ->
               error $ "Jump instruction without argument: " ++ show code 
            Just (Arg16 label) -> 
               case Map.lookup label labelMap of
                  Nothing ->
                     error $ "Jump instruction to unknown target label: " ++ show code
                  Just target -> target

relativeTarget :: Bytecode -> Word16 -> Word16 -> Bytecode
relativeTarget code@(Bytecode {..}) index target
   = code { args = Just $ Arg16 newTarget } 
   where
   newTarget = target - (index + (fromIntegral $ bytecodeSize code))

absoluteTarget :: Bytecode -> Word16 -> Bytecode
absoluteTarget code@(Bytecode {..}) target
   = code { args = Just $ Arg16 target }
