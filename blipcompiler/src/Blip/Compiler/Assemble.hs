{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Blip.Compiler.Assemble
-- Copyright   : (c) 2012, 2013, 2014 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Convert the jump targets in the annotated bytecode to real offsets.
--
-----------------------------------------------------------------------------

module Blip.Compiler.Assemble (assemble) where
 
import Data.Map as Map (lookup)
import Data.Word (Word16)
import Blip.Bytecode (Bytecode (..), BytecodeArg (..), bytecodeSize)
import Blip.Compiler.State (getBlockState, getLabelMap, modifyBlockState)
import Blip.Compiler.Utils (isRelativeJump, isAbsoluteJump)
import Blip.Compiler.Types (BlockState (..), AnnotatedCode (..), LabelMap)
import Blip.Compiler.Monad (Compile (..))

assemble :: Compile ()
assemble = do
   -- The bytecode instructions within the compiler state are
   -- in a list in reverse order.
   annotatedCode <- reverse `fmap` getBlockState state_instructions
   labelMap <- getLabelMap
   let finalAnnotatedCode = applyLabelMap labelMap annotatedCode
   modifyBlockState $ \s -> s { state_instructions = finalAnnotatedCode }

applyLabelMap :: LabelMap -> [AnnotatedCode] -> [AnnotatedCode]
applyLabelMap labelMap code =
   map fixJumpTarget code
   where
   fixJumpTarget :: AnnotatedCode -> AnnotatedCode
   fixJumpTarget annotatedCode =
      annotatedCode { annotatedCode_bytecode = newBytecode }
      where
      thisOpCode = opcode bytecode
      newBytecode
         | isRelativeJump thisOpCode = relativeTarget bytecode index jumpTarget
         | isAbsoluteJump thisOpCode = absoluteTarget bytecode jumpTarget
         | otherwise = bytecode
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
