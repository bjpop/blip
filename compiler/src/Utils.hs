module Utils (isJump, isRelativeJump, isAbsoluteJump) where

import Blip.Bytecode (Opcode (..))

-- test if an opcode is a jump instruction
isJump :: Opcode -> Bool
isJump x = isRelativeJump x || isAbsoluteJump x

isRelativeJump :: Opcode -> Bool
isRelativeJump JUMP_FORWARD = True
isRelativeJump SETUP_LOOP = True
isRelativeJump FOR_ITER = True
isRelativeJump SETUP_FINALLY = True
isRelativeJump SETUP_EXCEPT = True
isRelativeJump SETUP_WITH = True
isRelativeJump _ = False

isAbsoluteJump :: Opcode -> Bool
isAbsoluteJump POP_JUMP_IF_FALSE = True
isAbsoluteJump POP_JUMP_IF_TRUE = True
isAbsoluteJump JUMP_ABSOLUTE = True
isAbsoluteJump CONTINUE_LOOP = True
isAbsoluteJump JUMP_IF_FALSE_OR_POP = True
isAbsoluteJump JUMP_IF_TRUE_OR_POP = True
isAbsoluteJump _ = False
