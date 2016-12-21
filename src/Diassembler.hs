module Diassembler
    (
        disassemble,
        Instruction(..)
    ) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B

data Instruction = Instruction {
      inPrefix :: [Prefix]
    , inOpcode :: Opcode
    , inOperands :: [Operand]
    }

data Prefix

data Opcode

data Operand

disassemble :: ByteString -> [Instruction]
disassemble = const []

