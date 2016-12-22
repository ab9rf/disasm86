module Diassembler
    (
        disassemble,
        Instruction(..)
    ) where

import Control.Monad (join)
import Data.ByteString.Lazy (ByteString)
import Data.Word (Word8)
import Data.Binary.Get

import qualified Data.ByteString.Lazy as B

data Instruction = Instruction {
      inPrefix   :: [Prefix]
    , inOpcode   :: Opcode
    , inOperands :: [Operand]
    }

data Prefix

data Opcode

data Operand

disassemble :: ByteString -> ([Instruction], ByteString)
disassemble s = case runGetOrFail disassemble1 s of
                    Left _          -> ([], s)
                    Right (r, _, i) -> let (i',r') = disassemble r in (i:i', r')

disassemble1 :: Get Instruction
disassemble1 =  disassemble1' pfxNone

data PrefixState = PrefixState {
    }

pfxNone = PrefixState

disassemble1' :: PrefixState -> Get Instruction
disassemble1' pfx = do
    byte <- getWord8
    case byte of
        _ -> fail ("invalid opcode " ++ show byte)