module Diassembler
    (
        disassemble,
        Instruction(..)
    ) where

import Control.Monad (join)
import Data.ByteString.Lazy (ByteString)
import Data.Word (Word8)
import Data.Binary.Get
import Data.Bits

import qualified Data.ByteString.Lazy as B

data Instruction = Instruction {
      inPrefix   :: [Prefix]
    , inOpcode   :: Operation
    , inOperands :: [Operand]
    }

data Prefix

data Operation

data Operand

disassemble :: ByteString -> ([Instruction], ByteString)
disassemble s = case runGetOrFail disassemble1 s of
                    Left _          -> ([], s)
                    Right (r, _, i) -> let (i',r') = disassemble r in (i:i', r')

disassemble1 :: Get Instruction
disassemble1 =  disassemble1' pfxNone

data PrefixState = PrefixState {
          pfxRex ::  Maybe Word8
        , pfxO16 :: Bool
        , pfxA32 :: Bool
    }

pfxNone = PrefixState Nothing False False

-- this is the long mode (64-bit) disassembler
disassemble1' :: PrefixState -> Get Instruction
disassemble1' pfx = do
    opcode <- getWord8
    let bitW = (opcode .&. (bit 0))
        opWidth = o' bitW (fmap (\x -> (x .&. (bit 4)) /= 0) (pfxRex pfx)) (pfxO16 pfx)
            where o' 0 _ _                  = 8
                  o' 1 Nothing      False   = 32
                  o' 1 Nothing      True    = 16
                  o' 1 (Just False) False   = 32
                  o' 1 (Just False) True    = 16
                  o' 1 (Just True)  _       = 64
      in case opcode of
           _ -> fail ("invalid opcode " ++ show opcode)
