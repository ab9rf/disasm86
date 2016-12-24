module Disassembler
    (
          disassemble
        , textrep
        , Instruction(..)
        , Operation(..)
        , Operand(..)
        , Register(..)
        , GPR(..)
        , RegHalf(..)
    ) where

import Control.Monad (join)
import Data.ByteString.Lazy (ByteString)
import Data.Word (Word8, Word64)
import Data.Int (Int64)
import Data.Binary.Get
import Data.Bits
import Data.List (intercalate)

import qualified Data.ByteString.Lazy as B

data Instruction = Instruction {
      inPrefix    :: [Prefix]
    , inOperaiton :: Operation
    , inOperands  :: [Operand]
    } deriving (Show, Eq)

data Prefix = Prefix
    deriving (Show, Eq)

data Operation =
        I_ADD
    deriving (Show, Eq)

data Operand =
        Op_Mem {
                mSize :: Int
              , mBase :: Register
              , mSIB :: Maybe (Register, Word8, ImmediateU )
              }
      | Op_Reg Register
    deriving (Show, Eq)

data Register =
        RegNone
      | Reg8 GPR RegHalf
      | Reg16 GPR
      | Reg32 GPR
      | Reg64 GPR
      | RegSeg SReg
    deriving (Show, Eq)

data GPR = RAX | RCX | RDX | RBX | RSP | RBP | RSI | RDI | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
    deriving (Show, Eq, Ord, Enum)

data SReg = ES | CS | SS | DS | FS | GS
    deriving (Show, Eq, Ord, Enum)

data RegHalf = HalfL | HalfH
    deriving (Show, Eq, Ord, Enum)

data Immediate t = Immediate {
        iSize :: Int
      , iValue :: t
    } deriving (Show, Eq)

type ImmediateU = Immediate Word64
type ImmediateS = Immediate Int64

textrep :: Instruction -> String
textrep (Instruction p oper operands) =
    let t1 = opertext oper
        t2 = intercalate ", " (map operandtext operands)
      in case t2 of "" -> t1
                    _  -> t1 ++ " " ++ t2

opertext :: Operation -> String
opertext I_ADD = "add"

operandtext :: Operand -> String
operandtext (Op_Reg r) = registertext r
operandtext (Op_Mem _ base Nothing)    = "[" ++ registertext base ++ "]"
operandtext (Op_Mem _ base (Just sib)) = "[" ++ registertext base ++ "+" ++ (sibtext sib) ++ "]"

registertext :: Register -> String
registertext (Reg64 RAX) = "rax"
registertext (Reg8 RAX HalfL) = "al"

sibtext :: (Register, Word8, ImmediateU) -> String
sibtext _ = "<<sib>>" -- TODO

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
