module Disassembler.Types
    (
        Instruction(..),
        Prefix(..),
        Operation(..),
        Operand(..),
        Register(..),
        GPR(..),
        SReg(..),
        FPUReg(..),
        RegHalf(..),
        Immediate(..),
        ImmediateS, ImmediateU
    ) where

import Data.Word (Word8, Word64)
import Data.Int (Int64)

data Instruction = Instruction {
      inPrefix    :: [Prefix]
    , inOperation :: Operation
    , inOperands  :: [Operand]
    } deriving (Show, Eq)

data Prefix =
      PrefixO16
    | PrefixA32
    | PrefixRepNE
    | PrefixRep
    | PrefixLock
    | PrefixSeg SReg
    | PrefixRex Word8
    deriving (Show, Eq)

data Operation =
    Operation String
    deriving (Show, Eq)

data Operand =
        Op_Mem {
                mSize  :: Int
              , mASize :: Int
              , mReg   :: Register
              , mIdx   :: Register
              , mScale :: Word8
              , mDisp  :: ImmediateS
              , mSeg   :: Maybe SReg
              }
      | Op_Reg Register
      | Op_Imm ImmediateU
      | Op_Jmp ImmediateU
      | Op_Const Int
      | Op_Near Operand
      | Op_Far Operand
    deriving (Show, Eq)

data Register =
        RegNone
      | Reg8 GPR RegHalf
      | Reg16 GPR
      | Reg32 GPR
      | Reg64 GPR
      | RegSeg SReg
      | RegFPU FPUReg
    deriving (Show, Eq)

data GPR = RAX | RCX | RDX | RBX | RSP | RBP | RSI | RDI | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 | RIP
    deriving (Show, Eq, Ord, Enum)

data SReg = ES | CS | SS | DS | FS | GS | SR6 | SR7
    deriving (Show, Eq, Ord, Enum)

data FPUReg = ST0 | ST1 | ST2 | ST3 | ST4 | ST5 | ST6 | ST7
    deriving (Show, Eq, Ord, Enum)

data RegHalf = HalfL | HalfH
    deriving (Show, Eq, Ord, Enum)

data Immediate t = Immediate {
        iSize :: Int
      , iValue :: t
    } deriving (Show, Eq)

type ImmediateS = Immediate Int64
type ImmediateU = Immediate Word64
