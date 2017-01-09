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
        I_ADD
      | I_OR
      | I_ADC
      | I_SBB
      | I_AND
      | I_SUB
      | I_XOR
      | I_CMP
      | I_HLT
      | I_CMC
      | I_CLC
      | I_STC
      | I_CLI
      | I_STI
      | I_CLD
      | I_STD
      | I_IN
      | I_OUT
      | I_CALL
      | I_JMP
      | I_LOOPNZ
      | I_LOOPE
      | I_LOOP
      | I_JECXZ
      | I_FFREEP
      | I_FXCH7
      | I_FSTP8
      | I_FSTP9
      | I_FSTSW
      | I_FUCOMIP
      | I_FCOMIP
      | I_FILD
      | I_FISTTP
      | I_FIST
      | I_FISTP
      | I_FBLD
      | I_FBSTP
      | I_WAIT
      | I_PUSH
      | I_POP
      | I_CWD
      | I_CDQ
      | I_CQO
      | I_MOVSB
      | I_MOVSQ
      | I_MOVSD
      | I_MOVSW
      | I_CMPSB
      | I_CMPSQ
      | I_CMPSD
      | I_CMPSW
      | I_STOSB
      | I_STOSQ
      | I_STOSD
      | I_STOSW
      | I_LODSB
      | I_LODSQ
      | I_LODSD
      | I_LODSW
      | I_SCASB
      | I_SCASQ
      | I_SCASD
      | I_SCASW
      | I_ENTER
      | I_ROL
      | I_ROR
      | I_RCL
      | I_RCR
      | I_SHL
      | I_SHR
      | I_SAL
      | I_SAR
      | I_NOP
      | I_PAUSE
      | I_XCHG
      | I_IMUL
      | I_MOVSXD
      | I_JO
      | I_JNO
      | I_JB
      | I_JAE
      | I_JZ
      | I_JNZ
      | I_JBE
      | I_JA
      | I_JS
      | I_JNS
      | I_JP
      | I_JNP
      | I_JL
      | I_JGE
      | I_JLE
      | I_JG
      | I_OUTSD
      | I_OUTSW
      | I_OUTSB
      | I_INSD
      | I_INSW
      | I_INSB
      | I_TEST
      | I_MOV
      | I_LEA
      | I_PUSHFQ
      | I_PUSHF
      | I_POPFQ
      | I_POPF
      | I_CBW
      | I_CWDE
      | I_CDQE
      | I_LAHF
      | I_SAHF
      | I_IRETQ
      | I_IRETD
      | I_IRETW
      | I_INTO
      | I_INT
      | I_RETF
      | I_LEAVE
      | I_RET
      | I_XLATB
      | I_NOT
      | I_NEG
      | I_MUL
      | I_IDIV
      | I_DIV
      | I_INC
      | I_DEC
      | I_INT1
      | I_INT3
      | I_FADD
      | I_FMUL
      | I_FCOM
      | I_FCOMP
      | I_FSUB
      | I_FSUBR
      | I_FDIV
      | I_FDIVR
      | I_FXCH
      | I_FNOP
      | I_FST
      | I_FSTP
      | I_FLD
      | I_FLDENV
      | I_OUTSQ
      | I_INSQ
      | I_FIADD
      | I_FIMUL
      | I_FICOM
      | I_FICOMP
      | I_FISUB
      | I_FISUBR
      | I_FIDIV
      | I_FIDIVR
      | I_FFREE
      | I_FRSTOR
      | I_FCOM2
      | I_SLDT
      | I_FLDCW
      | I_BSWAP
      | I_SYSCALL
      | I_FCMOVB
      | I_FCMOVE
      | I_FCMOVBE
      | I_FCMOVU
      | I_FCMOVNB
      | I_FCMOVNE
      | I_FCMOVNBE
      | I_FCMOVNU
      | I_FDIVRP
      | I_FDIVP
      | I_FSUBRP
      | I_FSUBP
      | I_FCOMPP
      | I_FMULP
      | I_FADDP
      | I_FSTCW
      | I_FCOS
      | I_FSIN
      | I_FSCALE
      | I_FRNDINT
      | I_FSINCOS
      | I_FSQRT
      | I_FYL2XPI
      | I_FPREM
      | I_FSTENV
      | I_FDECSTP
      | I_FPREM1
      | I_FPXTRACT
      | I_FPATAN
      | I_FPTAN
      | I_FYL2XP1
      | I_FINCSTP
      | I_FYL2X
      | I_F2XM1
      | I_FLDZ
      | I_FLDLN2
      | I_FLDLG2
      | I_FLDPI
      | I_FLDL2E
      | I_FLDL2T
      | I_FLD1
      | I_FXAM
      | I_FTST
      | I_FABS
      | I_FCHS
      | I_FSTP1
      | I_STR
      | I_CLTS
      | I_FUCOMPP
      | I_SYSRET
      | I_INVD
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
