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
        , Immediate(..)
    ) where

import Control.Monad (join)
import Data.ByteString.Lazy (ByteString)
import Data.Word (Word64, Word32, Word16, Word8)
import Data.Int (Int64)
import Data.Binary.Get
import Data.Bits
import Data.List (intercalate)
import Data.Maybe (isJust, isNothing, fromJust)
import Control.Applicative ( (<|>) )
import Numeric (showHex)

import qualified Data.ByteString.Lazy as B

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
      | I_JRCXZ
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

textrep :: Instruction -> String
textrep (Instruction p oper operands) =
    let t1 = tp ++ (opertext oper)
        t2 = intercalate ", " oo
        oo' = (map operandtext operands)
        oo = case (ost, oper, oo') of
                ("", _, _) -> oo'
                (_, I_MOVSXD, oo1:oo2:oor) -> oo1:(ost++oo2):oor
                (_, _, oo1:oor) -> (ost++oo1):oor

        tp = concat (map ((++" ").prefixtext) p)
        ao = (ambiSelect oper operands)
        a = (not (null operands)
                                && (isAmbiguousSizeInstr oper)
                                && (all isAmbiguousSize ao))
        ost = if (null ao) || ((noByte oper) && (operandsize (head ao) == 8))
                then ""
                else if a
                    then (operandsizespec (head ao)) ++ " "
                    else ""
      in case t2 of "" -> t1
                    _  -> t1 ++ " " ++ t2

ambiSelect I_RCR    = take 1
ambiSelect I_RCL    = take 1
ambiSelect I_ROL    = take 1
ambiSelect I_ROR    = take 1
ambiSelect I_SHL    = take 1
ambiSelect I_SHR    = take 1
ambiSelect I_MOVSXD = drop 1
ambiSelect _        = id

isAmbiguousSizeInstr I_INT = False
isAmbiguousSizeInstr I_RET = False
isAmbiguousSizeInstr I_RETF = False
isAmbiguousSizeInstr I_JO = False
isAmbiguousSizeInstr I_JNO = False
isAmbiguousSizeInstr I_JB = False
isAmbiguousSizeInstr I_JAE = False
isAmbiguousSizeInstr I_JZ = False
isAmbiguousSizeInstr I_JNZ = False
isAmbiguousSizeInstr I_JBE = False
isAmbiguousSizeInstr I_JA = False
isAmbiguousSizeInstr I_JS = False
isAmbiguousSizeInstr I_JNS = False
isAmbiguousSizeInstr I_JP = False
isAmbiguousSizeInstr I_JNP = False
isAmbiguousSizeInstr I_JL = False
isAmbiguousSizeInstr I_JGE = False
isAmbiguousSizeInstr I_JLE = False
isAmbiguousSizeInstr I_JG = False
isAmbiguousSizeInstr I_ENTER = False
isAmbiguousSizeInstr I_LOOP = False
isAmbiguousSizeInstr I_LOOPE = False
isAmbiguousSizeInstr I_LOOPNZ = False
isAmbiguousSizeInstr I_JRCXZ = False
isAmbiguousSizeInstr I_FBLD = False
isAmbiguousSizeInstr I_FLDENV = False
isAmbiguousSizeInstr I_SLDT = False
isAmbiguousSizeInstr I_FRSTOR = False
isAmbiguousSizeInstr _ = True

noByte I_PUSH = True
noByte _ = False

prefixtext PrefixA32 = "a32"
prefixtext PrefixO16 = "o16"
prefixtext PrefixRepNE = "repne"
prefixtext PrefixRep = "rep"
prefixtext PrefixLock = "lock"
prefixtext (PrefixSeg r) = (registertext.RegSeg) r

operandtext :: Operand -> String
operandtext (Op_Near o@(Op_Mem _ _ _ _ _ _ _)) = "near " ++ (operandtext o)
operandtext (Op_Near o)            = (operandtext o)
operandtext (Op_Far o@(Op_Mem _ _ _ _ _ _ _)) = "far " ++ (operandtext o)
operandtext (Op_Far o) = (operandtext o)
operandtext (Op_Reg r) = registertext r
operandtext (Op_Mem sz asz base idx sf ofs seg) =
    let bs = registertext base
        is = case (idx,sf)  of
                    (RegNone,_) -> ""
                    (_,1)       -> (registertext idx)
                    (_,_)       -> ((registertext idx) ++ "*" ++ (show sf))
        os = case ofs of Immediate 0 _         -> ""
                         Immediate _ 0         -> "0x0"
                         Immediate isz v | asz == isz || only || v > 0 -> ("0x" ++ (showHex (unsigned asz v)) "")
                                         |                       v < 0 -> ("-0x" ++ (showHex (negate v)) "")
        bsis = case (bs,is) of
                    ("",_) -> is
                    (_,"") -> bs
                    (_,_) -> bs ++ "+" ++ is
        str = case (bsis,os) of
                    ("",_) -> os
                    (_,"") -> bsis
                    (_,('-':_)) -> bsis ++ os
                    (_,_) -> bsis ++ "+" ++ os
        so = case (seg) of
                    Nothing -> ""
                    (Just sr) -> (registertext (RegSeg sr)) ++ ":"
        only = case (base,idx) of (RegNone,RegNone) -> True; _ -> False
        unsigned sz x | x < 0  = max + (fromIntegral x) + (1 :: Integer)
                      | x >= 0 = (fromIntegral x) :: Integer
           where max = case sz of 64 -> (fromIntegral (maxBound :: Word64))
                                  32 -> (fromIntegral (maxBound :: Word32))
                                  16 -> (fromIntegral (maxBound :: Word16))
                                  8  -> (fromIntegral (maxBound :: Word8))

     in "[" ++ so ++ str ++ "]"
operandtext (Op_Imm i) = immediatetext i
operandtext (Op_Jmp i) = immediatetext i
operandtext (Op_Const i) = show i
operandtext o = "!operand "++ (show o) ++ "!"

operandsize (Op_Mem sz _ _ _ _ _ _) = sz
operandsize (Op_Jmp (Immediate sz _)) = sz
operandsize (Op_Imm (Immediate sz _)) = sz
operandsize (Op_Reg (Reg8 _ _)) = 8
operandsize (Op_Reg (Reg16 _)) = 16
operandsize (Op_Reg (Reg32 _)) = 32
operandsize (Op_Reg (Reg64 _)) = 64
operandsize (Op_Reg (RegFPU _)) = 64
operandsize (Op_Near o) = operandsize o
operandsize (Op_Far o) = operandsize o
operandsize (Op_Const _) = 0

operandsizespec o =
    (case (operandsize o) of 8 -> "byte"; 16 -> "word"; 32 -> "dword"; 64 -> "qword"; 80 -> "tword"; sz -> (show sz))

isAmbiguousSize (Op_Reg _) = False
isAmbiguousSize (Op_Mem _ _ _ _ _ _ _) = True
isAmbiguousSize (Op_Imm _) = True
isAmbiguousSize (Op_Jmp _) = False
isAmbiguousSize (Op_Const _ ) = True
isAmbiguousSize (Op_Near o) = isAmbiguousSize o
isAmbiguousSize (Op_Far o) = isAmbiguousSize o

immediatetext (Immediate _ v) = "0x" ++ (showHex v "")

disassemble :: Word64 -> ByteString -> ([Instruction], ByteString)
disassemble ofs s = case runGetOrFail (disassemble1 ofs) s of
                    Left _          -> ([], s)
                    Right (r, b, i) -> let (i',r') = disassemble (ofs+(fromIntegral b)) r in (i:i', r')

disassemble1 :: Word64 -> Get Instruction
disassemble1 ofs = disassemble1' pfxNone ofs

data PrefixState = PrefixState {
          pfxRex  ::  Maybe Word8
        , pfxO16  :: Bool
        , pfxA32  :: Bool
        , pfxRep  :: Maybe Prefix
        , pfxLock :: Bool
        , pfxSeg  :: Maybe SReg
    }

pfxNone = PrefixState Nothing False False Nothing False Nothing

--

bits s l i = fromIntegral $ (i `shiftR` s) .&. ((1 `shiftL` l) - 1)

bitTest i v = case v of
                Nothing -> False
                Just n  -> n .&. (bit i) /= 0

-- this is the long mode (64-bit) disassembler
disassemble1' :: PrefixState -> Word64 -> Get Instruction
disassemble1' pfx ofs = do
    opcode <- getWord8
    let bitW = (opcode .&. (bit 0))
        bitD = (opcode .&. (bit 1))
        opWidth = o' bitW (bitTest 3 (pfxRex pfx)) (pfxO16 pfx)
            where
                o' 0 _     _     = 8
                o' _ False True  = 16
                o' 1 False False = 32
                o' _ True  _     = 64
        opWidthA = o' (bitTest 3 (pfxRex pfx)) (pfxO16 pfx)
            where
                o' _ False   = 64
                o' _ True    = 16
        opWidth' = o' (bitTest 3 (pfxRex pfx)) (pfxO16 pfx)
            where
                o' True _       = 64
                o' _    False   = 32
                o' _    True    = 16
        opWidthIO = o' bitW (bitTest 3 (pfxRex pfx)) (pfxO16 pfx)
            where
                o' 0 _ _       = 8
                o' _ _ True    = 16
                o' 1 _ False   = 32
        opWidthPI = o' bitW (bitTest 3 (pfxRex pfx)) (pfxO16 pfx)
            where
                o' _ _ True    = 16
                o' _ _ False   = 32
      in case opcode of
        0x00 -> op2 I_ADD pfx opWidth bitD
        0x01 -> op2 I_ADD pfx opWidth bitD
        0x02 -> op2 I_ADD pfx opWidth bitD
        0x03 -> op2 I_ADD pfx opWidth bitD
        0x04 -> opImm I_ADD pfx 8
        0x05 -> opImm I_ADD pfx opWidth
        0x06 -> fail "invalid"
        0x07 -> fail "invalid"
        0x08 -> op2 I_OR pfx opWidth bitD
        0x09 -> op2 I_OR pfx opWidth bitD
        0x0a -> op2 I_OR pfx opWidth bitD
        0x0b -> op2 I_OR pfx opWidth bitD
        0x0c -> opImm I_OR pfx 8
        0x0d -> opImm I_OR pfx opWidth
        0x0e -> fail "invalid"
        0x0f -> grp0f pfx ofs

        0x10 -> op2 I_ADC pfx opWidth bitD
        0x11 -> op2 I_ADC pfx opWidth bitD
        0x12 -> op2 I_ADC pfx opWidth bitD
        0x13 -> op2 I_ADC pfx opWidth bitD
        0x14 -> opImm I_ADC pfx 8
        0x15 -> opImm I_ADC pfx opWidth
        0x16 -> fail "invalid"
        0x17 -> fail "invalid"
        0x18 -> op2 I_SBB pfx opWidth bitD
        0x19 -> op2 I_SBB pfx opWidth bitD
        0x1a -> op2 I_SBB pfx opWidth bitD
        0x1b -> op2 I_SBB pfx opWidth bitD
        0x1c -> opImm I_SBB pfx 8
        0x1d -> opImm I_SBB pfx opWidth
        0x1e -> fail "invalid"
        0x1f -> fail "invalid"

        0x20 -> op2 I_AND pfx opWidth bitD
        0x21 -> op2 I_AND pfx opWidth bitD
        0x22 -> op2 I_AND pfx opWidth bitD
        0x23 -> op2 I_AND pfx opWidth bitD
        0x24 -> opImm I_AND pfx 8
        0x25 -> opImm I_AND pfx opWidth
        0x26 -> disassemble1' (pfx { pfxSeg = Just ES }) ofs
        0x27 -> fail "invalid"
        0x28 -> op2 I_SUB pfx opWidth bitD
        0x29 -> op2 I_SUB pfx opWidth bitD
        0x2a -> op2 I_SUB pfx opWidth bitD
        0x2b -> op2 I_SUB pfx opWidth bitD
        0x2c -> opImm I_SUB pfx 8
        0x2d -> opImm I_SUB pfx opWidth
        0x2e -> disassemble1' (pfx { pfxSeg = Just CS }) ofs
        0x2f -> fail "invalid"

        0x30 -> op2 I_XOR pfx opWidth bitD
        0x31 -> op2 I_XOR pfx opWidth bitD
        0x32 -> op2 I_XOR pfx opWidth bitD
        0x33 -> op2 I_XOR pfx opWidth bitD
        0x34 -> opImm I_XOR pfx 8
        0x35 -> opImm I_XOR pfx opWidth
        0x36 -> disassemble1' (pfx { pfxSeg = Just SS }) ofs
        0x37 -> fail "invalid"
        0x38 -> op2 I_CMP pfx opWidth bitD
        0x39 -> op2 I_CMP pfx opWidth bitD
        0x3a -> op2 I_CMP pfx opWidth bitD
        0x3b -> op2 I_CMP pfx opWidth bitD
        0x3c -> opImm I_CMP pfx 8
        0x3d -> opImm I_CMP pfx opWidth
        0x3e -> disassemble1' (pfx { pfxSeg = Just DS }) ofs
        0x3f -> fail "invalid"

        0x40 -> disassemble1' (pfx { pfxRex = Just 0x40 }) ofs
        0x41 -> disassemble1' (pfx { pfxRex = Just 0x41 }) ofs
        0x42 -> disassemble1' (pfx { pfxRex = Just 0x42 }) ofs
        0x43 -> disassemble1' (pfx { pfxRex = Just 0x43 }) ofs
        0x44 -> disassemble1' (pfx { pfxRex = Just 0x44 }) ofs
        0x45 -> disassemble1' (pfx { pfxRex = Just 0x45 }) ofs
        0x46 -> disassemble1' (pfx { pfxRex = Just 0x46 }) ofs
        0x47 -> disassemble1' (pfx { pfxRex = Just 0x47 }) ofs
        0x48 -> disassemble1' (pfx { pfxRex = Just 0x48 }) ofs
        0x49 -> disassemble1' (pfx { pfxRex = Just 0x49 }) ofs
        0x4a -> disassemble1' (pfx { pfxRex = Just 0x4a }) ofs
        0x4b -> disassemble1' (pfx { pfxRex = Just 0x4b }) ofs
        0x4c -> disassemble1' (pfx { pfxRex = Just 0x4c }) ofs
        0x4d -> disassemble1' (pfx { pfxRex = Just 0x4d }) ofs
        0x4e -> disassemble1' (pfx { pfxRex = Just 0x4e }) ofs
        0x4f -> disassemble1' (pfx { pfxRex = Just 0x4f }) ofs

        0x50 -> grp50 I_PUSH (bits 0 3 opcode) opWidthA pfx
        0x51 -> grp50 I_PUSH (bits 0 3 opcode) opWidthA pfx
        0x52 -> grp50 I_PUSH (bits 0 3 opcode) opWidthA pfx
        0x53 -> grp50 I_PUSH (bits 0 3 opcode) opWidthA pfx
        0x54 -> grp50 I_PUSH (bits 0 3 opcode) opWidthA pfx
        0x55 -> grp50 I_PUSH (bits 0 3 opcode) opWidthA pfx
        0x56 -> grp50 I_PUSH (bits 0 3 opcode) opWidthA pfx
        0x57 -> grp50 I_PUSH (bits 0 3 opcode) opWidthA pfx
        0x58 -> grp50 I_POP (bits 0 3 opcode) opWidthA pfx
        0x59 -> grp50 I_POP (bits 0 3 opcode) opWidthA pfx
        0x5a -> grp50 I_POP (bits 0 3 opcode) opWidthA pfx
        0x5b -> grp50 I_POP (bits 0 3 opcode) opWidthA pfx
        0x5c -> grp50 I_POP (bits 0 3 opcode) opWidthA pfx
        0x5d -> grp50 I_POP (bits 0 3 opcode) opWidthA pfx
        0x5e -> grp50 I_POP (bits 0 3 opcode) opWidthA pfx
        0x5f -> grp50 I_POP (bits 0 3 opcode) opWidthA pfx

        0x60 -> fail "invalid"
        0x61 -> fail "invalid"
        0x62 -> fail "invalid"
        0x63 -> movsxd pfx
        0x64 -> disassemble1' (pfx { pfxSeg = Just FS }) ofs
        0x65 -> disassemble1' (pfx { pfxSeg = Just GS }) ofs
        0x66 -> disassemble1' (pfx { pfxO16 = True }) ofs
        0x67 -> disassemble1' (pfx { pfxA32 = True }) ofs
        0x68 -> pushImm pfx opWidthPI
        0x69 -> imul3 pfx opWidth' opWidth'
        0x6a -> pushImm pfx 8
        0x6b -> imul3 pfx opWidth' 8
        0x6c -> datamov I_INSB pfx [] opWidth
        0x6d -> case (bits 3 1 <$> pfxRex pfx) of
                    Just 1 -> fail "invalid"
                    _      -> let i = case opWidthIO of 32 -> I_INSD; 16 -> I_INSW
                                in datamov i pfx [] opWidth
        0x6e -> datamov I_OUTSB pfx [] opWidth
        0x6f -> let i = case opWidth of 64 -> I_OUTSQ; 32 -> I_OUTSD; 16 -> I_OUTSW
                  in datamov i pfx [] opWidth

        0x70 -> jshort I_JO pfx ofs
        0x71 -> jshort I_JNO pfx ofs
        0x72 -> jshort I_JB pfx ofs
        0x73 -> jshort I_JAE pfx ofs
        0x74 -> jshort I_JZ pfx ofs
        0x75 -> jshort I_JNZ pfx ofs
        0x76 -> jshort I_JBE pfx ofs
        0x77 -> jshort I_JA pfx ofs
        0x78 -> jshort I_JS pfx ofs
        0x79 -> jshort I_JNS pfx ofs
        0x7a -> jshort I_JP pfx ofs
        0x7b -> jshort I_JNP pfx ofs
        0x7c -> jshort I_JL pfx ofs
        0x7d -> jshort I_JGE pfx ofs
        0x7e -> jshort I_JLE pfx ofs
        0x7f -> jshort I_JG pfx ofs

        0x80 -> grp80 pfx opWidth 8
        0x81 -> grp80 pfx opWidth opWidth
        0x82 -> fail "invalid"
        0x83 -> grp80 pfx opWidth 8
        0x84 -> op2 I_TEST pfx opWidth 0
        0x85 -> op2 I_TEST pfx opWidth 0
        0x86 -> op2 I_XCHG pfx opWidth 0
        0x87 -> op2 I_XCHG pfx opWidth 0
        0x88 -> op2 I_MOV pfx 8 bitD
        0x89 -> op2 I_MOV pfx opWidth bitD
        0x8a -> op2 I_MOV pfx 8 bitD
        0x8b -> op2 I_MOV pfx opWidth bitD
        0x8c -> movsr pfx opWidth' bitD
        0x8d -> lea pfx opWidth
        0x8e -> movsr pfx opWidth' bitD
        0x8f -> pushpop I_POP pfx opWidth

        0x90 -> xchg (bits 0 3 opcode) opWidth' pfx
        0x91 -> xchg (bits 0 3 opcode) opWidth' pfx
        0x92 -> xchg (bits 0 3 opcode) opWidth' pfx
        0x93 -> xchg (bits 0 3 opcode) opWidth' pfx
        0x94 -> xchg (bits 0 3 opcode) opWidth' pfx
        0x95 -> xchg (bits 0 3 opcode) opWidth' pfx
        0x96 -> xchg (bits 0 3 opcode) opWidth' pfx
        0x97 -> xchg (bits 0 3 opcode) opWidth' pfx
        0x98 -> let i = case opWidth' of 64 -> I_CDQE; 32 -> I_CWDE; 16 -> I_CBW
                    ep = (emitPfx True False pfx)
                 in return (Instruction ep i [])
        0x99 -> let i = case opWidth' of 64 -> I_CQO; 32 -> I_CDQ; 16 -> I_CWD
                    ep = (emitPfx True False pfx)
                 in return (Instruction ep i [])
        0x9a -> fail "invalid"
        0x9b -> simple I_WAIT pfx []
        0x9c -> let ep = (emitPfx True False pfx)
                 in return (Instruction ep I_PUSHFQ [])
        0x9d -> let ep = (emitPfx True False pfx)
                 in return (Instruction ep I_POPFQ [])
        0x9e -> simple I_SAHF pfx []
        0x9f -> simple I_LAHF pfx []

        0xa0 -> movaddr pfx opWidth bitD ofs
        0xa1 -> movaddr pfx opWidth bitD ofs
        0xa2 -> movaddr pfx opWidth bitD ofs
        0xa3 -> movaddr pfx opWidth bitD ofs

        0xa4 -> datamov I_MOVSB pfx [] opWidth
        0xa5 -> let i = case opWidth' of 64 -> I_MOVSQ; 32 -> I_MOVSD; 16 -> I_MOVSW
                  in datamov i pfx [] opWidth
        0xa6 -> datamov I_CMPSB pfx [] opWidth
        0xa7 -> let i = case opWidth' of 64 -> I_CMPSQ; 32 -> I_CMPSD; 16 -> I_CMPSW
                  in datamov i pfx [] opWidth
        0xa8 -> testRax pfx opWidth
        0xa9 -> testRax pfx opWidth
        0xaa -> datamov I_STOSB pfx [] opWidth
        0xab -> let i = case opWidth' of 64 -> I_STOSQ; 32 -> I_STOSD; 16 -> I_STOSW
                    in datamov i pfx [] opWidth
        0xac -> datamov I_LODSB pfx [] opWidth
        0xad -> let i = case opWidth' of 64 -> I_LODSQ; 32 -> I_LODSD; 16 -> I_LODSW
                    in datamov i pfx [] opWidth
        0xae -> datamov I_SCASB pfx [] opWidth
        0xaf -> let i = case opWidth' of 64 -> I_SCASQ; 32 -> I_SCASD; 16 -> I_SCASW
                    in datamov i pfx [] opWidth

        0xb0 -> movreg (bits 0 3 opcode) pfx 8
        0xb1 -> movreg (bits 0 3 opcode) pfx 8
        0xb2 -> movreg (bits 0 3 opcode) pfx 8
        0xb3 -> movreg (bits 0 3 opcode) pfx 8
        0xb4 -> movreg (bits 0 3 opcode) pfx 8
        0xb5 -> movreg (bits 0 3 opcode) pfx 8
        0xb6 -> movreg (bits 0 3 opcode) pfx 8
        0xb7 -> movreg (bits 0 3 opcode) pfx 8
        0xb8 -> movreg (bits 0 3 opcode) pfx opWidth'
        0xb9 -> movreg (bits 0 3 opcode) pfx opWidth'
        0xba -> movreg (bits 0 3 opcode) pfx opWidth'
        0xbb -> movreg (bits 0 3 opcode) pfx opWidth'
        0xbc -> movreg (bits 0 3 opcode) pfx opWidth'
        0xbd -> movreg (bits 0 3 opcode) pfx opWidth'
        0xbe -> movreg (bits 0 3 opcode) pfx opWidth'
        0xbf -> movreg (bits 0 3 opcode) pfx opWidth'

        0xc0 -> shiftrot opWidth pfx
        0xc1 -> shiftrot opWidth pfx
        0xc2 -> do  i <- Immediate 16 <$> fromIntegral <$> getWord16le
                    simple I_RET pfx [Op_Imm i]
        0xc3 -> simple I_RET pfx []
        0xc4 -> fail "invalid"
        0xc5 -> fail "invalid"
        0xc6 -> movimm pfx opWidth
        0xc7 -> movimm pfx opWidth
        0xc8 -> do  op1 <- fromIntegral <$> getWord16le
                    op2 <- fromIntegral <$> getWord8
                    simple I_ENTER pfx [Op_Imm (Immediate 16 op1), Op_Imm (Immediate 8 op2)]
        0xc9 -> simple I_LEAVE pfx []
        0xca -> do op1 <- fromIntegral <$> getWord16le
                   simple I_RETF pfx [Op_Imm (Immediate 16 op1)]
        0xcb -> simple I_RETF pfx []
        0xcc -> simple I_INT3 pfx []
        0xcd -> do op1 <- fromIntegral <$> getWord8
                   simple I_INT pfx [Op_Imm (Immediate 8 op1)]
        0xce -> fail "invalid"
        0xcf -> let i = case opWidth' of 64 -> I_IRETQ; 32 -> I_IRETD; 16 -> I_IRETW
                    ep = (emitPfx True False pfx)
                  in return (Instruction ep i [])

        0xd0 -> shiftrot1 opWidth pfx (Op_Const 1)
        0xd1 -> shiftrot1 opWidth pfx (Op_Const 1)
        0xd2 -> shiftrot1 opWidth pfx (Op_Reg (Reg8 RCX HalfL))
        0xd3 -> shiftrot1 opWidth pfx (Op_Reg (Reg8 RCX HalfL))
        0xd4 -> fail "invalid"
        0xd5 -> fail "invalid"
        0xd6 -> fail "invalid"
        0xd7 -> simple I_XLATB pfx []
        0xd8 -> fpu opcode pfx ofs
        0xd9 -> fpu opcode pfx ofs
        0xda -> fpu opcode pfx ofs
        0xdb -> fpu opcode pfx ofs
        0xdc -> fpu opcode pfx ofs
        0xdd -> fpu opcode pfx ofs
        0xde -> fpu opcode pfx ofs
        0xdf -> fpu opcode pfx ofs

        0xe0 -> jshort I_LOOPNZ pfx ofs
        0xe1 -> jshort I_LOOPE pfx ofs
        0xe2 -> jshort I_LOOP pfx ofs
        0xe3 -> jshort I_JRCXZ pfx ofs
        0xe4 -> do op1 <- Op_Imm <$> Immediate 8 <$> fromIntegral <$> getWord8
                   inout pfx opWidthIO bitD op1
        0xe5 -> do op1 <- Op_Imm <$> Immediate 8 <$> fromIntegral <$> getWord8
                   inout pfx opWidthIO bitD op1
        0xe6 -> do op1 <- Op_Imm <$> Immediate 8 <$> fromIntegral <$> getWord8
                   inout pfx opWidthIO bitD op1
        0xe7 -> do op1 <- Op_Imm <$> Immediate 8 <$> fromIntegral <$> getWord8
                   inout pfx opWidthIO bitD op1
        0xe8 -> jmpcall I_CALL pfx ofs
        0xe9 -> jmpcall I_JMP pfx ofs
        0xea -> fail "invalid"
        0xeb -> jshort I_JMP pfx ofs
        0xec -> inout pfx opWidthIO bitD (Op_Reg (Reg16 RDX))
        0xed -> inout pfx opWidthIO bitD (Op_Reg (Reg16 RDX))
        0xee -> inout pfx opWidthIO bitD (Op_Reg (Reg16 RDX))
        0xef -> inout pfx opWidthIO bitD (Op_Reg (Reg16 RDX))

        0xf0 -> disassemble1' (pfx { pfxLock = True }) ofs
        0xf1 -> simple I_INT1 pfx []
        0xf2 -> disassemble1' (pfx { pfxRep = Just PrefixRepNE }) ofs
        0xf3 -> disassemble1' (pfx { pfxRep = Just PrefixRep }) ofs
        0xf4 -> simple I_HLT pfx []
        0xf5 -> simple I_CMC pfx []
        0xf6 -> grpf6 pfx opWidth
        0xf7 -> grpf6 pfx opWidth
        0xf8 -> simple I_CLC pfx []
        0xf9 -> simple I_STC pfx []
        0xfa -> simple I_CLI pfx []
        0xfb -> simple I_STI pfx []
        0xfc -> simple I_CLD pfx []
        0xfd -> simple I_STD pfx []
        0xfe -> grpfe pfx opWidth bitW
        0xff -> grpfe pfx opWidth bitW

        _ -> fail ("invalid opcode " ++ show opcode)

grp0f pfx ofs = do
    opcode2 <- getWord8
    case opcode2 of
        0x00 -> grp0f00 pfx ofs
        0x05 -> simple I_SYSCALL pfx []
        0xc8 -> bswap pfx opcode2
        0xc9 -> bswap pfx opcode2
        0xca -> bswap pfx opcode2
        0xcb -> bswap pfx opcode2
        0xcc -> bswap pfx opcode2
        0xcd -> bswap pfx opcode2
        0xce -> bswap pfx opcode2
        0xcf -> bswap pfx opcode2
        _ -> fail "invalid"

bswap pfx opcode = let
    reg = selectreg 0 (bits 0 3 opcode) (if (bitTest 3 (pfxRex pfx)) then 64 else 32) (pfxRex pfx) False
    ep = emitPfx True True pfx
    in return (Instruction ep I_BSWAP [Op_Reg reg])

grp0f00 pfx ofs = do
    rmb <- lookAhead getWord8
    case bits 3 3 rmb of
       0 -> do (rm, _, _, _, _) <- modrm pfx 32; let ep = emitPfx True True pfx in return (Instruction ep I_SLDT [rm])
       _ -> fail "invalid"

emitPfx noo16 noa32 pfx =
    (if (not noo16) && pfxO16 pfx then [PrefixO16] else []) ++
    (if (not noa32) && pfxA32 pfx then [PrefixA32] else []) ++
    (if pfxLock pfx then [PrefixLock] else []) ++
    (maybe [] (:[]) (pfxRep pfx))

inout pfx opWidth direction op1 = do
     let op2 = selectreg 0 0 opWidth (Nothing :: Maybe Word8) False
         (i,ops) = case direction of
                    0 -> (I_IN, [Op_Reg op2, op1])
                    _ -> (I_OUT, [op1, Op_Reg op2])
         ep = emitPfx (opWidth /= 8) False pfx
       in return (Instruction ep i ops)


grpf6 pfx opWidth = do
        (rm, _, op, _, _) <- modrm pfx opWidth
        let ep = emitPfx (opWidth /= 8) True pfx
            in case op of
                0 -> f6test ep rm
                1 -> f6test ep rm
                2 -> return (Instruction ep I_NOT [rm])
                3 -> return (Instruction ep I_NEG [rm])
                4 -> return (Instruction ep I_MUL [rm])
                5 -> return (Instruction ep I_IMUL [rm])
                6 -> return (Instruction ep I_DIV [rm])
                7 -> return (Instruction ep I_IDIV [rm])
    where f6test ep rm = do imm <- (Immediate opWidth) <$> case opWidth of
                                       8  -> fromIntegral <$> getWord8
                                       16 -> fromIntegral <$> getWord16le
                                       32 -> fromIntegral <$> getWord32le
                                       64 -> fromIntegral <$> getWord64le
                            return (Instruction ep I_TEST [rm, Op_Imm imm])

grpfe pfx opWidth bitW = do
     md <- lookAhead getWord8
     let op = bits 3 3 md
         ep = emitPfx (bitW == 1) True pfx
        in case (bitW,op) of
            (_,0) -> do (rm, _, op, mod, _) <- modrm pfx opWidth; return (Instruction ep I_INC [rm])
            (_,1) -> do (rm, _, op, mod, _) <- modrm pfx opWidth; return (Instruction ep I_DEC [rm])
            (1,2) -> do (rm, _, op, mod, _) <- modrm pfx 64; return (Instruction ep I_CALL [Op_Near rm])
            (1,3) -> do (rm, _, op, mod, _) <- modrm pfx opWidth; return (Instruction ep I_CALL [Op_Far rm])
            (1,4) -> do (rm, _, op, mod, _) <- modrm pfx 64; if (mod == 3) then fail "invalid" else return (Instruction ep I_JMP [Op_Near rm])
            (1,5) -> do (rm, _, op, mod, _) <- modrm pfx 32; return (Instruction ep I_JMP [Op_Far rm])
            (1,6) -> do (rm, _, op, mod, _) <- modrm pfx opWidth; return (Instruction ep I_PUSH [rm])
            _     -> fail "invalid"


grp50 i r opWidth pfx = let
        reg = selectreg 0 r opWidth (pfxRex pfx) False
        ep = emitPfx True False pfx
    in return (Instruction ep i [Op_Reg reg])

grp80 pfx opWidth immSize = do
        (rm, _, op, _, _) <- modrm pfx opWidth
        imm <- (Immediate immSize) <$> case immSize of 8  -> fromIntegral <$> getWord8
                                                       16 -> fromIntegral <$> getWord16le
                                                       32 -> fromIntegral <$> getWord32le
                                                       64 -> fromIntegral <$> getWord32le
        let i = case op of
                    0 -> I_ADD
                    1 -> I_OR
                    2 -> I_ADC
                    3 -> I_SBB
                    4 -> I_AND
                    5 -> I_SUB
                    6 -> I_XOR
                    7 -> I_CMP
            ep = emitPfx (opWidth /= 8) True pfx
          in return (Instruction ep i [rm, Op_Imm imm])

movimm pfx opWidth = do
        (rm, _, op, _, _) <- modrm pfx opWidth
        case op of 0 -> do imm <- (Immediate opWidth) <$> case opWidth of
                                       8  -> fromIntegral <$> getWord8
                                       16 -> fromIntegral <$> getWord16le
                                       32 -> fromIntegral <$> getWord32le
                                       64 -> fromIntegral <$> getWord32le -- FIXME?
                           let ep = emitPfx (opWidth /= 8) False pfx
                             in return (Instruction ep I_MOV [rm, Op_Imm imm])
                   _ -> fail "invalid"

movsr pfx opWidth direction = do
    (rm, _, sr, _, _) <- modrm pfx opWidth
    let sreg = RegSeg ( [ES, CS, SS, DS, FS, GS, SR6, SR7] !! sr )
        ops = case direction of
                    0 -> [rm, Op_Reg sreg]
                    _ -> [Op_Reg sreg, rm]
        ep = emitPfx True True pfx
      in return (Instruction ep I_MOV ops)

pushpop i pfx opWidth = do
    (rm, _, op, _, _) <- modrm pfx 64
    case op of 0 -> let ep = emitPfx True True pfx in return (Instruction ep i [rm])
               _ -> fail "invalid"

xchg r opWidth pfx = let
        reg1 = selectreg 0 r opWidth (pfxRex pfx) False
        reg2 = selectreg 0 0 opWidth (Nothing :: Maybe Word8) False
        ep = emitPfx True False pfx
    in if (reg1 == reg2)
         then return (Instruction ep (case (pfxRep pfx) of (Just PrefixRep) -> I_PAUSE; _ -> I_NOP) [])
         else return (Instruction ep I_XCHG [Op_Reg reg1, Op_Reg reg2])

movreg r pfx opWidth = do
    imm <- (Immediate opWidth) <$> case opWidth of 8  -> fromIntegral <$> getWord8
                                                   16 -> fromIntegral <$> getWord16le
                                                   32 -> fromIntegral <$> getWord32le
                                                   64 -> fromIntegral <$> getWord64le
    let reg = selectreg 0 r opWidth (pfxRex pfx) False
        ep = emitPfx (opWidth /= 8) False pfx
      in return (Instruction ep I_MOV [Op_Reg reg, Op_Imm imm])

testRax pfx opWidth = do
    imm <- (Immediate opWidth) <$> case opWidth of 8  -> fromIntegral <$> getWord8
                                                   16 -> fromIntegral <$> getWord16le
                                                   32 -> fromIntegral <$> getWord32le
                                                   64 -> fromIntegral <$> getWord32le
    let reg = selectreg 0 0 opWidth (Nothing :: Maybe Word8) False
        ep = emitPfx (opWidth /= 8) False pfx
      in return (Instruction ep I_TEST [Op_Reg reg, Op_Imm imm])

pushImm pfx opWidth = do
    imm <- case opWidth of 8  -> Immediate 8 . fromIntegral <$> getWord8
                           16 -> Immediate 16 . fromIntegral <$> getWord16le
                           32 -> Immediate 32 . fromIntegral <$> getWord32le
                           64 -> Immediate 64 . fromIntegral <$> getWord32le
    let ep = emitPfx (opWidth /= 8) False pfx
      in return (Instruction ep I_PUSH [Op_Imm imm])

lea pfx opWidth = do
    (rm, reg, _, mod, _) <- modrm' pfx 64 opWidth False
    if mod == 3 then fail "invalid" else
        let ops = [Op_Reg reg, rm]
            ep = emitPfx (opWidth /= 8) True pfx
          in return (Instruction ep I_LEA ops)

op2 i pfx opWidth direction = do
    (rm, reg, _, _, _) <- modrm pfx opWidth
    let ops = case direction of
                    0 -> [rm, Op_Reg reg]
                    _ -> [Op_Reg reg, rm]
        ep = emitPfx (opWidth /= 8) True pfx
      in return (Instruction ep i ops)

movsxd pfx = do
    (rm, reg, _, _, _) <- modrm' pfx (if bitTest 3 (pfxRex pfx) then 64 else 32) (if (pfxO16 pfx) then 16 else 32) False
    let ops = [Op_Reg reg, rm]
        ep = emitPfx True True pfx
      in return (Instruction ep I_MOVSXD ops)

movaddr pfx opWidth direction ofs = let
    aWidth = case (pfxA32 pfx) of
                 (True)  -> 32
                 (False) -> 64
     in do
        disp <- case aWidth of
                64 -> fromIntegral <$> getWord64le
                32 -> fromIntegral <$> getWord32le
        let imm = Op_Mem opWidth aWidth RegNone RegNone 0 (Immediate aWidth disp) (pfxSeg pfx)
            ep = (emitPfx (opWidth /= 8) (opWidth /= 8) pfx) -- wack?
            reg = selectreg 0 0 opWidth (Nothing :: Maybe Word8) False
            ops = case direction of
                                0 -> [Op_Reg reg, imm]
                                _ -> [imm, Op_Reg reg]
          in return (Instruction ep I_MOV ops)

imul3 pfx opWidth immSize = do
    (rm, reg, _, _, _) <- modrm pfx opWidth
    imm <- (Immediate immSize) <$> case immSize of 8  -> fromIntegral <$> getWord8
                                                   16 -> fromIntegral <$> getWord16le
                                                   32 -> fromIntegral <$> getWord32le
                                                   64 -> fromIntegral <$> getWord32le
    let ep = emitPfx (opWidth /= 8) True pfx
      in return (Instruction ep I_IMUL [Op_Reg reg, rm, Op_Imm imm])

shiftrot opWidth pfx = do
        (rm, _, op, _, _) <- modrm pfx opWidth
        imm <- (Immediate 8 . fromIntegral) <$> getWord8
        shiftrot' pfx op rm (Op_Imm imm) opWidth

shiftrot1 opWidth pfx op2 = do
        (rm, _, op, _, _) <- modrm pfx opWidth
        shiftrot' pfx op rm op2 opWidth

shiftrot' pfx op rm op2 opWidth =
        let i = case op of
                    0 -> I_ROL
                    1 -> I_ROR
                    2 -> I_RCL
                    3 -> I_RCR
                    4 -> I_SHL
                    5 -> I_SHR
                    6 -> I_SHL
                    7 -> I_SAR
            ep = emitPfx (opWidth /= 8) True pfx
          in return (Instruction ep i [rm, op2])

modrm pfx opWidth = modrm' pfx opWidth opWidth False

modrmFpu pfx opWidth = modrm' pfx opWidth opWidth True

modrm' pfx opWidth opWidth' fpu = do
    modrm <- getWord8
    let b'mod = bits 6 2 modrm
        b'reg = bits 3 3 modrm
        b'rm  = bits 0 3 modrm
        aWidth = if (pfxA32 pfx) then 32 else 64
        reg = selectreg 2 b'reg opWidth' (pfxRex pfx) fpu
        hasSib = (b'mod /= 3 && b'rm == 4)
        dispSize = case (b'mod, b'rm) of
            (0,5) -> Just 32
            (1,_) -> Just 8
            (2,_) -> Just 32
            _     -> Nothing
        so = (pfxSeg pfx)
      in do
        (sib,dispSize') <- if hasSib then (parseSib b'mod dispSize (pfxRex pfx) aWidth) <$> getWord8
                                     else return ((RegNone,RegNone,0),dispSize)
        disp <- case dispSize' of
                    Just 8 -> (Immediate 8 . fromIntegral) <$> getInt8
                    Just 32 -> (Immediate 32 . fromIntegral) <$> getInt32le
                    _  -> return $ Immediate 0 0
        let rm = case (b'mod, b'rm) of
                (3,_) -> Op_Reg (selectreg 0 b'rm opWidth (pfxRex pfx) fpu)
                (0,5) -> Op_Mem opWidth aWidth ((if aWidth == 64 then Reg64 else Reg32) RIP) RegNone 0 disp so
                (0,4) -> let (br, ir, sc) = sib in Op_Mem opWidth aWidth br ir sc disp so
                (1,4) -> let (br, ir, sc) = sib in Op_Mem opWidth aWidth br ir sc disp so
                (2,4) -> let (br, ir, sc) = sib in Op_Mem opWidth aWidth br ir sc disp so
                (_,_) -> Op_Mem opWidth aWidth (selectreg 0 b'rm aWidth (pfxRex pfx) False) RegNone 0 disp so
          in return (rm, reg, b'reg, b'mod, b'rm)

opImm i pfx opWidth = do
    iv <- case opWidth of
             8 -> fromIntegral <$> getWord8
             16 -> fromIntegral <$> getWord16le
             32 -> fromIntegral <$> getWord32le
             64 -> fromIntegral <$> getWord32le -- why?
    let reg = case opWidth of
             8 -> Reg8 RAX HalfL
             16 -> Reg16 RAX
             32 -> (if bitTest 3 (pfxRex pfx) then Reg64 else Reg32) RAX
             64 -> Reg64 RAX
        imm = Immediate opWidth iv
        ep = (emitPfx (opWidth /= 8) False pfx)
      in return (Instruction ep i [Op_Reg reg, Op_Imm imm])

simple i pfx opl = let
        ep = (emitPfx False False pfx)
    in return (Instruction ep i opl)

datamov i pfx opl opwidth = let
        seg = pfxSeg pfx
        pe' = (maybe [] ((:[]).PrefixSeg) seg)
        pe = case (i,seg) of
                    (I_OUTSB, _)       -> []
                    (I_OUTSW, _)       -> []
                    (I_OUTSD, _)       -> []
                    (I_OUTSQ, _)       -> []
                    (I_INSB, _)        -> []
                    (I_INSW, _)        -> []
                    (I_INSD, _)        -> []
                    (I_INSQ, _)        -> []
                    (I_CMPSB, _)       -> []
                    (I_CMPSW, _)       -> []
                    (I_CMPSD, _)       -> []
                    (I_CMPSQ, _)       -> []
                    (I_SCASB, _)       -> []
                    (I_SCASW, _)       -> []
                    (I_SCASD, _)       -> []
                    (I_SCASQ, _)       -> []
                    (I_MOVSB, Just SS) -> pe'
                    (I_MOVSW, Just SS) -> pe'
                    (I_MOVSD, Just SS) -> pe'
                    (I_MOVSQ, Just SS) -> pe'
                    (I_LODSB, Just SS) -> pe'
                    (I_LODSW, Just SS) -> pe'
                    (I_LODSD, Just SS) -> pe'
                    (I_LODSQ, Just SS) -> pe'
                    (I_STOSB, Just SS) -> pe'
                    (I_STOSW, Just SS) -> pe'
                    (I_STOSD, Just SS) -> pe'
                    (I_STOSQ, Just SS) -> pe'
                    (_,       Just SS) -> []
                    _                  -> pe'
        ep = (emitPfx (opwidth /= 8) False pfx) ++ pe

    in return (Instruction ep i opl)

jmpcall i pfx ofs = let
        opWidth = case (pfxO16 pfx) of
                 (True)  -> 16
                 (False) -> 32
         in do
            disp <- case opWidth of
                    16 -> fromIntegral <$> getInt16le
                    32 -> fromIntegral <$> getInt32le
            eip <- ((ofs+).fromIntegral <$> bytesRead)
            let iv = bits 0 64 (eip + disp)
                imm = Immediate 64 iv
                ep = (emitPfx True False pfx)
              in return (Instruction ep i [Op_Jmp imm])

jshort i pfx ofs = do
            disp <- fromIntegral <$> getInt8
            eip <- ((ofs+).fromIntegral <$> bytesRead)
            let iv = bits 0 64 (eip + disp)
                imm = Immediate 64 iv
                ep = (emitPfx False False pfx)
              in return (Instruction ep i [Op_Jmp imm])

fpu opcode pfx ofs = do
            rmb <- lookAhead getWord8
            let set = bits 0 3 opcode
                op = bits 0 3 rmb
                opWidth = case (set, op) of (3,0) -> 32; (3,_) -> 80; (4,_) -> 64; (5,_) -> 64; (6,_) -> 16; (7,7) -> 64; (7,5) -> 64; (7,0) -> 16; _ -> 32
             in do
                (rm, _, op, mod, reg) <- modrmFpu pfx opWidth
                let ep = (emitPfx False True pfx)
                    fpureg = (Op_Reg . RegFPU) ([ST0, ST1, ST2, ST3, ST4, ST5, ST6, ST7] !! reg)
                    r i o = return (Instruction ep i o)
                    st0 = Op_Reg (RegFPU ST0)
                    rr i = if mod == 3 then r i [st0, rm] else r i [rm]
                   in case (set, op, mod) of
                        (0, 0, _) -> rr I_FADD
                        (0, 1, _) -> rr I_FMUL
                        (0, 2, _) -> rr I_FCOM
                        (0, 3, _) -> rr I_FCOMP
                        (0, 4, _) -> rr I_FSUB
                        (0, 5, _) -> rr I_FSUBR
                        (0, 6, _) -> rr I_FDIV
                        (0, 7, _) -> rr I_FDIVR

                        (1, 0, _) -> rr I_FLD
                        (1, 1, _) -> rr I_FXCH
                        (1, 2, 3) -> if reg == 0 then r I_FNOP [] else fail "invalid"
                        (1, 2, _) -> rr I_FST
                        (1, 3, _) -> rr I_FSTP
                        (1, 3, _) -> rr I_FSTP
                        (1, 4, _) -> rr I_FLDENV
                        (1, 5, _) -> rr I_FLDCW

                        (2, 0, 3) -> r I_FCMOVB [st0, fpureg]
                        (2, 1, 3) -> r I_FCMOVE [st0, fpureg]
                        (2, 2, 3) -> r I_FCMOVBE [st0, fpureg]
                        (2, 3, 3) -> r I_FCMOVU [st0, fpureg]
                        (2, 0, _) -> r I_FIADD [rm]
                        (2, 1, _) -> r I_FIMUL [rm]
                        (2, 2, _) -> r I_FICOM [rm]
                        (2, 3, _) -> r I_FICOMP [rm]
                        (2, 4, _) -> r I_FISUB [rm]
                        (2, 5, _) -> r I_FISUBR [rm]
                        (2, 6, _) -> r I_FIDIV [rm]
                        (2, 7, _) -> r I_FIDIVR [rm]

                        (3, 0, 3) -> r I_FCMOVNB [st0, fpureg]
                        (3, 1, 3) -> r I_FCMOVNE [st0, fpureg]
                        (3, 2, 3) -> r I_FCMOVNBE [st0, fpureg]
                        (3, 3, 3) -> r I_FCMOVNU [st0, fpureg]
                        (3, 0, _) -> r I_FILD [rm]
                        (3, 1, _) -> r I_FISTTP [rm]
                        (3, 7, _) -> r I_FSTP [rm]

                        (4, 0, _) -> r I_FADD [rm]
                        (4, 1, _) -> r I_FMUL [rm]
                        (4, 2, 3) -> r I_FCOM2 [fpureg]
                        (4, 2, _) -> r I_FCOM [rm]
                        (4, 3, _) -> r I_FCOMP [rm]
                        (4, 4, _) -> r I_FSUB [rm]
                        (4, 5, _) -> r I_FSUBR [rm]
                        (4, 6, _) -> r I_FDIV [rm]
                        (4, 7, _) -> r I_FDIVR [rm]

                        (5, 1, _) -> r I_FISTTP [rm]
                        (5, 2, _) -> r I_FST [rm]
                        (5, 3, _) -> r I_FSTP [rm]
                        (5, 4, _) -> r I_FRSTOR [rm]
                        (5, 0, 0) -> r I_FLD [rm]
                        (5, 0, 3) -> r I_FFREE [rm]

                        (6, 0, _) -> r I_FIADD [rm]
                        (6, 1, _) -> r I_FIMUL [rm]
                        (6, 2, _) -> r I_FICOM [rm]
                        (6, 3, _) -> r I_FICOMP [rm]
                        (6, 4, _) -> r I_FISUB [rm]
                        (6, 5, _) -> r I_FISUBR [rm]
                        (6, 6, _) -> r I_FIDIV [rm]
                        (6, 7, _) -> r I_FIDIVR [rm]

                        (7, 0, 3) -> r I_FFREEP [fpureg]
                        (7, 0, _) -> r I_FILD [rm]
                        (7, 1, 3) -> r I_FXCH7 [fpureg]
                        (7, 1, _) -> r I_FISTTP [rm]
                        (7, 2, 3) -> r I_FSTP8 [fpureg]
                        (7, 2, _) -> r I_FIST [rm]
                        (7, 3, 3) -> r I_FSTP9 [fpureg]
                        (7, 3, _) -> r I_FISTP [rm]
                        (7, 4, 3) -> case reg of
                                    0 -> r I_FSTSW [Op_Reg (Reg16 RAX)]
                                    _ -> fail "invalid"
                        (7, 4, _) -> r I_FBLD [rm]
                        (7, 5, 3) -> r I_FUCOMIP [fpureg]
                        (7, 5, _) -> r I_FILD [rm]
                        (7, 6, 3) -> r I_FCOMIP [fpureg]
                        (7, 6, _) -> r I_FBSTP [rm]
                        (7, 7, 3) -> fail "invalid"
                        (7, 7, _) -> r I_FISTP [rm]
                        _         -> fail "invalid"

parseSib m dispSize rex aw sib = let
                         br = bits 0 3 sib
                         ir = bits 3 3 sib
                         ss = bits 6 2 sib
                         sp = (case aw of 16 -> Reg16; 32 -> Reg32; 64 -> Reg64) RSP
                         breg = selectreg 0 br aw rex False
                         ireg = selectreg 1 ir aw rex False
                         sf = case ss of { 0 -> 1; 1 -> 2; 2 -> 4; 3 -> 8 }
                    in case (m, br) of (0, 5) -> ((RegNone, if ireg == sp then RegNone else ireg, sf), Just 32)
                                       _      -> ((breg, if ireg == sp then RegNone else ireg, sf), dispSize)

selectreg rexBit reg opWidth rex fpu = let
                rvec' = case () of
                        _ | bitTest rexBit rex ->
                                [R8, R9, R10, R11, R12, R13, R14, R15]
                          | otherwise ->
                                [RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI]
                rvec = case (fpu, opWidth) of
                        (True, _) -> map RegFPU [ST0, ST1, ST2, ST3, ST4, ST5, ST6, ST7]
                        (False, 8) | isNothing rex ->
                                         [Reg8 RAX HalfL, Reg8 RCX HalfL, Reg8 RDX HalfL, Reg8 RBX HalfL,
                                          Reg8 RAX HalfH, Reg8 RCX HalfH, Reg8 RDX HalfH, Reg8 RBX HalfH]
                                   | isJust rex -> map (\i -> Reg8 i HalfL) rvec'
                        (False, 16) -> map Reg16 rvec'
                        (False, 32) -> map Reg32 rvec'
                        (False, 64) -> map Reg64 rvec'
            in rvec !! reg



--

opertext :: Operation -> String
opertext I_FADD = "fadd"
opertext I_FMUL = "fmul"
opertext I_FCOM = "fcom"
opertext I_FCOMP = "fcomp"
opertext I_FSUB = "fsub"
opertext I_FSUBR = "fsubr"
opertext I_FDIV = "fdiv"
opertext I_FDIVR = "fdivr"
opertext I_FIADD = "fiadd"
opertext I_FIMUL = "fimul"
opertext I_FICOM = "ficom"
opertext I_FICOMP = "ficomp"
opertext I_FISUB = "fisub"
opertext I_FISUBR = "fisubr"
opertext I_FIDIV = "fidiv"
opertext I_FIDIVR = "fidivr"
opertext I_ADD = "add"
opertext I_OR  = "or"
opertext I_ADC = "adc"
opertext I_SBB = "sbb"
opertext I_AND = "and"
opertext I_SUB = "sub"
opertext I_XOR = "xor"
opertext I_CMP = "cmp"
opertext I_HLT = "hlt"
opertext I_CMC = "cmc"
opertext I_CLC = "clc"
opertext I_STC = "stc"
opertext I_CLI = "cli"
opertext I_STI = "sti"
opertext I_CLD = "cld"
opertext I_STD = "std"
opertext I_IN  = "in"
opertext I_OUT = "out"
opertext I_CALL = "call"
opertext I_JMP = "jmp"
opertext I_LOOPNZ = "loopnz"
opertext I_LOOPE = "loope"
opertext I_LOOP = "loop"
opertext I_JRCXZ = "jrcxz"
opertext I_FFREEP = "ffreep"
opertext I_FXCH7 = "fxch7"
opertext I_FSTP8 = "fstp8"
opertext I_FSTP9 = "fstp9"
opertext I_FSTSW = "fstsw"
opertext I_FUCOMIP = "fucomip"
opertext I_FCOMIP = "fcomip"
opertext I_FILD = "fild"
opertext I_FISTTP = "fisttp"
opertext I_FIST = "fist"
opertext I_FISTP = "fistp"
opertext I_FBLD = "fbld"
opertext I_FBSTP = "fbstp"
opertext I_WAIT = "wait"
opertext I_PUSH = "push"
opertext I_POP = "pop"
opertext I_CWD = "cwd"
opertext I_CDQ = "cdq"
opertext I_CQO = "cqo"
opertext I_MOVSB = "movsb"
opertext I_MOVSQ = "movsq"
opertext I_MOVSD = "movsd"
opertext I_MOVSW = "movsw"
opertext I_CMPSB = "cmpsb"
opertext I_CMPSQ = "cmpsq"
opertext I_CMPSD = "cmpsd"
opertext I_CMPSW = "cmpsw"
opertext I_STOSB = "stosb"
opertext I_STOSQ = "stosq"
opertext I_STOSD = "stosd"
opertext I_STOSW = "stosw"
opertext I_LODSB = "lodsb"
opertext I_LODSQ = "lodsq"
opertext I_LODSD = "lodsd"
opertext I_LODSW = "lodsw"
opertext I_SCASB = "scasb"
opertext I_SCASQ = "scasq"
opertext I_SCASD = "scasd"
opertext I_SCASW = "scasw"
opertext I_ENTER = "enter"
opertext I_ROL = "rol"
opertext I_ROR = "ror"
opertext I_RCL = "rcl"
opertext I_RCR = "rcr"
opertext I_SHL = "shl"
opertext I_SHR = "shr"
opertext I_SAL = "sal"
opertext I_SAR = "sar"
opertext I_NOP = "nop"
opertext I_XCHG = "xchg"
opertext I_IMUL = "imul"
opertext I_MOVSXD = "movsxd"
opertext I_JO = "jo"
opertext I_JNO = "jno"
opertext I_JB = "jb"
opertext I_JAE = "jae"
opertext I_JZ = "jz"
opertext I_JNZ = "jnz"
opertext I_JBE = "jbe"
opertext I_JA = "ja"
opertext I_JS = "js"
opertext I_JNS = "jns"
opertext I_JP = "jp"
opertext I_JNP = "jnp"
opertext I_JL = "jl"
opertext I_JGE = "jge"
opertext I_JLE = "jle"
opertext I_JG = "jg"
opertext I_OUTSD = "outsd"
opertext I_OUTSW = "outsw"
opertext I_OUTSB = "outsb"
opertext I_INSD = "insd"
opertext I_INSW = "insw"
opertext I_INSB = "insb"
opertext I_MOV = "mov"
opertext I_TEST = "test"
opertext I_LEA = "lea"
opertext I_PUSHF = "pushf"
opertext I_PUSHFQ = "pushfq"
opertext I_POPF = "popf"
opertext I_POPFQ = "popfq"
opertext I_CBW = "cbw"
opertext I_CWDE = "cwde"
opertext I_CDQE = "cdqe"
opertext I_LAHF = "lahf"
opertext I_SAHF = "sahf"
opertext I_IRETW = "iretw"
opertext I_IRETD = "iretd"
opertext I_IRETQ = "iretq"
opertext I_INTO = "into"
opertext I_INT = "int"
opertext I_RETF = "retf"
opertext I_LEAVE = "leave"
opertext I_RET = "ret"
opertext I_XLATB = "xlatb"
opertext I_NOT = "not"
opertext I_NEG = "neg"
opertext I_IDIV = "idiv"
opertext I_DIV = "div"
opertext I_INC = "inc"
opertext I_DEC = "dec"
opertext I_INT1 = "int1"
opertext I_INT3 = "int3"
opertext I_FXCH = "fxch"
opertext I_FNOP = "fnop"
opertext I_FST = "fst"
opertext I_FSTP = "fstp"
opertext I_FLD = "fld"
opertext I_FLDENV = "fldenv"
opertext I_PAUSE = "pause"
opertext I_OUTSQ = "outsq"
opertext I_INSQ = "insq"
opertext I_FFREE = "ffree"
opertext I_FRSTOR = "frstor"
opertext I_FCOM2 = "fcom2"
opertext I_SLDT = "sldt"
opertext I_FLDCW = "fldcw"
opertext I_BSWAP = "bswap"
opertext I_SYSCALL = "syscall"
opertext I_FCMOVB = "fcmovb"
opertext I_FCMOVE = "fcmove"
opertext I_FCMOVBE = "fcmovbe"
opertext I_FCMOVU = "fcmovu"
opertext I_FCMOVNB = "fcmovnb"
opertext I_FCMOVNE = "fcmovne"
opertext I_FCMOVNBE = "fcmovnbe"
opertext I_FCMOVNU = "fcmovnu"
--

registertext :: Register -> String
registertext RegNone = ""

registertext (Reg64 RAX) = "rax"
registertext (Reg64 RCX) = "rcx"
registertext (Reg64 RDX) = "rdx"
registertext (Reg64 RBX) = "rbx"
registertext (Reg64 RSP) = "rsp"
registertext (Reg64 RBP) = "rbp"
registertext (Reg64 RSI) = "rsi"
registertext (Reg64 RDI) = "rdi"
registertext (Reg64 R8)  = "r8"
registertext (Reg64 R9)  = "r9"
registertext (Reg64 R10) = "r10"
registertext (Reg64 R11) = "r11"
registertext (Reg64 R12) = "r12"
registertext (Reg64 R13) = "r13"
registertext (Reg64 R14) = "r14"
registertext (Reg64 R15) = "r15"
registertext (Reg64 RIP) = "rip"

registertext (Reg32 RAX) = "eax"
registertext (Reg32 RCX) = "ecx"
registertext (Reg32 RDX) = "edx"
registertext (Reg32 RBX) = "ebx"
registertext (Reg32 RSP) = "esp"
registertext (Reg32 RBP) = "ebp"
registertext (Reg32 RSI) = "esi"
registertext (Reg32 RDI) = "edi"
registertext (Reg32 R8)  = "r8d"
registertext (Reg32 R9)  = "r9d"
registertext (Reg32 R10) = "r10d"
registertext (Reg32 R11) = "r11d"
registertext (Reg32 R12) = "r12d"
registertext (Reg32 R13) = "r13d"
registertext (Reg32 R14) = "r14d"
registertext (Reg32 R15) = "r15d"
registertext (Reg32 RIP) = "eip"

registertext (Reg16 RAX) = "ax"
registertext (Reg16 RCX) = "cx"
registertext (Reg16 RDX) = "dx"
registertext (Reg16 RBX) = "bx"
registertext (Reg16 RSP) = "sp"
registertext (Reg16 RBP) = "bp"
registertext (Reg16 RSI) = "si"
registertext (Reg16 RDI) = "di"
registertext (Reg16 R8)  = "r8w"
registertext (Reg16 R9)  = "r9w"
registertext (Reg16 R10) = "r10w"
registertext (Reg16 R11) = "r11w"
registertext (Reg16 R12) = "r12w"
registertext (Reg16 R13) = "r13w"
registertext (Reg16 R14) = "r14w"
registertext (Reg16 R15) = "r15w"
registertext (Reg16 RIP) = "ip"

registertext (Reg8 RAX HalfL) = "al"
registertext (Reg8 RCX HalfL) = "cl"
registertext (Reg8 RDX HalfL) = "dl"
registertext (Reg8 RBX HalfL) = "bl"
registertext (Reg8 RSP HalfL) = "spl"
registertext (Reg8 RBP HalfL) = "bpl"
registertext (Reg8 RSI HalfL) = "sil"
registertext (Reg8 RDI HalfL) = "dil"
registertext (Reg8 R8 HalfL)  = "r8b"
registertext (Reg8 R9 HalfL)  = "r9b"
registertext (Reg8 R10 HalfL) = "r10b"
registertext (Reg8 R11 HalfL) = "r11b"
registertext (Reg8 R12 HalfL) = "r12b"
registertext (Reg8 R13 HalfL) = "r13b"
registertext (Reg8 R14 HalfL) = "r14b"
registertext (Reg8 R15 HalfL) = "r15b"

registertext (Reg8 RAX HalfH) = "ah"
registertext (Reg8 RCX HalfH) = "ch"
registertext (Reg8 RDX HalfH) = "dh"
registertext (Reg8 RBX HalfH) = "bh"

registertext (RegFPU ST0) = "st0"
registertext (RegFPU ST1) = "st1"
registertext (RegFPU ST2) = "st2"
registertext (RegFPU ST3) = "st3"
registertext (RegFPU ST4) = "st4"
registertext (RegFPU ST5) = "st5"
registertext (RegFPU ST6) = "st6"
registertext (RegFPU ST7) = "st7"

registertext (RegSeg CS) = "cs"
registertext (RegSeg DS) = "ds"
registertext (RegSeg ES) = "es"
registertext (RegSeg SS) = "ss"
registertext (RegSeg FS) = "fs"
registertext (RegSeg GS) = "gs"
registertext (RegSeg SR6) = "cr0" -- dubious
registertext (RegSeg SR7) = "cr1" -- dubious

registertext r = "!reg:" ++ (show r) ++ "!"
