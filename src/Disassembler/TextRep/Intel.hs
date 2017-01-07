module Disassembler.TextRep.Intel
    (
        textrep
    ) where

import Disassembler.Types

import Data.Word (Word64, Word32, Word16, Word8)
import Data.List (intercalate)
import Numeric (showHex)

textrep :: Instruction -> String
textrep (Instruction p oper operands) =
    let t1 = tp ++ (opertext oper)
        t2 = intercalate ", " oo
        oo' = (map operandtext operands)
        oo = case (ost, oper, oo') of
                ("", _, _) -> oo'
                (_, I_MOVSXD, oo1:oo2:oor) -> oo1:(ost++oo2):oor
                (_, _, oo1:oor) -> (ost++oo1):oor

        tp = concat (map ((++" ").prefixtext) p')
        p' = filter (\pfx -> case pfx of (PrefixRex _) -> False; (PrefixSeg _) -> False; _ -> True) p
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
prefixtext (PrefixRex x) = "rex " ++ (showHex x "")

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
opertest I_FDIVRP = "fdivrp"
opertest I_FDIVP = "fdivp"
opertest I_FSUBRP = "fsubrp"
opertest I_FSUBP = "fsubp"
opertest I_FCOMPP = "fcompp"
opertest I_FMULP = "fmulp"
opertest I_FADDP = "faddp"
opertest I_FSTCW = "fstcw"
opertest I_FCOS = "fcos"
opertest I_FSIN = "fsin"
opertest I_FSCALE = "fscale"
opertest I_FRNDINT = "frndint"
opertest I_FSINCOS = "fsincos"
opertest I_FSQRT = "fsqrt"
opertest I_FYL2XPI = "fyl2xpi"
opertest I_FPREM = "fprem"
opertest I_FSTENV = "fstenv"
opertest I_FDECSTP = "fdecstp"
opertest I_FPREM1 = "fprem1"
opertest I_FXTRACT = "fxtract"
opertest I_FPATAN = "fpatan"
opertest I_FPTAN = "fptan"
opertest I_FYL2XP1 = "fyl2xp1"
opertest I_FINCSTP = "fincstp"
opertest I_FYL2X = "fyl2x"
opertest I_F2XM1 = "f2xm1"
opertest I_FLDZ = "fldz"
opertest I_FLDLN2 = "fldln2"
opertest I_FLDLG2 = "fldlg2"
opertest I_FLDPI = "fldpi"
opertest I_FLDL2E = "fldl2e"
opertest I_FLDL2T = "fldl2t"
opertest I_FLD1 = "fld1"
opertest I_FXAM = "fxam"
opertest I_FTST = "ftst"
opertest I_FABS = "fabs"
opertest I_FCHS = "fchs"

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
