module Disassembler.TextRep.Intel
    (
        textrep
    ) where

import Disassembler.Types

import Data.Word (Word64, Word32, Word16, Word8)
import Data.List (intercalate, sortBy)
import Data.Ord (comparing)
import Numeric (showHex)

textrep :: Instruction -> String
textrep i@(Instruction p oper operands) =
    let t1 = tp ++ (opertext oper)
        t2 = intercalate ", " oo
        oo' = (map operandtext operands)
        oo = case (ost, oper, oo') of
                ("", _, _) -> oo'
                (_, Operation "movsxd", oo1:oo2:oor) -> oo1:(ost++oo2):oor
                (_, _, oo1:oor) -> (ost++oo1):oor

        tp = concat (map ((++" ").prefixtext) p')
        p' = (reorderPfx (filter (pfxFilter i) p))
        ao = (ambiSelect oper operands)
        a = (not (null operands)
                                && (isAmbiguousSizeInstr i)
                                && (all isAmbiguousSize ao))
        ost = if (null ao) || ((noByte oper) && (operandsize (head ao) == 8))
                then ""
                else if a
                    then (operandsizespec (head ao)) ++ " "
                    else ""
      in case t2 of "" -> t1
                    _  -> t1 ++ " " ++ t2

reorderPfx p = sortBy ((comparing pfxOrder) ) p

pfxOrder :: Prefix -> Int
pfxOrder PrefixLock = 1
pfxOrder PrefixRepNE = 0
pfxOrder PrefixRep = 0
pfxOrder (PrefixSeg _) = 2
pfxOrder PrefixO16 = -2
pfxOrder PrefixA32 = -1
pfxOrder _ = 9


pfxFilter (Instruction _ (Operation "pause") _) PrefixRep = False

pfxFilter (Instruction _ (Operation "movsb") _) (PrefixSeg _) = True
pfxFilter (Instruction _ (Operation "movsw") _) (PrefixSeg _) = True
pfxFilter (Instruction _ (Operation "movsd") _) (PrefixSeg _) = True
pfxFilter (Instruction _ (Operation "movsq") _) (PrefixSeg _) = True
pfxFilter (Instruction _ (Operation "stosb") _) (PrefixSeg _) = True
pfxFilter (Instruction _ (Operation "stosw") _) (PrefixSeg _) = True
pfxFilter (Instruction _ (Operation "stosd") _) (PrefixSeg _) = True
pfxFilter (Instruction _ (Operation "stosq") _) (PrefixSeg _) = True
pfxFilter (Instruction _ (Operation "lodsb") _) (PrefixSeg _) = True
pfxFilter (Instruction _ (Operation "lodsw") _) (PrefixSeg _) = True
pfxFilter (Instruction _ (Operation "lodsd") _) (PrefixSeg _) = True
pfxFilter (Instruction _ (Operation "lodsq") _) (PrefixSeg _) = True

pfxFilter (Instruction _ (Operation "cbw") _) PrefixO16 = False
pfxFilter (Instruction _ (Operation "cwd") _) PrefixO16 = False
pfxFilter (Instruction _ (Operation "cqo") _) PrefixO16 = False
pfxFilter (Instruction _ (Operation "cdqe") _) PrefixO16 = False
pfxFilter (Instruction _ (Operation "pushfq") _) PrefixO16 = False
pfxFilter (Instruction _ (Operation "outsw") _) PrefixO16 = False
pfxFilter (Instruction _ (Operation "cmpsw") _) PrefixO16 = False
pfxFilter (Instruction _ (Operation "outsq") _) PrefixO16 = False
pfxFilter (Instruction _ (Operation "cmpsq") _) PrefixO16 = False
pfxFilter (Instruction _ (Operation "stosw") _) PrefixO16 = False
pfxFilter (Instruction _ (Operation "stosq") _) PrefixO16 = False
pfxFilter (Instruction _ (Operation "movsw") _) PrefixO16 = False
pfxFilter (Instruction _ (Operation "movsq") _) PrefixO16 = False
pfxFilter (Instruction _ (Operation "scasw") _) PrefixO16 = False
pfxFilter (Instruction _ (Operation "scasq") _) PrefixO16 = False
pfxFilter (Instruction _ (Operation "lodsw") _) PrefixO16 = False
pfxFilter (Instruction _ (Operation "lodsq") _) PrefixO16 = False
pfxFilter (Instruction _ (Operation "iretw") _) PrefixO16 = False
pfxFilter (Instruction _ (Operation "iretq") _) PrefixO16 = False
pfxFilter (Instruction _ (Operation "jmp") _) PrefixO16 = False
pfxFilter (Instruction _ (Operation "call") _) PrefixO16 = False
pfxFilter (Instruction _ (Operation "push") [Op_Imm (Immediate 8 _)]) PrefixO16 = True
pfxFilter (Instruction _ (Operation "push") _) PrefixO16 = False
pfxFilter (Instruction _ (Operation "pop") _) PrefixO16 = False
pfxFilter (Instruction _ (Operation "popfq") _) PrefixO16 = False

pfxFilter (Instruction _ (Operation "fstp") _) PrefixO16 = True
pfxFilter (Instruction _ (Operation "fidiv") _) PrefixO16 = True
pfxFilter (Instruction _ (Operation "fld") _) PrefixO16 = True
pfxFilter (Instruction _ (Operation "fiadd") _) PrefixO16 = True
pfxFilter (Instruction _ (Operation "fisttp") _) PrefixO16 = True
pfxFilter (Instruction _ (Operation "fnstsw") _) PrefixO16 = True
pfxFilter (Instruction _ (Operation "fisub") _) PrefixO16 = True
pfxFilter (Instruction _ (Operation "fist") _) PrefixO16 = True
pfxFilter (Instruction _ (Operation "fidivr") _) PrefixO16 = True

pfxFilter (Instruction _ (Operation "call") [Op_Far _]) PrefixA32 = True

pfxFilter (Instruction _ (Operation "in") _) PrefixA32 = True
pfxFilter (Instruction _ (Operation "out") _) PrefixA32 = True
pfxFilter (Instruction _ (Operation "in") [Op_Reg (Reg8 RAX HalfL), Op_Reg _]) PrefixO16 = True
pfxFilter (Instruction _ (Operation "in") [Op_Reg (Reg16 RAX), Op_Reg _]) PrefixO16 = False
pfxFilter (Instruction _ (Operation "out") [Op_Reg _, Op_Reg (Reg8 RAX HalfL)]) PrefixO16 = True
pfxFilter (Instruction _ (Operation "out") [Op_Reg _, Op_Reg (Reg16 RAX)]) PrefixO16 = False
pfxFilter (Instruction _ (Operation "xchg") [Op_Reg _, Op_Reg _]) PrefixA32 = True

pfxFilter (Instruction _ (Operation "insw") _) PrefixO16 = False

pfxFilter (Instruction _ (Operation "mov") [Op_Reg (RegSeg _), _]) PrefixO16 = False
pfxFilter (Instruction _ (Operation "mov") [_, Op_Reg (RegSeg _)]) PrefixO16 = False

pfxFilter (Instruction _ (Operation "jecxz") _) PrefixA32 = False
pfxFilter (Instruction _ (Operation "jrcxz") _) PrefixA32 = False

pfxFilter (Instruction _ _ ((Op_Reg (Reg64 _)):_)) PrefixO16 = False
pfxFilter (Instruction _ _ (_:(Op_Reg (Reg64 _)):_)) PrefixO16 = False
pfxFilter (Instruction _ _ ((Op_Reg (Reg16 _)):_)) PrefixO16 = False
pfxFilter (Instruction _ _ (_:(Op_Reg (Reg16 _)):_)) PrefixO16 = False
pfxFilter (Instruction _ _ ((Op_Mem 16 _ _ _ _ _ _):_)) PrefixO16 = False
pfxFilter (Instruction _ _ ((Op_Mem 64 _ _ _ _ _ _):_)) PrefixO16 = False
pfxFilter (Instruction _ _ (_:(Op_Mem 16 _ _ _ _ _ _):_)) PrefixO16 = False
pfxFilter (Instruction _ _ (_:(Op_Mem 64 _ _ _ _ _ _):_)) PrefixO16 = False

pfxFilter (Instruction _ (Operation _) [Op_Reg _, Op_Mem _ _ _ _ _ _ _]) PrefixA32 = False
pfxFilter (Instruction _ (Operation _) [Op_Mem _ 32 (Reg32 _) _ _ _ _, Op_Reg _]) PrefixA32 = False
pfxFilter (Instruction _ (Operation _) [Op_Mem _ 32 _ _ _ _ _, Op_Reg _]) PrefixA32 = True
pfxFilter (Instruction _ (Operation _) [Op_Mem _ _ _ _ _ _ _, Op_Reg _]) PrefixA32 = False

pfxFilter (Instruction _ (Operation _) [Op_Reg (Reg8 _ _), Op_Imm _]) PrefixA32 = True
pfxFilter (Instruction _ (Operation "test") [Op_Reg (Reg32 RAX), Op_Imm _]) PrefixA32 = True
pfxFilter (Instruction _ (Operation "test") [Op_Reg (Reg16 RAX), Op_Imm _]) PrefixA32 = True
pfxFilter (Instruction _ (Operation "test") [Op_Reg (Reg16 _), Op_Imm _]) PrefixA32 = False
pfxFilter (Instruction _ (Operation "test") [Op_Reg _, Op_Imm _]) PrefixA32 = False
pfxFilter (Instruction _ (Operation _) [Op_Reg _, Op_Imm _]) PrefixA32 = True

pfxFilter (Instruction _ (Operation "rol") [Op_Reg _, Op_Imm _]) PrefixA32 = False
pfxFilter (Instruction _ (Operation "shl") [Op_Reg _, Op_Imm _]) PrefixA32 = False
-- pfxFilter (Instruction _ (Operation "add") [Op_Reg _, Op_Imm _]) PrefixA32 = False

pfxFilter (Instruction _ _ ((Op_Mem _ 64 _ _ _ _ _):_)) PrefixA32 = False
pfxFilter (Instruction _ _ (_:(Op_Mem _ 64 _ _ _ _ _):_)) PrefixA32 = False
pfxFilter (Instruction _ _ ((Op_Mem _ 32 (Reg32 _) _ _ _ _):_)) PrefixA32 = False
pfxFilter (Instruction _ _ (_:(Op_Mem _ 32 (Reg32 _) _ _ _ _):_)) PrefixA32 = False
pfxFilter (Instruction _ _ [Op_Reg (RegFPU _), Op_Reg (RegFPU _)]) PrefixA32 = True
pfxFilter (Instruction _ _ [Op_Reg _, Op_Reg _]) PrefixA32 = False
pfxFilter (Instruction _ _ [Op_Reg _, Op_Const _]) PrefixA32 = False

pfxFilter _ (PrefixSeg _) = False
pfxFilter _ (PrefixRex _) = False
pfxFilter _ _ = True


ambiSelect (Operation "rcr")    = take 1
ambiSelect (Operation "rcl")    = take 1
ambiSelect (Operation "rol")    = take 1
ambiSelect (Operation "ror")    = take 1
ambiSelect (Operation "shl")    = take 1
ambiSelect (Operation "shr")    = take 1
ambiSelect (Operation "sar")    = take 1
ambiSelect (Operation "movsxd") = drop 1
ambiSelect _        = id

isAmbiguousSizeInstr (Instruction _ (Operation "shl") [Op_Mem 8 _ _ _ _ _ _,Op_Reg (Reg8 RCX HalfL)]) = False
isAmbiguousSizeInstr (Instruction _ (Operation "shl") [_,Op_Reg (Reg8 RCX HalfL)]) = True
isAmbiguousSizeInstr (Instruction _ (Operation "shr") [Op_Mem 8 _ _ _ _ _ _,Op_Reg (Reg8 RCX HalfL)]) = False
isAmbiguousSizeInstr (Instruction _ (Operation "shr") [_,Op_Reg (Reg8 RCX HalfL)]) = True
isAmbiguousSizeInstr (Instruction _ (Operation "sar") [Op_Mem 32 _ _ _ _ _ _,Op_Reg (Reg8 RCX HalfL)]) = True
isAmbiguousSizeInstr (Instruction _ (Operation "sar") [Op_Mem 16 _ _ _ _ _ _,Op_Reg (Reg8 RCX HalfL)]) = True
isAmbiguousSizeInstr (Instruction _ (Operation "sar") [Op_Mem 8 _ _ _ _ _ _,Op_Reg (Reg8 RCX HalfL)]) = True
isAmbiguousSizeInstr (Instruction _ (Operation "sar") [_,Op_Reg (Reg8 RCX HalfL)]) = True
isAmbiguousSizeInstr (Instruction _ (Operation "int") _) = False
isAmbiguousSizeInstr (Instruction _ (Operation "ret") _) = False
isAmbiguousSizeInstr (Instruction _ (Operation "retf") _) = False
isAmbiguousSizeInstr (Instruction _ (Operation "jo") _) = False
isAmbiguousSizeInstr (Instruction _ (Operation "jno") _) = False
isAmbiguousSizeInstr (Instruction _ (Operation "jb") _) = False
isAmbiguousSizeInstr (Instruction _ (Operation "jae") _) = False
isAmbiguousSizeInstr (Instruction _ (Operation "jz") _) = False
isAmbiguousSizeInstr (Instruction _ (Operation "jnz") _) = False
isAmbiguousSizeInstr (Instruction _ (Operation "jbe") _) = False
isAmbiguousSizeInstr (Instruction _ (Operation "ja") _) = False
isAmbiguousSizeInstr (Instruction _ (Operation "js") _) = False
isAmbiguousSizeInstr (Instruction _ (Operation "jns") _) = False
isAmbiguousSizeInstr (Instruction _ (Operation "jp") _) = False
isAmbiguousSizeInstr (Instruction _ (Operation "jnp") _) = False
isAmbiguousSizeInstr (Instruction _ (Operation "jl") _) = False
isAmbiguousSizeInstr (Instruction _ (Operation "jge") _) = False
isAmbiguousSizeInstr (Instruction _ (Operation "jle") _) = False
isAmbiguousSizeInstr (Instruction _ (Operation "jg") _) = False
isAmbiguousSizeInstr (Instruction _ (Operation "enter") _) = False
isAmbiguousSizeInstr (Instruction _ (Operation "loop") _) = False
isAmbiguousSizeInstr (Instruction _ (Operation "loope") _) = False
isAmbiguousSizeInstr (Instruction _ (Operation "loopnz") _) = False
isAmbiguousSizeInstr (Instruction _ (Operation "jecxz") _) = False
isAmbiguousSizeInstr (Instruction _ (Operation "fbld") _ ) = False
isAmbiguousSizeInstr (Instruction _ (Operation "fldenv") _) = False
isAmbiguousSizeInstr (Instruction _ (Operation "sldt") _ ) = False
isAmbiguousSizeInstr (Instruction _ (Operation "frstor") _) = False
isAmbiguousSizeInstr (Instruction _ (Operation "fnsave") _) = False
isAmbiguousSizeInstr (Instruction _ _ _) = True

noByte (Operation "push") = True
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
operandtext (Op_Near o)                        = (operandtext o)
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
                         Immediate isz v | asz == isz || v > 0 -> ("0x" ++ (showHex (unsigned asz v)) "")
                                         |               v < 0 -> ("-0x" ++ (showHex (negate v)) "")
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
--operandtext o = "!operand "++ (show o) ++ "!"

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
opertext (Operation s) = s

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
