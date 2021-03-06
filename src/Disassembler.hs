module Disassembler
    (
          disassemble
        , Instruction(..)
        , Operation(..)
        , Operand(..)
        , Register(..)
        , GPR(..)
        , RegHalf(..)
        , Immediate(..)
    ) where

import Disassembler.Types

import Control.Monad (join)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Data.ByteString.Lazy (ByteString)
import Data.Word (Word64, Word32, Word16, Word8)
import Data.Int (Int64)
import Data.Bits
import Data.List (find)
import Data.Maybe (isJust, isNothing, fromJust, listToMaybe, mapMaybe, catMaybes, fromMaybe, maybeToList)
import Control.Applicative ( (<|>) )
import Data.Attoparsec.ByteString.Lazy hiding (take)

import qualified Data.Map.Lazy as Map
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import qualified Data.Binary.Get as G
import Data.Attoparsec.ByteString.Lazy as A


disassemble :: Word64 -> ByteString -> ([Instruction], ByteString)
disassemble ofs s = case (parse (disassemble1 ofs) s) of
                    Fail _ _ _   -> ([], s)
                    Done r (b,i) -> let l = fromIntegral (BS.length b)
                                        (i',r') = disassemble (ofs+l) r
                                      in (i:i', r')

disassemble1 :: Word64 -> Parser (BS.ByteString, Instruction)
disassemble1 ofs = let st = dsInitial ofs
                       res = evalStateT disassemble1' st
                     in match res

type Disassembler a = StateT DisassemblerState Parser a

data DisassemblerState = DisassemblerState {
      dsOffset :: Word64
    , dsLock :: Bool
    , dsRepNE :: Bool
    , dsRep :: Bool
    , dsSegOverride :: Maybe SReg
    , dsRex :: Maybe Word8
    , dsOpWidthOverride :: Bool
    , dsAdWidthOverride :: Bool
    , dsOpWidth :: Int
    , dsAdWidth :: Int
    , dsModRM :: Maybe ModRM
    , dsImmed :: Maybe Operand
    , dsMoffset :: Maybe Operand
}

dsInitial ofs = DisassemblerState { dsOffset = ofs,
                                    dsLock = False, dsRepNE = False, dsRep = False,
                                    dsSegOverride = Nothing,
                                    dsRex = Nothing,
                                    dsOpWidthOverride = False,
                                    dsAdWidthOverride = False,
                                    dsOpWidth = 32, dsAdWidth = 64,
                                    dsModRM = Nothing, dsImmed = Nothing, dsMoffset = Nothing
                                  }

adv :: Int -> Disassembler ()
adv n = modify (\x -> x { dsOffset = (dsOffset x) + (fromIntegral n) })

mask :: Word8 -> Word8 -> Disassembler Word8
mask m v = (lift $ satisfy (\x -> (x .&. m) == v)) <* adv 1

opcode :: Word8 -> Disassembler Word8
opcode v = (lift $ word8 v) <* adv 1

disassemble1' :: Disassembler Instruction
disassemble1' = choice [
--            option () (many' segover >> pure ()) >> option () (mask 0xf1 0x40 >> pure()) >> opcode 0x90 >> instr "nop" []
            general
          ]

general = do many' (choice [lockrep, segover, opsize, adsize])
             option () rex
             baseOpcode

lockrep :: Disassembler ()
lockrep = choice [ opcode 0xf0 >> modify (\x -> x { dsLock = True }),
                   opcode 0xf2 >> modify (\x -> x { dsRepNE = True }),
                   opcode 0xf3 >> modify (\x -> x { dsRep = True }) ]

segover :: Disassembler ()
segover = choice [
                    opcode 0x2e >> modify (\x -> x { dsSegOverride = Just CS }),
                    opcode 0x36 >> modify (\x -> x { dsSegOverride = Just SS }),
                    opcode 0x3e >> modify (\x -> x { dsSegOverride = Just DS }),
                    opcode 0x26 >> modify (\x -> x { dsSegOverride = Just ES }),
                    opcode 0x64 >> modify (\x -> x { dsSegOverride = Just FS }),
                    opcode 0x65 >> modify (\x -> x { dsSegOverride = Just GS })
                  ]

opsize :: Disassembler ()
opsize = opcode 0x66 >> modify ( \x -> x { dsOpWidthOverride = True } )

adsize :: Disassembler ()
adsize = opcode 0x67 >> modify ( \x -> x { dsAdWidthOverride = True } )

rex :: Disassembler ()
rex = do o <- mask 0xf0 0x40
         modify (\st -> st { dsRex = Just o })
         pure ()

baseOpcode :: Disassembler Instruction
baseOpcode = choice [
		  fail "dunsel"
-- 0x00
                , do i <- mask 0xc7 0x00; opWidthB; modrm; instr (ext1A' i) [modrm_rm, modrm_reg]
                , do i <- mask 0xc7 0x01; opWidthW; modrm; instr (ext1A' i) [modrm_rm, modrm_reg]
                , do i <- mask 0xc7 0x02; opWidthB; modrm; instr (ext1A' i) [modrm_reg, modrm_rm]
                , do i <- mask 0xc7 0x03; opWidthW; modrm; instr (ext1A' i) [modrm_reg, modrm_rm]
                , do i <- mask 0xc7 0x04; opWidthB;   imm; instr (ext1A' i) [accum, immed]
                , do i <- mask 0xc7 0x05; opWidthW;   imm; instr (ext1A' i) [accum, immed]
-- 0x40 (REX prefixes handled above)
-- 0x50
                , do r <- mask 0xf8 0x50; opWidthX' 64 16; instr "push" [reg (r .&. 0x07) id]
                , do r <- mask 0xf8 0x58; opWidthX' 64 16; instr "pop" [reg (r .&. 0x07) id]
-- 0x60
                , opcode 0x63 >> opWidthW >> modrm >> instr "movsxd" [modrm_reg, opWidthF 32 >> modrm_rm]
                , opcode 0x68 >> opWidthX' 32 16 >> imm >> instr "push" [immed]
                , opcode 0x69 >> modrm >> opWidthW >> imm >> instr "imul" [modrm_reg, modrm_rm, immed]
                , opcode 0x6a >> opWidthB >> imm >> instr "push" [immed]
                , opcode 0x6b >> opWidthW >> modrm >> immB >> instr "imul" [modrm_reg, modrm_rm, immed]
                , opcode 0x6c >> instr "insb" []
                , opcode 0x6d >> forkX (fail "invalid") (instr "insd" []) (instr "insw" [])
                , opcode 0x6e >> instr "outsb" []
                , opcode 0x6f >> forkX (instr "outsq" []) (instr "outsd" []) (instr "outsw" [])
-- 0x70
                , do i <- mask 0xf0 0x70; d <- displ; instr (shortjmp i) [pure d]
-- 0x80
                , do opcode 0x80; opWidthB; modrm; i <- modopcode; imm; instr (ext1A i) [modrm_rm, immed]
                , do opcode 0x81; opWidthW; modrm; i <- modopcode; imm; instr (ext1A i) [modrm_rm, immed]
                , do opcode 0x83; opWidthW; modrm; i <- modopcode; immB; instr (ext1A i) [modrm_rm, immed]
                , opcode 0x84 >> opWidthB >> modrm >> instr "test" [modrm_rm, modrm_reg]
                , opcode 0x85 >> opWidthW >> modrm >> instr "test" [modrm_rm, modrm_reg]
                , opcode 0x86 >> opWidthB >> modrm >> instr "xchg" [modrm_rm, modrm_reg]
                , opcode 0x87 >> opWidthW >> modrm >> instr "xchg" [modrm_rm, modrm_reg]
                , opcode 0x88 >> opWidthB >> modrm >> instr "mov" [modrm_rm, modrm_reg]
                , opcode 0x89 >> opWidthW >> modrm >> instr "mov" [modrm_rm, modrm_reg]
                , opcode 0x8a >> opWidthB >> modrm >> instr "mov" [modrm_reg, modrm_rm]
                , opcode 0x8b >> opWidthW >> modrm >> instr "mov" [modrm_reg, modrm_rm]
                , opcode 0x8c >> opWidthX 32 32 16 >> modrm >> instr "mov" [modrm_rm, modrm_sreg] -- intel says otherwise
                , opcode 0x8e >> opWidthX 32 32 16 >> modrm >> instr "mov" [modrm_sreg, modrm_rm] -- intel says otherwise
                , opcode 0x8d >> opWidthW >> modrm >> opmodnot3 >> instr "lea" [modrm_reg, modrm_rm]
                , opcode 0x8f >> modrm >> opcodeMatch 0 >> opWidthF 64 >> instr "pop" [modrm_rm]
-- 0x90
                , opcode 0x90 >> nop
                , do r <- mask 0xf8 0x90; opWidthW; instr "xchg" [reg (r .&. 0x07) id, accum]
                , opcode 0x98 >> forkX (instr "cdqe" []) (instr "cwde" []) (instr "cbw" [])
                , opcode 0x99 >> forkX (instr "cqo" [])  (instr "cdq" [])  (instr "cwd" [])
                , opcode 0x9b >> instr "wait" []
                , opcode 0x9c >> forkX (instr "pushfq" []) (instr "pushfq" []) (instr "pushfq" []) -- intel says otherwise
                , opcode 0x9d >> forkX (instr "popfq" []) (instr "popfq" []) (instr "popf" [])
                , opcode 0x9e >> instr "sahf" []
                , opcode 0x9f >> instr "lahf" []
-- 0xa0
                , opcode 0xa0 >> opWidthB >> moffs >> instr "mov" [accum, moffset]
                , opcode 0xa1 >> opWidthW >> moffs >> instr "mov" [accum, moffset]
                , opcode 0xa2 >> opWidthB >> moffs >> instr "mov" [moffset, accum]
                , opcode 0xa3 >> opWidthW >> moffs >> instr "mov" [moffset, accum]
                , opcode 0xa4 >> instr "movsb" []
                , opcode 0xa5 >> forkX (instr "movsq" []) (instr "movsd" []) (instr "movsw" [])
                , opcode 0xa6 >> instr "cmpsb" []
                , opcode 0xa7 >> forkX (instr "cmpsq" []) (instr "cmpsd" []) (instr "cmpsw" [])
                , opcode 0xa8 >> opWidthB >> imm >> instr "test" [accum, immed]
                , opcode 0xa9 >> opWidthW >> imm >> instr "test" [accum, immed]
                , opcode 0xaa >> instr "stosb" []
                , opcode 0xab >> forkX (instr "stosq" []) (instr "stosd" []) (instr "stosw" [])
                , opcode 0xac >> instr "lodsb" []
                , opcode 0xad >> forkX (instr "lodsq" []) (instr "lodsd" []) (instr "lodsw" [])
                , opcode 0xae >> instr "scasb" []
                , opcode 0xaf >> forkX (instr "scasq" []) (instr "scasd" []) (instr "scasw" [])
-- 0xb0
                , do r <- mask 0xf8 0xb0; opWidthB; immL; instr "mov" [reg (r .&. 0x07) id, immed]
                , do r <- mask 0xf8 0xb8; opWidthW; immL; instr "mov" [reg (r .&. 0x07) id, immed]
-- 0xc0
                , do opcode 0xc0; opWidthB; modrm; i <- ext2A; immB; instr i [modrm_rm, immed]
                , do opcode 0xc1; opWidthW; modrm; i <- ext2A; immB; instr i [modrm_rm, immed]
                , opcode 0xc2 >> immW >> instr "ret" [immed]
                , opcode 0xc3 >> instr "ret" []
                , opcode 0xc6 >> opcode 0xf8 >> immB >> instr "xabort" [immed]
                , opcode 0xc6 >> modrm >> opcodeMatch 0 >> opWidthB >> imm >> instr "mov" [modrm_rm, immed]
                , opcode 0xc7 >> opcode 0xf8 >> immB >> instr "xflush" [immed]
                , opcode 0xc7 >> modrm >> opcodeMatch 0 >> opWidthW >> imm >> instr "mov" [modrm_rm, immed]
                , do opcode 0xc8; opWidthF 16; imm; i <- imm''; instr "enter" [immed, pure $ Op_Imm $ Immediate 8 i]
                , opcode 0xc9 >> instr "leave" []
                , opcode 0xca >> opWidthF 16 >> imm >> instr "retf" [immed]
                , opcode 0xcb >> instr "retf" []
                , opcode 0xcc >> instr "int3" []
                , opcode 0xcd >> opWidthB >> imm >> instr "int" [immed]
                , opcode 0xcf >> forkX (instr "iretq" []) (instr "iretd" []) (instr "iretw" [])
-- 0xd0
                , do opcode 0xd0; opWidthB; modrm; i <- ext2A; instr i [modrm_rm, pure (Op_Const 1)]
                , do opcode 0xd1; opWidthW; modrm; i <- ext2A; instr i [modrm_rm, pure (Op_Const 1)]
                , do opcode 0xd2; opWidthB; modrm; i <- ext2A; instr i [modrm_rm, pure (Op_Reg (Reg8 RCX HalfL))]
                , do opcode 0xd3; opWidthW; modrm; i <- ext2A; instr i [modrm_rm, pure (Op_Reg (Reg8 RCX HalfL))]
                , opcode 0xd7 >> instr "xlatb" []
-- fpu: 0xd8
                , fpu
-- 0xe0
                , do opcode 0xe0; d <- displ; instr "loopnz" [pure d]
                , do opcode 0xe1; d <- displ; instr "loope" [pure d]
                , do opcode 0xe2; d <- displ; instr "loop" [pure d]
                , do opcode 0xe3; d <- displ; forkA (instr "jrcxz" [pure d]) (instr "jecxz" [pure d])
                , opcode 0xe4 >> opWidthB >> immB >> instr "in" [accum, immed]
                , opcode 0xe5 >> opWidthX' 32 16 >> immB >> instr "in" [accum, immed]
                , opcode 0xe6 >> opWidthB >> immB >> instr "out" [immed, accum]
                , opcode 0xe7 >> opWidthX' 32 16 >> immB >> instr "out" [immed, accum]
                , do opcode 0xe8; d <- forkX' displD displW; instr "call" [pure d]
                , do opcode 0xe9; d <- forkX' displD displW; instr "jmp" [pure d]
                , do opcode 0xeb; d <- displ; instr "jmp" [pure d]
                , opcode 0xec >> opWidthB >> instr "in" [accum, pure (Op_Reg (Reg16 RDX))]
                , opcode 0xed >> opWidthX' 32 16 >> instr "in" [accum, pure (Op_Reg (Reg16 RDX))]
                , opcode 0xee >> opWidthB >> instr "out" [pure (Op_Reg (Reg16 RDX)), accum]
                , opcode 0xef >> opWidthX' 32 16 >> instr "out" [pure (Op_Reg (Reg16 RDX)), accum]
-- 0xf0
                , opcode 0xf1 >> instr "int1" [] -- not in intel spec
                , opcode 0xf4 >> instr "hlt" []
                , opcode 0xf5 >> instr "cmc" []
                , opcode 0xf6 >> modrm >> opWidthB >> opcodeMatch 0 >> imm >> instr "test" [modrm_rm, immed]
                , opcode 0xf6 >> modrm >> opWidthB >> opcodeMatch 1 >> imm >> instr "test" [modrm_rm, immed] -- not intel spec
                , opcode 0xf6 >> modrm >> opWidthB >> opcodeMatch 2 >> instr "not" [modrm_rm]
                , opcode 0xf6 >> modrm >> opWidthB >> opcodeMatch 3 >> instr "neg" [modrm_rm]
                , opcode 0xf6 >> modrm >> opWidthB >> opcodeMatch 4 >> instr "mul" [modrm_rm]
                , opcode 0xf6 >> modrm >> opWidthB >> opcodeMatch 5 >> instr "imul" [modrm_rm]
                , opcode 0xf6 >> modrm >> opWidthB >> opcodeMatch 6 >> instr "div" [modrm_rm]
                , opcode 0xf6 >> modrm >> opWidthB >> opcodeMatch 7 >> instr "idiv" [modrm_rm]
                , opcode 0xf7 >> modrm >> opWidthW >> opcodeMatch 0 >> imm >> instr "test" [modrm_rm, immed]
                , opcode 0xf7 >> modrm >> opWidthW >> opcodeMatch 1 >> imm >> instr "test" [modrm_rm, immed] -- not intel spec
                , opcode 0xf7 >> modrm >> opWidthW >> opcodeMatch 2 >> instr "not" [modrm_rm]
                , opcode 0xf7 >> modrm >> opWidthW >> opcodeMatch 3 >> instr "neg" [modrm_rm]
                , opcode 0xf7 >> modrm >> opWidthW >> opcodeMatch 4 >> instr "mul" [modrm_rm]
                , opcode 0xf7 >> modrm >> opWidthW >> opcodeMatch 5 >> instr "imul" [modrm_rm]
                , opcode 0xf7 >> modrm >> opWidthW >> opcodeMatch 6 >> instr "div" [modrm_rm]
                , opcode 0xf7 >> modrm >> opWidthW >> opcodeMatch 7 >> instr "idiv" [modrm_rm]
                , opcode 0xf8 >> instr "clc" []
                , opcode 0xf9 >> instr "stc" []
                , opcode 0xfa >> instr "cli" []
                , opcode 0xfb >> instr "sti" []
                , opcode 0xfc >> instr "cld" []
                , opcode 0xfd >> instr "std" []
                , opcode 0xfe >> modrm >> opWidthB >> opcodeMatch 0 >> instr "inc" [modrm_rm]
                , opcode 0xfe >> modrm >> opWidthB >> opcodeMatch 1 >> instr "dec" [modrm_rm]
                , opcode 0xff >> modrm >> opWidthW >> opcodeMatch 0 >> instr "inc" [modrm_rm]
                , opcode 0xff >> modrm >> opWidthW >> opcodeMatch 1 >> instr "dec" [modrm_rm]
                , opcode 0xff >> modrm >> opWidthX' 32 16 >> opcodeMatch 2 >> instr "call" [Op_Near <$> modrm_rm]
                , opcode 0xff >> modrm >> opWidthX' 32 16 >> opcodeMatch 3 >> instr "call" [Op_Far <$> modrm_rm]
                , opcode 0xff >> modrm >> opWidthF 32 >> opcodeMatch 4 >> instr "jmp" [Op_Near <$> modrm_rm]
                , opcode 0xff >> modrm >> opWidthF 32 >> opcodeMatch 5 >> instr "jmp" [Op_Far <$> modrm_rm]
                , opcode 0xff >> modrm >> opWidthF 64 >> opcodeMatch 6 >> instr "push" [modrm_rm]

                , opcode 0x0f >> opcode 0x00 >> modrm >> opcodeMatch 0 >> instr "sldt" [modrm_rm]
                , opcode 0x0f >> opcode 0x00 >> modrm >> opcodeMatch 1 >> instr "str" [modrm_rm]
                , opcode 0x0f >> opcode 0x00 >> modrm >> opcodeMatch 2 >> instr "lldt" [modrm_rm]
                , opcode 0x0f >> opcode 0x00 >> modrm >> opcodeMatch 3 >> instr "ltr" [modrm_rm]
                , opcode 0x0f >> opcode 0x00 >> modrm >> opcodeMatch 4 >> instr "verr" [modrm_rm]
                , opcode 0x0f >> opcode 0x00 >> modrm >> opcodeMatch 5 >> instr "verw" [modrm_rm]

                , opcode 0x0f >> opcode 0x01 >> opcode 0xc1 >> instr "vmcall" []
                , opcode 0x0f >> opcode 0x01 >> opcode 0xc2 >> instr "vmlaunch" []
                , opcode 0x0f >> opcode 0x01 >> opcode 0xc3 >> instr "vmresume" []
                , opcode 0x0f >> opcode 0x01 >> opcode 0xc4 >> instr "vmxoff" []
                , opcode 0x0f >> opcode 0x01 >> opcode 0xc8 >> instr "monitor" []
                , opcode 0x0f >> opcode 0x01 >> opcode 0xc9 >> instr "mwait" []
                , opcode 0x0f >> opcode 0x01 >> opcode 0xca >> instr "clac" []
                , opcode 0x0f >> opcode 0x01 >> opcode 0xcb >> instr "stac" []
                , opcode 0x0f >> opcode 0x01 >> opcode 0xcf >> instr "encls" []
                , opcode 0x0f >> opcode 0x01 >> opcode 0xd0 >> instr "xgetbv" []
                , opcode 0x0f >> opcode 0x01 >> opcode 0xd1 >> instr "xsetbv" []
                , opcode 0x0f >> opcode 0x01 >> opcode 0xd4 >> instr "vmfunc" []
                , opcode 0x0f >> opcode 0x01 >> opcode 0xd5 >> instr "xend" []
                , opcode 0x0f >> opcode 0x01 >> opcode 0xd6 >> instr "xtest" []
                , opcode 0x0f >> opcode 0x01 >> opcode 0xd7 >> instr "enclu" []


     ]
fpu = choice [ fail "dunsel"
            , do opcode 0xd8; r <- (lift $ satisfy (>=0xc0)); adv 1; instr (fpuD8 (bits 3 3 r)) [pure (Op_Reg (RegFPU ST0)), freg (r .&. 7)]
            , do opcode 0xd8; modrm; opmodnot3; i <- modopcode; instr (fpuD8 i) [modrm_rm]

            , do opcode 0xd9; r <- mask 0xf8 0xc0; instr "fld" [pure (Op_Reg (RegFPU ST0)), freg (r .&. 7)]
            , do opcode 0xd9; r <- mask 0xf8 0xc8; instr "fxch" [pure (Op_Reg (RegFPU ST0)), freg (r .&. 7)]
            , opcode 0xd9 >> opcode 0xd0 >> instr "fnop" []
            , opcode 0xd9 >> opcode 0xe0 >> instr "fchs" []
            , opcode 0xd9 >> opcode 0xe1 >> instr "fabs" []
            , opcode 0xd9 >> opcode 0xe4 >> instr "ftst" []
            , opcode 0xd9 >> opcode 0xe5 >> instr "fxam" []
            , opcode 0xd9 >> opcode 0xf0 >> instr "f2xm1" []
            , opcode 0xd9 >> opcode 0xf1 >> instr "fyl2x" []
            , opcode 0xd9 >> opcode 0xf2 >> instr "fptan" []
            , opcode 0xd9 >> opcode 0xf3 >> instr "fpatan" []
            , opcode 0xd9 >> opcode 0xf4 >> instr "fxtract" []
            , opcode 0xd9 >> opcode 0xf5 >> instr "fprem1" []
            , opcode 0xd9 >> opcode 0xf6 >> instr "fdecstp" []
            , opcode 0xd9 >> opcode 0xf7 >> instr "fincstp" []
            , opcode 0xd9 >> opcode 0xe8 >> instr "fld1" []
            , opcode 0xd9 >> opcode 0xe9 >> instr "fldl2t" []
            , opcode 0xd9 >> opcode 0xea >> instr "fldl2e" []
            , opcode 0xd9 >> opcode 0xeb >> instr "fldpi" []
            , opcode 0xd9 >> opcode 0xec >> instr "fldlg2" []
            , opcode 0xd9 >> opcode 0xed >> instr "fldln2" []
            , opcode 0xd9 >> opcode 0xee >> instr "fldz" []
            , opcode 0xd9 >> opcode 0xf8 >> instr "fprem" []
            , opcode 0xd9 >> opcode 0xf9 >> instr "fyl2xp1" []
            , opcode 0xd9 >> opcode 0xfa >> instr "fsqrt" []
            , opcode 0xd9 >> opcode 0xfb >> instr "fsincos" []
            , opcode 0xd9 >> opcode 0xfc >> instr "frndint" []
            , opcode 0xd9 >> opcode 0xfd >> instr "fscale" []
            , opcode 0xd9 >> opcode 0xfe >> instr "fsin" []
            , opcode 0xd9 >> opcode 0xff >> instr "fcos" []
            , opcode 0xd9 >> modrm >> opmodnot3 >> opcodeMatch 0 >> opWidthF 32 >> instr "fld" [modrm_rm]
            , opcode 0xd9 >> modrm >> opmodnot3 >> opcodeMatch 2 >> opWidthF 32 >> instr "fst" [modrm_rm]
            , opcode 0xd9 >> modrm >> opmodnot3 >> opcodeMatch 3 >> opWidthF 32 >> instr "fstp" [modrm_rm]
            , opcode 0xd9 >> modrm >> opmodnot3 >> opcodeMatch 4 >> opWidthF 32 >> instr "fldenv" [modrm_rm]
            , opcode 0xd9 >> modrm >> opmodnot3 >> opcodeMatch 5 >> opWidthF 16 >> instr "fldcw" [modrm_rm]
            , opcode 0xd9 >> modrm >> opmodnot3 >> opcodeMatch 6 >> opWidthF 32 >> instr "fnstenv" [modrm_rm]
            , opcode 0xd9 >> modrm >> opmodnot3 >> opcodeMatch 7 >> opWidthF 32 >> instr "fnstcw" [modrm_rm]

            , do opcode 0xda; r <- mask 0xf8 0xc0; instr "fcmovb" [pure (Op_Reg (RegFPU ST0)), freg (r .&. 7)]
            , do opcode 0xda; r <- mask 0xf8 0xc8; instr "fcmove" [pure (Op_Reg (RegFPU ST0)), freg (r .&. 7)]
            , do opcode 0xda; r <- mask 0xf8 0xd0; instr "fcmovbe" [pure (Op_Reg (RegFPU ST0)), freg (r .&. 7)]
            , do opcode 0xda; r <- mask 0xf8 0xd8; instr "fcmovu" [pure (Op_Reg (RegFPU ST0)), freg (r .&. 7)]
            , opcode 0xda >> opcode 0xe9 >> instr "fucompp" []
            , opcode 0xda >> modrm >> opmodnot3 >> opcodeMatch 0 >> opWidthF 32 >> instr "fiadd" [modrm_rm]
            , opcode 0xda >> modrm >> opmodnot3 >> opcodeMatch 1 >> opWidthF 32 >> instr "fimul" [modrm_rm]
            , opcode 0xda >> modrm >> opmodnot3 >> opcodeMatch 2 >> opWidthF 32 >> instr "ficom" [modrm_rm]
            , opcode 0xda >> modrm >> opmodnot3 >> opcodeMatch 3 >> opWidthF 32 >> instr "ficomp" [modrm_rm]
            , opcode 0xda >> modrm >> opmodnot3 >> opcodeMatch 4 >> opWidthF 32 >> instr "fisub" [modrm_rm]
            , opcode 0xda >> modrm >> opmodnot3 >> opcodeMatch 5 >> opWidthF 32 >> instr "fisubr" [modrm_rm]
            , opcode 0xda >> modrm >> opmodnot3 >> opcodeMatch 6 >> opWidthF 32 >> instr "fidiv" [modrm_rm]
            , opcode 0xda >> modrm >> opmodnot3 >> opcodeMatch 7 >> opWidthF 32 >> instr "fidivr" [modrm_rm]

            , do opcode 0xdb; r <- mask 0xf8 0xc0; instr "fcmovnb" [pure (Op_Reg (RegFPU ST0)), freg (r .&. 7)]
            , do opcode 0xdb; r <- mask 0xf8 0xc8; instr "fcmovne" [pure (Op_Reg (RegFPU ST0)), freg (r .&. 7)]
            , do opcode 0xdb; r <- mask 0xf8 0xd0; instr "fcmovnbe" [pure (Op_Reg (RegFPU ST0)), freg (r .&. 7)]
            , do opcode 0xdb; r <- mask 0xf8 0xd8; instr "fcmovnu" [pure (Op_Reg (RegFPU ST0)), freg (r .&. 7)]
            , do opcode 0xdb; r <- mask 0xf8 0xe8; instr "fucomi" [pure (Op_Reg (RegFPU ST0)), freg (r .&. 7)]
            , do opcode 0xdb; r <- mask 0xf8 0xf0; instr "fcomi" [pure (Op_Reg (RegFPU ST0)), freg (r .&. 7)]
            , opcode 0xdb >> opcode 0xe2 >> instr "fclex" []
            , opcode 0xdb >> opcode 0xe3 >> instr "finit" []
            , opcode 0xdb >> modrm >> opmodnot3 >> opcodeMatch 0 >> opWidthF 32 >> instr "fild" [modrm_rm]
            , opcode 0xdb >> modrm >> opmodnot3 >> opcodeMatch 1 >> opWidthF 32 >> instr "fisttp" [modrm_rm]
            , opcode 0xdb >> modrm >> opmodnot3 >> opcodeMatch 2 >> opWidthF 32 >> instr "fist" [modrm_rm]
            , opcode 0xdb >> modrm >> opmodnot3 >> opcodeMatch 3 >> opWidthF 32 >> instr "fistp" [modrm_rm]
            , opcode 0xdb >> modrm >> opmodnot3 >> opcodeMatch 5 >> opWidthF 80 >> instr "fld" [modrm_rm]
            , opcode 0xdb >> modrm >> opmodnot3 >> opcodeMatch 7 >> opWidthF 80 >> instr "fstp" [modrm_rm]

            , do opcode 0xdc; r <- mask 0xf8 0xc0; instr "fadd" [freg (r .&. 7), pure (Op_Reg (RegFPU ST0))]
            , do opcode 0xdc; r <- mask 0xf8 0xc8; instr "fmul" [freg (r .&. 7), pure (Op_Reg (RegFPU ST0))]
            , do opcode 0xdc; r <- mask 0xf8 0xd0; instr "fcom2" [freg (r .&. 7), pure (Op_Reg (RegFPU ST0))] -- not in intel spec
            , do opcode 0xdc; r <- mask 0xf8 0xe0; instr "fsubr" [freg (r .&. 7), pure (Op_Reg (RegFPU ST0))]
            , do opcode 0xdc; r <- mask 0xf8 0xe8; instr "fsub" [freg (r .&. 7), pure (Op_Reg (RegFPU ST0))]
            , do opcode 0xdc; r <- mask 0xf8 0xf0; instr "fdivr" [freg (r .&. 7), pure (Op_Reg (RegFPU ST0))]
            , do opcode 0xdc; r <- mask 0xf8 0xf8; instr "fdiv" [freg (r .&. 7), pure (Op_Reg (RegFPU ST0))]
            , do opcode 0xdc; modrm; opmodnot3; i <- modopcode; opWidthF 64 >> instr (fpuD8 i) [modrm_rm]

            , do opcode 0xdd; r <- mask 0xf8 0xc0; instr "ffree" [freg (r .&. 7)]
            , do opcode 0xdd; r <- mask 0xf8 0xd0; instr "fst" [freg (r .&. 7)]
            , do opcode 0xdd; r <- mask 0xf8 0xd8; instr "fstp" [freg (r .&. 7)]
            , do opcode 0xdd; r <- mask 0xf8 0xe0; instr "fucom" [freg (r .&. 7), pure (Op_Reg (RegFPU ST0))]
            , do opcode 0xdd; r <- mask 0xf8 0xe8; instr "fucomp" [freg (r .&. 7)]
            , opcode 0xdd >> modrm >> opmodnot3 >> opcodeMatch 0 >> opWidthF 64 >> instr "fld" [modrm_rm]
            , opcode 0xdd >> modrm >> opmodnot3 >> opcodeMatch 1 >> opWidthF 64 >> instr "fisttp" [modrm_rm]
            , opcode 0xdd >> modrm >> opmodnot3 >> opcodeMatch 2 >> opWidthF 64 >> instr "fst" [modrm_rm]
            , opcode 0xdd >> modrm >> opmodnot3 >> opcodeMatch 3 >> opWidthF 64 >> instr "fstp" [modrm_rm]
            , opcode 0xdd >> modrm >> opmodnot3 >> opcodeMatch 4 >> opWidthF 64 >> instr "frstor" [modrm_rm]
            , opcode 0xdd >> modrm >> opmodnot3 >> opcodeMatch 6 >> opWidthF 64 >> instr "fnsave" [modrm_rm]
            , opcode 0xdd >> modrm >> opmodnot3 >> opcodeMatch 7 >> opWidthF 16 >> instr "fnstsw" [modrm_rm]

            , do opcode 0xde; r <- mask 0xf8 0xc0; instr "faddp" [freg (r .&. 7), pure (Op_Reg (RegFPU ST0))]
            , do opcode 0xde; r <- mask 0xf8 0xc8; instr "fmulp" [freg (r .&. 7), pure (Op_Reg (RegFPU ST0))]
            , do opcode 0xde; r <- mask 0xf8 0xe0; instr "fsubrp" [freg (r .&. 7), pure (Op_Reg (RegFPU ST0))]
            , do opcode 0xde; r <- mask 0xf8 0xe8; instr "fsubp" [freg (r .&. 7), pure (Op_Reg (RegFPU ST0))]
            , do opcode 0xde; r <- mask 0xf8 0xf0; instr "fdivrp" [freg (r .&. 7), pure (Op_Reg (RegFPU ST0))]
            , do opcode 0xde; r <- mask 0xf8 0xf8; instr "fdivp" [freg (r .&. 7), pure (Op_Reg (RegFPU ST0))]
            , opcode 0xde >> opcode 0xd9 >> instr "fcompp" []
            , opcode 0xde >> modrm >> opmodnot3 >> opcodeMatch 0 >> opWidthF 16 >> instr "fiadd" [modrm_rm]
            , opcode 0xde >> modrm >> opmodnot3 >> opcodeMatch 1 >> opWidthF 16 >> instr "fimul" [modrm_rm]
            , opcode 0xde >> modrm >> opmodnot3 >> opcodeMatch 2 >> opWidthF 16 >> instr "ficom" [modrm_rm]
            , opcode 0xde >> modrm >> opmodnot3 >> opcodeMatch 3 >> opWidthF 16 >> instr "ficomp" [modrm_rm]
            , opcode 0xde >> modrm >> opmodnot3 >> opcodeMatch 4 >> opWidthF 16 >> instr "fisub" [modrm_rm]
            , opcode 0xde >> modrm >> opmodnot3 >> opcodeMatch 5 >> opWidthF 16 >> instr "fisubr" [modrm_rm]
            , opcode 0xde >> modrm >> opmodnot3 >> opcodeMatch 6 >> opWidthF 16 >> instr "fidiv" [modrm_rm]
            , opcode 0xde >> modrm >> opmodnot3 >> opcodeMatch 7 >> opWidthF 16 >> instr "fidivr" [modrm_rm]

            , opcode 0xdf >> opcode 0xe0 >> instr "fnstsw" [pure (Op_Reg (Reg16 RAX))]
            , do opcode 0xdf; r <- mask 0xf8 0xe8; instr "fucomip" [pure (Op_Reg (RegFPU ST0)), freg (r .&. 7)]
            , do opcode 0xdf; r <- mask 0xf8 0xf0; instr "fcomip" [pure (Op_Reg (RegFPU ST0)), freg (r .&. 7)]
            , opcode 0xdf >> modrm >> opmodnot3 >> opcodeMatch 0 >> opWidthF 16 >> instr "fild" [modrm_rm]
            , opcode 0xdf >> modrm >> opmodnot3 >> opcodeMatch 1 >> opWidthF 16 >> instr "fisttp" [modrm_rm]
            , opcode 0xdf >> modrm >> opmodnot3 >> opcodeMatch 2 >> opWidthF 16 >> instr "fist" [modrm_rm]
            , opcode 0xdf >> modrm >> opmodnot3 >> opcodeMatch 3 >> opWidthF 16 >> instr "fistp" [modrm_rm]
            , opcode 0xdf >> modrm >> opmodnot3 >> opcodeMatch 4 >> opWidthF 16 >> instr "fbld" [modrm_rm]
            , opcode 0xdf >> modrm >> opmodnot3 >> opcodeMatch 5 >> opWidthF 16 >> instr "fild" [modrm_rm]
            , opcode 0xdf >> modrm >> opmodnot3 >> opcodeMatch 6 >> opWidthF 16 >> instr "fbstp" [modrm_rm]
            , opcode 0xdf >> modrm >> opmodnot3 >> opcodeMatch 7 >> opWidthF 16 >> instr "fistp" [modrm_rm]
            ]

nop = do b <- dsRexB
         rep <- (gets dsRep)
         if b == 1 then fail "not nop"
            else if rep then instr "pause" []
                else instr "nop" []

ext1A i = ["add", "or", "adc", "sbb", "and", "sub", "xor", "cmp"] !! (fromIntegral i)
ext1A' i = ext1A (fromIntegral (bits 3 3 i))

fpuD8 i = [ "fadd", "fmul", "fcom", "fcomp", "fsub", "fsubr", "fdiv", "fdivr"] !! (fromIntegral i)

shortjmp i = ["jo", "jno", "jb", "jae", "jz", "jnz", "jbe", "ja", "js", "jns", "jp", "jnp", "jl", "jge", "jle", "jg"]
                        !! (fromIntegral (bits 0 4 i))

ext2A = do i <- modopcode
           case i of 0 -> pure "rol"
                     1 -> pure "ror"
                     2 -> pure "rcl"
                     3 -> pure "rcr"
                     4 -> pure "shl"
                     5 -> pure "shr"
                     6 -> pure "shl" -- intel says 6 is invalid but reportedly it's also shl
                     7 -> pure "sar"

opWidthB = opWidthX 8 8 8
opWidthW = opWidthX 64 32 16
opWidthF n = opWidthX n n n

forkX q d w = do o16 <- dsO16
                 rexW <- dsRexW
                 case (o16, rexW) of (_, 1) -> q; (True, 0) -> w; (False, 0) -> d

forkX' d w = do o16 <- dsO16
                case (o16) of (True) -> w; (False) -> d

forkA q d  = do a32 <- dsA32
                case (a32) of (True) -> d; (False) -> q

opWidthX q d w = do o16 <- dsO16
                    rexW <- dsRexW
                    let ow = case (o16, rexW) of (_, 1) -> q; (True, 0) -> w; (False, 0) -> d
                      in modify (\x -> x { dsOpWidth = ow })

opWidthX' d w = do o16 <- dsO16
                   let ow = case (o16) of (True) -> w; (False) -> d
                     in modify (\x -> x { dsOpWidth = ow })

adWidth n1 n2 = dsA32 >>= (\f -> modify (\x -> x { dsAdWidth = if f then n1 else n2 } ) )

instr :: String -> [Disassembler Operand] -> Disassembler Instruction
instr i ops = Instruction <$> pfx <*> pure (Operation i) <*> sequence ops

readBytes :: Int -> Disassembler ByteString
readBytes n = (lift $ (B.fromStrict <$> A.take n)) <* adv n

modopcode :: Disassembler Word8
modopcode = (gets dsModRM) >>= pure . modRM_breg . fromJust

opcodeMatch c = do o <- modopcode; if o == c then pure () else fail "no match"
opmodmatch c = do o <- modRM_mod <$> fromJust <$> (gets dsModRM); if o == c then pure () else fail "no match"
opmodnot3 = do o <- modRM_mod <$> fromJust <$> (gets dsModRM); if o == 3 then fail "no match" else pure ()

imm :: Disassembler ()
imm = do
    ow <- gets dsOpWidth
    case ow of  8 ->  (readBytes 1) >>= pure . (\s -> fromIntegral (G.runGet G.getWord8 (s)))
                16 -> (readBytes 2) >>= pure . (\s -> fromIntegral (G.runGet G.getWord16le (s)))
                32 -> (readBytes 4) >>= pure . (\s -> fromIntegral (G.runGet G.getWord32le (s)))
                64 -> (readBytes 4) >>= pure . (\s -> fromIntegral (G.runGet G.getWord32le (s)))
      >>= \i -> modify (\x -> x { dsImmed = Just $ Op_Imm (Immediate ow (i)) } )

immL :: Disassembler ()
immL = do
    ow <- gets dsOpWidth
    case ow of  8 ->  (readBytes 1) >>= pure . (\s -> fromIntegral (G.runGet G.getWord8 (s)))
                16 -> (readBytes 2) >>= pure . (\s -> fromIntegral (G.runGet G.getWord16le (s)))
                32 -> (readBytes 4) >>= pure . (\s -> fromIntegral (G.runGet G.getWord32le (s)))
                64 -> (readBytes 8) >>= pure . (\s -> fromIntegral (G.runGet G.getWord64le (s)))
      >>= \i -> modify (\x -> x { dsImmed = Just $ Op_Imm (Immediate ow (i)) } )

imm'' :: Disassembler Word64
imm'' = fromIntegral <$> (lift anyWord8) <* adv 1

moffs :: Disassembler ()
moffs = do
    aw <- (\x -> if x then 32 :: Int else 64 :: Int) <$> dsA32
    ow <- gets dsOpWidth
    seg <- gets dsSegOverride
    case aw of 32 -> (readBytes 4) >>= pure . (\s -> fromIntegral (G.runGet G.getWord32le (s)))
               64 -> (readBytes 8) >>= pure . (\s -> fromIntegral (G.runGet G.getWord64le (s)))
      >>= \disp -> modify (\x -> x {
                dsMoffset = Just $ Op_Mem ow aw RegNone RegNone 0 (Immediate aw disp) seg
                })

moffset :: Disassembler Operand
moffset = (gets dsMoffset) >>= pure . fromJust


immB :: Disassembler ()
immB = imm' 1 G.getWord8

immW :: Disassembler ()
immW = imm' 2 G.getWord16le

imm' :: (Integral t) => Int -> G.Get t -> Disassembler ()
imm' b f = (readBytes b) >>= pure . (\s -> fromIntegral (G.runGet f (s)))
                         >>= \i -> modify (\x -> x { dsImmed = Just $ Op_Imm $ Immediate (8 * b) i } )

displ :: Disassembler Operand
displ = do  s <- (readBytes 1)
            disp <- pure (fromIntegral (G.runGet G.getInt8 (s)))
            eip <- gets dsOffset
            let iv = bits 0 64 (eip + disp)
                imm = Immediate 64 iv
              in pure (Op_Jmp imm)

displW :: Disassembler Operand
displW = do s <- (readBytes 2)
            disp <- pure (fromIntegral (G.runGet G.getInt16le (s)))
            eip <- gets dsOffset
            let iv = bits 0 64 (eip + disp)
                imm = Immediate 64 iv
              in pure (Op_Jmp imm)

displD :: Disassembler Operand
displD = do s <- (readBytes 4)
            disp <- pure (fromIntegral (G.runGet G.getInt32le (s)))
            eip <- gets dsOffset
            let iv = bits 0 64 (eip + disp)
                imm = Immediate 64 iv
              in pure (Op_Jmp imm)

pfx :: Disassembler [Prefix]
pfx = do st <- get
         let ff a b = if (a st) then [b] else []
             pfx = (ff dsLock PrefixLock)
                ++ (ff dsRepNE PrefixRepNE)
                ++ (ff dsRep PrefixRep)
                ++ (ff dsOpWidthOverride PrefixO16)
                ++ (ff dsAdWidthOverride PrefixA32)
                ++ (maybe [] ((:[]) . PrefixSeg) (dsSegOverride st))
                ++ (maybe [] ((:[]) . PrefixRex) (dsRex st))
            in pure pfx

modrm_rm :: Disassembler Operand
modrm_rm = do rm <- modRM_rm <$> fromJust <$> gets dsModRM
              ow <- gets dsOpWidth
              rexB <- dsRexB
              rex <- gets dsRex
              let sr = selectreg ow rexB rex
                in pure (rm ow sr)

modrm_reg :: Disassembler Operand
modrm_reg = do b'reg <- modRM_breg <$> fromJust <$> (gets dsModRM)
               opWidth <- gets dsOpWidth
               rexR <- dsRexR
               rex <- gets dsRex
               let reg = selectreg opWidth rexR rex b'reg
                 in pure $ Op_Reg reg

modrm_sreg :: Disassembler Operand
modrm_sreg = do b'reg <- modRM_breg <$> fromJust <$> (gets dsModRM)
                case b'reg of 0 -> pure (Op_Reg $ RegSeg ES)
                              1 -> pure (Op_Reg $ RegSeg CS)
                              2 -> pure (Op_Reg $ RegSeg SS)
                              3 -> pure (Op_Reg $ RegSeg DS)
                              4 -> pure (Op_Reg $ RegSeg FS)
                              5 -> pure (Op_Reg $ RegSeg GS)
                              6 -> pure (Op_Reg $ RegSeg SR6) -- not in Intel spec
                              7 -> pure (Op_Reg $ RegSeg SR7) -- not in Intel spec
                              _ -> fail "invalid"

immed :: Disassembler Operand
immed = (gets dsImmed) >>= pure . fromJust

reg :: Word8 -> (Int -> Int) -> Disassembler Operand
reg rr ov = do
    ow <- gets dsOpWidth
    rex <- gets dsRex
    rexB <- dsRexB
    let r = selectreg (ov ow) rexB rex rr
      in pure $ Op_Reg r

freg :: Word8 -> Disassembler Operand
freg rr = (pure . Op_Reg . RegFPU) ( [ST0, ST1, ST2, ST3, ST4, ST5, ST6, ST7] !! (fromIntegral rr) )

accum :: Disassembler Operand
accum = do
    ow <- gets dsOpWidth
    rex <- gets dsRex
    let r = selectreg ow 0 Nothing 0
      in pure $ Op_Reg r

-- type DisassemblerSingle = Word8 -> DisassemblerState -> Get Instruction
-- type OpcodeMap = Map.Map Word8 DisassemblerSingle
--
dsO16 :: Disassembler Bool
dsO16 = gets dsOpWidthOverride
dsA32 :: Disassembler Bool
dsA32 = gets dsAdWidthOverride

dsRexW :: Disassembler Word8
dsRexW = gets dsRex >>= \rex -> pure $ bits 3 1 (fromMaybe 0 rex)
dsRexR :: Disassembler Word8
dsRexR = gets dsRex >>= \rex -> pure $ bits 2 1 (fromMaybe 0 rex)
dsRexX :: Disassembler Word8
dsRexX = gets dsRex >>= \rex -> pure $ bits 1 1 (fromMaybe 0 rex)
dsRexB :: Disassembler Word8
dsRexB = gets dsRex >>= \rex -> pure $ bits 0 1 (fromMaybe 0 rex)

dsSeg :: Disassembler (Maybe SReg)
dsSeg = gets dsSegOverride

-- dsA32 ds = PrefixA32 `elem` (dsPfx ds)
-- dsRep ds = PrefixRep `elem` (dsPfx ds)
-- dsSeg ds = listToMaybe (catMaybes (map (\p -> case p of (PrefixSeg s) -> Just s; _ -> Nothing) (dsPfx ds)))

modrm :: Disassembler ()
modrm = do
    val      <- (lift $ anyWord8) <* adv 1
    aWidth   <- (\x -> if x then 32 :: Int else 64 :: Int) <$> dsA32
    rex      <- gets dsRex
    rexR     <- dsRexR
    rexX     <- dsRexX
    rexB     <- dsRexB
    opWidth  <- gets dsOpWidth
    so       <- dsSeg
    let b'mod = bits 6 2 val
        b'reg = bits 3 3 val
        b'rm  = bits 0 3 val
        hasSib = (b'mod /= 3 && b'rm == 4)
        dispSize = case (b'mod, b'rm) of
            (0,5) -> Just 32
            (1,_) -> Just 8
            (2,_) -> Just 32
            _     -> Nothing
      in do
        (sib,dispSize') <- if hasSib then (parseSib b'mod dispSize rex rexB rexX aWidth) <$> getWord8
                                     else return ((RegNone,RegNone,0),dispSize)
        disp <- case dispSize' of
                    Just 8 -> (Immediate 8 . fromIntegral) <$> getInt8
                    Just 32 -> (Immediate 32 . fromIntegral) <$> getInt32le
                    _  -> return $ Immediate 0 0
        let rm = case (b'mod, b'rm) of
                (3,_) -> \ow sr -> Op_Reg (sr b'rm)
                (0,5) -> \ow sr -> Op_Mem ow aWidth ((if aWidth == 64 then Reg64 else Reg32) RIP) RegNone 0 disp so
                (0,4) -> \ow sr -> let (br, ir, sc) = sib in Op_Mem ow aWidth br ir sc disp so
                (1,4) -> \ow sr -> let (br, ir, sc) = sib in Op_Mem ow aWidth br ir sc disp so
                (2,4) -> \ow sr -> let (br, ir, sc) = sib in Op_Mem ow aWidth br ir sc disp so
                (_,_) -> \ow sr -> Op_Mem ow aWidth (selectreg aWidth rexB rex b'rm ) RegNone 0 disp so
          in
            modify $ \x -> x { dsModRM = Just $ ModRM rm b'reg b'mod b'rm }
    where
        getWord8   = (lift $ anyWord8) <* adv 1
        getInt8    = (readBytes 1) >>= \s -> pure (G.runGet G.getInt8 (s))
        getInt32le = (readBytes 4) >>= \s -> pure (G.runGet G.getInt32le (s))

        parseSib m dispSize rex rexB rexX aw sib = let
                                 br = (bits 0 3 sib)
                                 ir = (bits 3 3 sib)
                                 ss = (bits 6 2 sib)
                                 sp = (case aw of 16 -> Reg16; 32 -> Reg32; 64 -> Reg64) RSP
                                 breg = selectreg aw rexB rex br
                                 ireg = selectreg aw rexX rex ir
                                 sf = case ss of { 0 -> 1; 1 -> 2; 2 -> 4; 3 -> 8 }
                            in case (m, br) of (0, 5) -> ((RegNone, if ireg == sp then RegNone else ireg, sf), Just 32)
                                               _      -> ((breg, if ireg == sp then RegNone else ireg, sf), dispSize)

selectreg :: Int -> Word8 -> Maybe Word8 -> Word8 -> Register
selectreg opWidth rex rex' reg = let
                rvec' = case rex of
                        1 -> [R8, R9, R10, R11, R12, R13, R14, R15]
                        0 -> [RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI]
                rvec = case (opWidth, rex') of
                        (8, Just _)  -> map (\i -> Reg8 i HalfL) rvec'
                        (8, Nothing) -> [Reg8 RAX HalfL, Reg8 RCX HalfL, Reg8 RDX HalfL, Reg8 RBX HalfL,
                                         Reg8 RAX HalfH, Reg8 RCX HalfH, Reg8 RDX HalfH, Reg8 RBX HalfH]
                        (16, _) -> map Reg16 rvec'
                        (32, _) -> map Reg32 rvec'
                        (64, _) -> map Reg64 rvec'
            in rvec !! (fromIntegral reg)

bits :: (Integral a, Bits a) => Int-> Int -> a -> a
bits s l i = fromIntegral $ (i `shiftR` s) .&. ((1 `shiftL` l) - 1)

bitTest :: Int -> Maybe Prefix -> Bool
bitTest i v = case v of
                Nothing -> False
                Just (PrefixRex n) -> n .&. (bit i) /= 0


data ModRM = ModRM {
        modRM_rm :: Int -> (Word8 -> Register) -> Operand
      , modRM_breg :: Word8
      , modRM_mod :: Word8
      , modRM_brm :: Word8
      }
