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
import Data.ByteString.Lazy (ByteString)
import Data.Word (Word64, Word32, Word16, Word8)
import Data.Int (Int64)
import Data.Binary.Get
import Data.Bits
import Data.List (find)
import Data.Maybe (isJust, isNothing, fromJust, listToMaybe, mapMaybe, catMaybes)
import Control.Applicative ( (<|>) )

import qualified Data.Map.Lazy as Map
import qualified Data.ByteString.Lazy as B

disassemble :: Word64 -> ByteString -> ([Instruction], ByteString)
disassemble ofs s = case runGetOrFail (disassemble1 ofs) s of
                    Left _          -> ([], s)
                    Right (r, b, i) -> let (i',r') = disassemble (ofs+(fromIntegral b)) r in (i:i', r')

disassemble1 :: Word64 -> Get Instruction
disassemble1 ofs = disassemble1' basicOpcodeMap (dsInitial ofs)

disassemble1' :: OpcodeMap -> DisassemblerState -> Get Instruction
disassemble1' map ds = do opcode <- getWord8
                          case Map.lookup opcode map of
                              Nothing -> fail "invalid"
                              Just f  -> f opcode ds

data DisassemblerState = DisassemblerState {
      dsPfx :: [Prefix]
    , dsOffset :: Word64
}

type DisassemblerSingle = Word8 -> DisassemblerState -> Get Instruction
type OpcodeMap = Map.Map Word8 DisassemblerSingle

dsInitial ofs = DisassemblerState { dsOffset = ofs, dsPfx = [] }

prefixMap = Map.fromList ([ (0x26, PrefixSeg ES), (0x2e, PrefixSeg CS), (0x36, PrefixSeg SS), (0x3e, PrefixSeg DS),
                           (0x64, PrefixSeg FS), (0x65, PrefixSeg GS), (0x66, PrefixO16), (0x67, PrefixA32),
                           (0xF0, PrefixLock), (0xF2, PrefixRepNE), (0xF3, PrefixRep)
                          ] ++ [ (x, PrefixRex x) | x <- [0x40..0x4f] ])

dsRex :: DisassemblerState -> Maybe Prefix
dsRex ds = find (\x -> case x of PrefixRex _ -> True; _ -> False) (dsPfx ds)
dsO16 ds = PrefixO16 `elem` (dsPfx ds)
dsA32 ds = PrefixA32 `elem` (dsPfx ds)
dsRep ds = PrefixRep `elem` (dsPfx ds)
dsSeg ds = listToMaybe (catMaybes (map (\p -> case p of (PrefixSeg s) -> Just s; _ -> Nothing) (dsPfx ds)))

opOpWidth :: Word8 -> DisassemblerState -> Int
opOpWidth o ds =
        o' (o .&. (bit 0)) (bitTest 3 (dsRex ds)) (dsO16 ds)
            where
                o' 0 _     _     = 8
                o' _ False True  = 16
                o' 1 False False = 32
                o' _ True  _     = 64

opOpWidth' o ds =
        o' (bitTest 3 (dsRex ds)) (dsO16 ds)
            where
                o' True _       = 64
                o' _    False   = 32
                o' _    True    = 16

opOpWidthA o ds = o' (bitTest 3 (dsRex ds)) (dsO16 ds)
            where
                o' _ False   = 64
                o' _ True    = 16
--

bits s l i = fromIntegral $ (i `shiftR` s) .&. ((1 `shiftL` l) - 1)

bitTest :: Int -> Maybe Prefix -> Bool
bitTest i v = case v of
                Nothing -> False
                Just (PrefixRex n) -> n .&. (bit i) /= 0

fetchImm opWidth = Op_Imm <$> (case opWidth of 8  -> Immediate 8 . fromIntegral <$> getWord8
                                               16 -> Immediate 16 . fromIntegral <$> getWord16le
                                               32 -> Immediate 32 . fromIntegral <$> getWord32le
                                               64 -> Immediate 64 . fromIntegral <$> getWord32le)

--

op2 :: Operation -> DisassemblerSingle
op2 i opcode ds = op2aux (opOpWidth opcode ds) (bits 1 1 opcode) i ds

op2xt :: Operation -> DisassemblerSingle
op2xt i opcode ds = op2aux (opOpWidth opcode ds) 0 i ds

op2aux opWidth direction i ds = do
        (rm, reg, _, _, _) <- modrm ds opWidth
        let ops = case direction of
                        0 -> [rm, Op_Reg reg]
                        _ -> [Op_Reg reg, rm]
            ep = dsPfx ds
          in return (Instruction ep i ops)

opImm :: Operation -> DisassemblerSingle
opImm i opcode ds = let
        pfx = dsPfx ds
        opWidth = opOpWidth opcode ds
    in do
        imm <- fetchImm opWidth
        let reg = case opWidth of
                 8 -> Reg8 RAX HalfL
                 16 -> Reg16 RAX
                 32 -> (if bitTest 3 (dsRex ds) then Reg64 else Reg32) RAX
                 64 -> Reg64 RAX
            ep = pfx
          in return (Instruction ep i [Op_Reg reg, imm])

grp50 :: Operation -> DisassemblerSingle
grp50 i opcode ds = let
        r = bits 0 3 opcode
        opWidth = opOpWidthA opcode ds
        pfx = dsPfx ds
        reg = selectreg 0 r opWidth (dsRex ds) False
        ep = pfx
    in return (Instruction ep i [Op_Reg reg])

movsxd :: DisassemblerSingle
movsxd opcode ds = let pfx = dsPfx ds
                       o16 = dsO16 ds
                       rexW = bitTest 3 (dsRex ds)
                       (o1w, o2w) = case (o16, rexW) of
                                    (False, False) -> (32, 32)
                                    (True,  False) -> (16, 32)
                                    (False, True)  -> (64, 32)
                                    (True,  True)  -> (64, 32)
    in do (rm, reg, _, _, _) <- modrm' ds o2w o1w False
          let ops = [Op_Reg reg, rm]
              ep = pfx
            in return (Instruction ep I_MOVSXD ops)

pushImm :: DisassemblerSingle
pushImm opcode ds = let pfx = dsPfx ds; opWidth = case (dsO16 ds, opcode) of (_, 0x6a) -> 8; (True, 0x68) -> 16; (False, 0x68) -> 32 in do
    imm <- fetchImm opWidth
    let ep = pfx
      in return (Instruction ep I_PUSH [imm])

imul3 :: DisassemblerSingle
imul3 opcode ds = let pfx = dsPfx ds; opWidth = opOpWidth' opcode ds; immSize = case opcode of 0x69 -> opWidth; 0x6b -> 8 in do
    (rm, reg, _, _, _) <- modrm ds opWidth
    imm <- fetchImm immSize
    let ep = pfx
      in return (Instruction ep I_IMUL [Op_Reg reg, rm, imm])

simple :: Operation -> [Operand] -> DisassemblerSingle
simple i opl opcode ds = let
        pfx = dsPfx ds
        ep = pfx
    in return (Instruction ep i opl)

jshort :: Operation -> DisassemblerSingle
jshort i opcode ds = do
            disp <- fromIntegral <$> getInt8
            eip <- (((dsOffset ds)+).fromIntegral <$> bytesRead)
            let iv = bits 0 64 (eip + disp)
                imm = Immediate 64 iv
                ep = dsPfx ds
              in return (Instruction ep i [Op_Jmp imm])

grp80 :: DisassemblerSingle
grp80 opcode ds = let
        (opWidth, immSize) = case opcode of 0x80 -> (o, 8); 0x81 -> (o, o); 0x83 -> (o, 8)
        o = opOpWidth opcode ds
    in do
        (rm, _, op, _, _) <- modrm ds opWidth
        imm <- fetchImm immSize
        let i = case op of
                    0 -> I_ADD
                    1 -> I_OR
                    2 -> I_ADC
                    3 -> I_SBB
                    4 -> I_AND
                    5 -> I_SUB
                    6 -> I_XOR
                    7 -> I_CMP
            ep = dsPfx ds
          in return (Instruction ep i [rm, imm])

movsr opcode ds = do
    (rm, _, sr, _, _) <- modrm ds (if dsO16 ds then 16 else 32)
    let sreg = RegSeg ( [ES, CS, SS, DS, FS, GS, SR6, SR7] !! sr )
        ops = case (bits 1 1 opcode) of
                    0 -> [rm, Op_Reg sreg]
                    _ -> [Op_Reg sreg, rm]
        ep = dsPfx ds
      in return (Instruction ep I_MOV ops)

lea opcode ds = do
    (rm, reg, _, mod, _) <- modrm' ds 64 (opOpWidth opcode ds) False
    if mod == 3 then fail "invalid" else
        let ops = [Op_Reg reg, rm]
            ep = dsPfx ds
          in return (Instruction ep I_LEA ops)

pushpop i opcode ds = do
    (rm, _, op, _, _) <- modrm ds 64
    case op of 0 -> return (Instruction (dsPfx ds) i [rm])
               _ -> fail "invalid"

xchg opcode ds = let
        opWidth = opOpWidth' opcode ds
        reg1 = selectreg 0 (bits 0 3 opcode) opWidth (dsRex ds) False
        reg2 = selectreg 0 0 opWidth (Nothing :: Maybe Prefix) False
    in if (reg1 == reg2)
         then return (Instruction (dsPfx ds) (if (dsRep ds) then I_PAUSE else I_NOP) [])
         else return (Instruction (dsPfx ds) I_XCHG [Op_Reg reg1, Op_Reg reg2])

sized i16 i32 i64 opcode ds =
    let i = case opOpWidth' opcode ds of 64 -> i64; 32 -> i32; 16 -> i16
    in return (Instruction (dsPfx ds) i [])

sized' i16 i32 opcode ds = let r i = return (Instruction (dsPfx ds) i [])
    in case opOpWidth' opcode ds of 32 -> r i32; 16 -> r i16; _ -> fail "invalid"

movaddr opcode ds = let
    aWidth = case (dsA32 ds) of
                 (True)  -> 32
                 (False) -> 64
    opWidth = opOpWidth opcode ds
    direction = bits 1 1 opcode
     in do
        disp <- case aWidth of
                64 -> fromIntegral <$> getWord64le
                32 -> fromIntegral <$> getWord32le
        let imm = Op_Mem opWidth aWidth RegNone RegNone 0 (Immediate aWidth disp) (dsSeg ds)
            ep = dsPfx ds
            reg = selectreg 0 0 opWidth (Nothing :: Maybe Prefix) False
            ops = case direction of
                                0 -> [Op_Reg reg, imm]
                                _ -> [imm, Op_Reg reg]
          in return (Instruction ep I_MOV ops)

testRax opcode ds = let opWidth = opOpWidth opcode ds in do
    imm <- fetchImm opWidth
    let reg = selectreg 0 0 opWidth (Nothing :: Maybe Prefix) False
        ep = dsPfx ds
      in return (Instruction ep I_TEST [Op_Reg reg, imm])

movreg opcode ds = let
        r = bits 0 3 opcode
        opWidth = case (bits 3 1 opcode) of 0 -> 8; _ -> opOpWidth' opcode ds
        reg = selectreg 0 r opWidth (dsRex ds) False
        ep = dsPfx ds
    in do
        imm <- (Immediate opWidth) <$> case opWidth of 8  -> fromIntegral <$> getWord8
                                                       16 -> fromIntegral <$> getWord16le
                                                       32 -> fromIntegral <$> getWord32le
                                                       64 -> fromIntegral <$> getWord64le
        return (Instruction ep I_MOV [Op_Reg reg, Op_Imm imm])

shiftrot opcode ds = let opWidth = opOpWidth opcode ds
    in do
        (rm, _, op, _, _) <- modrm ds opWidth
        imm <- (Immediate 8 . fromIntegral) <$> getWord8
        shiftrot' ds op rm (Op_Imm imm) opWidth

shiftrot1 op2 opcode ds = let opWidth = opOpWidth opcode ds
    in do
        (rm, _, op, _, _) <- modrm ds opWidth
        shiftrot' ds op rm op2 opWidth

shiftrot' ds op rm op2 opWidth =
        let i = case op of
                    0 -> I_ROL
                    1 -> I_ROR
                    2 -> I_RCL
                    3 -> I_RCR
                    4 -> I_SHL
                    5 -> I_SHR
                    6 -> I_SHL
                    7 -> I_SAR
            ep = dsPfx ds
          in return (Instruction ep i [rm, op2])

ret opcode ds = do
    i <- Immediate 16 <$> fromIntegral <$> getWord16le
    simple I_RET [Op_Imm i] opcode ds

retf opcode ds = do
    i <- Immediate 16 <$> fromIntegral <$> getWord16le
    simple I_RETF [Op_Imm i] opcode ds

movimm opcode ds = let opWidth = opOpWidth opcode ds
    in do
        (rm, _, op, _, _) <- modrm ds opWidth
        case op of 0 -> do imm <- fetchImm opWidth
                           return (Instruction (dsPfx ds) I_MOV [rm, imm])
                   _ -> fail "invalid"

enter opcode ds = do
    op1 <- Immediate 16 <$> fromIntegral <$> getWord16le
    op2 <- Immediate 8 <$> fromIntegral <$> getWord8
    simple I_ENTER [Op_Imm op1, Op_Imm op2] opcode ds

int opcode ds = do
    op1 <- Immediate 8 <$> fromIntegral <$> getWord8
    simple I_INT [Op_Imm op1] opcode ds

inout o opcode ds = let
    opWidth = o' (bits 0 1 opcode) (bitTest 3 (dsRex ds)) (dsO16 ds)
                  where
                      o' 0 _ _       = 8
                      o' _ _ True    = 16
                      o' 1 _ False   = 32
    direction = bits 1 1 opcode
    op2 = selectreg 0 0 opWidth (Nothing :: Maybe Prefix) False
    in do
        op1 <- case o of Nothing -> Op_Imm <$> Immediate 8 <$> fromIntegral <$> getWord8
                         Just o' -> return o'
        let
            (i,ops) = case direction of
                       0 -> (I_IN, [Op_Reg op2, op1])
                       _ -> (I_OUT, [op1, Op_Reg op2])
            ep = dsPfx ds
          in return (Instruction ep i ops)

jmpcall i opcode ds = let
        opWidth = case (dsO16 ds) of
                 (True)  -> 16
                 (False) -> 32
        ofs = dsOffset ds
         in do
            disp <- case opWidth of
                    16 -> fromIntegral <$> getInt16le
                    32 -> fromIntegral <$> getInt32le
            eip <- ((ofs+).fromIntegral <$> bytesRead)
            let iv = bits 0 64 (eip + disp)
                imm = Immediate 64 iv
                ep = dsPfx ds
              in return (Instruction ep i [Op_Jmp imm])

grpf6 opcode ds = let opWidth = opOpWidth opcode ds
    in do
        (rm, _, op, _, _) <- modrm ds opWidth
        let ep = dsPfx ds
            f6test ep rm = do imm <- (Immediate opWidth) <$> case opWidth of
                                                       8  -> fromIntegral <$> getWord8
                                                       16 -> fromIntegral <$> getWord16le
                                                       32 -> fromIntegral <$> getWord32le
                                                       64 -> fromIntegral <$> getWord64le
                              return (Instruction ep I_TEST [rm, Op_Imm imm])
            in case op of
                0 -> f6test ep rm
                1 -> f6test ep rm
                2 -> return (Instruction ep I_NOT [rm])
                3 -> return (Instruction ep I_NEG [rm])
                4 -> return (Instruction ep I_MUL [rm])
                5 -> return (Instruction ep I_IMUL [rm])
                6 -> return (Instruction ep I_DIV [rm])
                7 -> return (Instruction ep I_IDIV [rm])

grpfe opcode ds = let bitW = bits 0 1 opcode
                      opWidth = opOpWidth opcode ds
                    in do
                         md <- lookAhead getWord8
                         let op = bits 3 3 md
                             ep = dsPfx ds
                            in case (bitW,op) of
                                (_,0) -> do (rm, _, op, mod, _) <- modrm ds opWidth; return (Instruction ep I_INC [rm])
                                (_,1) -> do (rm, _, op, mod, _) <- modrm ds opWidth; return (Instruction ep I_DEC [rm])
                                (1,2) -> do (rm, _, op, mod, _) <- modrm ds 64; return (Instruction ep I_CALL [Op_Near rm])
                                (1,3) -> do (rm, _, op, mod, _) <- modrm ds opWidth; return (Instruction ep I_CALL [Op_Far rm])
                                (1,4) -> do (rm, _, op, mod, _) <- modrm ds 64; if (mod == 3) then fail "invalid" else return (Instruction ep I_JMP [Op_Near rm])
                                (1,5) -> do (rm, _, op, mod, _) <- modrm ds 32; return (Instruction ep I_JMP [Op_Far rm])
                                (1,6) -> do (rm, _, op, mod, _) <- modrm ds opWidth; return (Instruction ep I_PUSH [rm])
                                _     -> fail "invalid"


fpu opcode ds = do
            rmb <- lookAhead getWord8
            let set = bits 0 3 opcode
                op = bits 0 3 rmb
                opWidth = case (set, op) of (3,0) -> 32; (3,1) -> 32; (3,2) -> 32; (3,3) -> 32; (3,_) -> 64; (4,_) -> 64; (5,_) -> 64; (6,_) -> 16; (7,7) -> 64; (7,5) -> 64; (7,0) -> 16; _ -> 32
             in do
                (rm, _, op, mod, reg) <- modrmFpu ds opWidth
                let ep = dsPfx ds
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
                        (1, 1, 3) -> rr I_FXCH
                        (1, 2, 3) -> if reg == 0 then r I_FNOP [] else fail "invalid"
                        (1, 2, _) -> rr I_FST
                        (1, 3, 3) -> r I_FSTP1 [fpureg]
                        (1, 3, _) -> rr I_FSTP
                        (1, 4, 3) -> case reg of
                                        0 -> r I_FCHS []
                                        1 -> r I_FABS []
                                        4 -> r I_FTST []
                                        5 -> r I_FXAM []
                                        _ -> fail "invalid"
                        (1, 4, _) -> rr I_FLDENV
                        (1, 5, 3) -> case reg of
                                        0 -> r I_FLD1 []
                                        1 -> r I_FLDL2T []
                                        2 -> r I_FLDL2E []
                                        3 -> r I_FLDPI []
                                        4 -> r I_FLDLG2 []
                                        5 -> r I_FLDLN2 []
                                        6 -> r I_FLDZ []
                                        _ -> fail "invalid"
                        (1, 5, _) -> rr I_FLDCW
                        (1, 6, 3) -> case reg of
                                        0 -> r I_F2XM1 []
                                        1 -> r I_FYL2X []
                                        2 -> r I_FPTAN []
                                        3 -> r I_FPATAN []
                                        4 -> r I_FPXTRACT []
                                        5 -> r I_FPREM1 []
                                        6 -> r I_FDECSTP []
                                        7 -> r I_FINCSTP []
                        (1, 6, _) -> rr I_FSTENV
                        (1, 7, 3) -> case reg of
                                        0 -> r I_FPREM []
                                        1 -> r I_FYL2XP1 []
                                        2 -> r I_FSQRT []
                                        3 -> r I_FSINCOS []
                                        4 -> r I_FRNDINT []
                                        5 -> r I_FSCALE []
                                        6 -> r I_FSIN []
                                        7 -> r I_FCOS []
                        (1, 7, _) -> rr I_FSTCW

                        (2, 0, 3) -> r I_FCMOVB [st0, fpureg]
                        (2, 1, 3) -> r I_FCMOVE [st0, fpureg]
                        (2, 2, 3) -> r I_FCMOVBE [st0, fpureg]
                        (2, 3, 3) -> r I_FCMOVU [st0, fpureg]
                        (2, 4, 3) -> fail "invalid"
                        (2, 5, 3) -> if reg == 1 then r I_FUCOMPP [] else fail "invalid"
                        (2, 6, 3) -> fail "invalid"
                        (2, 7, 3) -> fail "invalid"
                        (2, 0, _) -> rr I_FIADD
                        (2, 1, _) -> rr I_FIMUL
                        (2, 2, _) -> rr I_FICOM
                        (2, 3, _) -> rr I_FICOMP
                        (2, 4, _) -> rr I_FISUB
                        (2, 5, _) -> rr I_FISUBR
                        (2, 6, _) -> rr I_FIDIV
                        (2, 7, _) -> rr I_FIDIVR

                        (3, 0, 3) -> r I_FCMOVNB [st0, fpureg]
                        (3, 1, 3) -> r I_FCMOVNE [st0, fpureg]
                        (3, 2, 3) -> r I_FCMOVNBE [st0, fpureg]
                        (3, 3, 3) -> r I_FCMOVNU [st0, fpureg]
                        (3, 0, _) -> rr I_FILD
                        (3, 1, _) -> rr I_FISTTP
                        (3, 7, _) -> rr I_FSTP

                        (4, 0, _) -> rr I_FADD
                        (4, 1, _) -> rr I_FMUL
                        (4, 2, 3) -> r I_FCOM2 [fpureg]
                        (4, 2, _) -> rr I_FCOM
                        (4, 3, _) -> rr I_FCOMP
                        (4, 4, _) -> rr I_FSUB
                        (4, 5, _) -> rr I_FSUBR
                        (4, 6, _) -> rr I_FDIV
                        (4, 7, _) -> rr I_FDIVR

                        (5, 1, _) -> r I_FISTTP [rm]
                        (5, 2, _) -> r I_FST [rm]
                        (5, 3, _) -> r I_FSTP [rm]
                        (5, 4, _) -> r I_FRSTOR [rm]
                        (5, 0, 0) -> r I_FLD [rm]
                        (5, 0, 3) -> r I_FFREE [rm]

                        (6, 0, 3) -> r I_FIADD [rm]
                        (6, 0, _) -> r I_FADDP [rm]
                        (6, 1, _) -> r I_FMULP [rm]
                        (6, 2, _) -> r I_FCOMP [rm]
                        (6, 3, _) -> r I_FCOMPP [rm]
                        (6, 4, _) -> r I_FSUBP [rm]
                        (6, 5, _) -> r I_FSUBRP [rm]
                        (6, 6, _) -> r I_FDIVP [rm]
                        (6, 7, _) -> r I_FDIVRP [rm]

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


applyPrefix :: DisassemblerSingle
applyPrefix opcode ds = disassemble1' basicOpcodeMap (addPfx opcode ds)
  where addPfx o ds = let newpfx = (Map.!) prefixMap o
                          isseg = case newpfx of (PrefixSeg _) -> True; _ -> False
                          fi pfx = case pfx of (PrefixRex _) -> False;
                                               (PrefixSeg _) -> not isseg
                                               _ | pfx == newpfx -> False
                                                 | otherwise -> True
                          pfx' = filter fi (dsPfx ds)
                        in ds { dsPfx = ((Map.!) prefixMap o):pfx' }

invalid :: DisassemblerSingle
invalid _ _ = fail "invalid"

basicOpcodeMap :: OpcodeMap
basicOpcodeMap = Map.fromList [
           ( 0x00, op2 I_ADD
        ), ( 0x01, op2 I_ADD
        ), ( 0x02, op2 I_ADD
        ), ( 0x03, op2 I_ADD
        ), ( 0x04, opImm I_ADD
        ), ( 0x05, opImm I_ADD
        ), ( 0x06, invalid
        ), ( 0x07, invalid
        ), ( 0x08, op2 I_OR
        ), ( 0x09, op2 I_OR
        ), ( 0x0a, op2 I_OR
        ), ( 0x0b, op2 I_OR
        ), ( 0x0c, opImm I_OR
        ), ( 0x0d, opImm I_OR
        ), ( 0x0e, invalid
        ), ( 0x0f, \opcode ds -> disassemble1' opcodeMap0f ds
        ), ( 0x10, op2 I_ADC
        ), ( 0x11, op2 I_ADC
        ), ( 0x12, op2 I_ADC
        ), ( 0x13, op2 I_ADC
        ), ( 0x14, opImm I_ADC
        ), ( 0x15, opImm I_ADC
        ), ( 0x16, invalid
        ), ( 0x17, invalid
        ), ( 0x18, op2 I_SBB
        ), ( 0x19, op2 I_SBB
        ), ( 0x1a, op2 I_SBB
        ), ( 0x1b, op2 I_SBB
        ), ( 0x1c, opImm I_SBB
        ), ( 0x1d, opImm I_SBB
        ), ( 0x1e, invalid
        ), ( 0x1f, invalid
        ), ( 0x20, op2 I_AND
        ), ( 0x21, op2 I_AND
        ), ( 0x22, op2 I_AND
        ), ( 0x23, op2 I_AND
        ), ( 0x24, opImm I_AND
        ), ( 0x25, opImm I_AND
        ), ( 0x26, applyPrefix -- es
        ), ( 0x27, invalid
        ), ( 0x28, op2 I_SUB
        ), ( 0x29, op2 I_SUB
        ), ( 0x2a, op2 I_SUB
        ), ( 0x2b, op2 I_SUB
        ), ( 0x2c, opImm I_SUB
        ), ( 0x2d, opImm I_SUB
        ), ( 0x2e, applyPrefix -- cs
        ), ( 0x2f, invalid
        ), ( 0x30, op2 I_XOR
        ), ( 0x31, op2 I_XOR
        ), ( 0x32, op2 I_XOR
        ), ( 0x33, op2 I_XOR
        ), ( 0x34, opImm I_XOR
        ), ( 0x35, opImm I_XOR
        ), ( 0x36, applyPrefix -- ss
        ), ( 0x37, invalid
        ), ( 0x38, op2 I_CMP
        ), ( 0x39, op2 I_CMP
        ), ( 0x3a, op2 I_CMP
        ), ( 0x3b, op2 I_CMP
        ), ( 0x3c, opImm I_CMP
        ), ( 0x3d, opImm I_CMP
        ), ( 0x3e, applyPrefix -- ds
        ), ( 0x3f, invalid
        ), ( 0x40, applyPrefix -- rex
        ), ( 0x41, applyPrefix -- rex
        ), ( 0x42, applyPrefix -- rex
        ), ( 0x43, applyPrefix -- rex
        ), ( 0x44, applyPrefix -- rex
        ), ( 0x45, applyPrefix -- rex
        ), ( 0x46, applyPrefix -- rex
        ), ( 0x47, applyPrefix -- rex
        ), ( 0x48, applyPrefix -- rex
        ), ( 0x49, applyPrefix -- rex
        ), ( 0x4a, applyPrefix -- rex
        ), ( 0x4b, applyPrefix -- rex
        ), ( 0x4c, applyPrefix -- rex
        ), ( 0x4d, applyPrefix -- rex
        ), ( 0x4e, applyPrefix -- rex
        ), ( 0x4f, applyPrefix -- rex
        ), ( 0x50, grp50 I_PUSH
        ), ( 0x51, grp50 I_PUSH
        ), ( 0x52, grp50 I_PUSH
        ), ( 0x53, grp50 I_PUSH
        ), ( 0x54, grp50 I_PUSH
        ), ( 0x55, grp50 I_PUSH
        ), ( 0x56, grp50 I_PUSH
        ), ( 0x57, grp50 I_PUSH
        ), ( 0x58, grp50 I_POP
        ), ( 0x59, grp50 I_POP
        ), ( 0x5a, grp50 I_POP
        ), ( 0x5b, grp50 I_POP
        ), ( 0x5c, grp50 I_POP
        ), ( 0x5d, grp50 I_POP
        ), ( 0x5e, grp50 I_POP
        ), ( 0x5f, grp50 I_POP
        ), ( 0x60, invalid
        ), ( 0x61, invalid
        ), ( 0x62, invalid
        ), ( 0x63, movsxd
        ), ( 0x64, applyPrefix -- fs
        ), ( 0x65, applyPrefix -- gs
        ), ( 0x66, applyPrefix -- o16
        ), ( 0x67, applyPrefix -- a32
        ), ( 0x68, pushImm
        ), ( 0x69, imul3
        ), ( 0x6a, pushImm
        ), ( 0x6b, imul3
        ), ( 0x6c, simple I_INSB []
        ), ( 0x6d, sized' I_INSW I_INSD
        ), ( 0x6e, simple I_OUTSB []
        ), ( 0x6f, sized I_OUTSW I_OUTSD I_OUTSQ
        ), ( 0x70, jshort I_JO
        ), ( 0x71, jshort I_JNO
        ), ( 0x72, jshort I_JB
        ), ( 0x73, jshort I_JAE
        ), ( 0x74, jshort I_JZ
        ), ( 0x75, jshort I_JNZ
        ), ( 0x76, jshort I_JBE
        ), ( 0x77, jshort I_JA
        ), ( 0x78, jshort I_JS
        ), ( 0x79, jshort I_JNS
        ), ( 0x7a, jshort I_JP
        ), ( 0x7b, jshort I_JNP
        ), ( 0x7c, jshort I_JL
        ), ( 0x7d, jshort I_JGE
        ), ( 0x7e, jshort I_JLE
        ), ( 0x7f, jshort I_JG
        ), ( 0x80, grp80
        ), ( 0x81, grp80
        ), ( 0x82, invalid
        ), ( 0x83, grp80
        ), ( 0x84, op2xt I_TEST
        ), ( 0x85, op2xt I_TEST
        ), ( 0x86, op2xt I_XCHG
        ), ( 0x87, op2xt I_XCHG
        ), ( 0x88, op2 I_MOV
        ), ( 0x89, op2 I_MOV
        ), ( 0x8a, op2 I_MOV
        ), ( 0x8b, op2 I_MOV
        ), ( 0x8c, movsr
        ), ( 0x8d, lea
        ), ( 0x8e, movsr
        ), ( 0x8f, pushpop I_POP
        ), ( 0x90, xchg
        ), ( 0x91, xchg
        ), ( 0x92, xchg
        ), ( 0x93, xchg
        ), ( 0x94, xchg
        ), ( 0x95, xchg
        ), ( 0x96, xchg
        ), ( 0x97, xchg
        ), ( 0x98, sized I_CBW I_CWDE I_CDQE
        ), ( 0x99, sized I_CWD I_CDQ I_CQO
        ), ( 0x9a, invalid
        ), ( 0x9b, simple I_WAIT []
        ), ( 0x9c, simple I_PUSHFQ []
        ), ( 0x9d, simple I_POPFQ []
        ), ( 0x9e, simple I_SAHF []
        ), ( 0x9f, simple I_LAHF []
        ), ( 0xa0, movaddr
        ), ( 0xa1, movaddr
        ), ( 0xa2, movaddr
        ), ( 0xa3, movaddr
        ), ( 0xa4, simple I_MOVSB []
        ), ( 0xa5, sized I_MOVSW I_MOVSD I_MOVSQ
        ), ( 0xa6, simple I_CMPSB []
        ), ( 0xa7, sized I_CMPSW I_CMPSD I_CMPSQ
        ), ( 0xa8, testRax
        ), ( 0xa9, testRax
        ), ( 0xaa, simple I_STOSB []
        ), ( 0xab, sized I_STOSW I_STOSD I_STOSQ
        ), ( 0xac, simple I_LODSB []
        ), ( 0xad, sized I_LODSW I_LODSD I_LODSQ
        ), ( 0xae, simple I_SCASB []
        ), ( 0xaf, sized I_SCASW I_SCASD I_SCASQ
        ), ( 0xb0, movreg
        ), ( 0xb1, movreg
        ), ( 0xb2, movreg
        ), ( 0xb3, movreg
        ), ( 0xb4, movreg
        ), ( 0xb5, movreg
        ), ( 0xb6, movreg
        ), ( 0xb7, movreg
        ), ( 0xb8, movreg
        ), ( 0xb9, movreg
        ), ( 0xba, movreg
        ), ( 0xbb, movreg
        ), ( 0xbc, movreg
        ), ( 0xbd, movreg
        ), ( 0xbe, movreg
        ), ( 0xbf, movreg
        ), ( 0xc0, shiftrot
        ), ( 0xc1, shiftrot
        ), ( 0xc2, ret
        ), ( 0xc3, simple I_RET []
        ), ( 0xc4, invalid
        ), ( 0xc5, invalid
        ), ( 0xc6, movimm
        ), ( 0xc7, movimm
        ), ( 0xc8, enter
        ), ( 0xc9, simple I_LEAVE []
        ), ( 0xca, retf
        ), ( 0xcb, simple I_RETF []
        ), ( 0xcc, simple I_INT3 []
        ), ( 0xcd, int
        ), ( 0xce, invalid
        ), ( 0xcf, sized I_IRETW I_IRETD I_IRETQ
        ), ( 0xd0, shiftrot1 (Op_Const 1)
        ), ( 0xd1, shiftrot1 (Op_Const 1)
        ), ( 0xd2, shiftrot1 (Op_Reg (Reg8 RCX HalfL))
        ), ( 0xd3, shiftrot1 (Op_Reg (Reg8 RCX HalfL))
        ), ( 0xd4, invalid
        ), ( 0xd5, invalid
        ), ( 0xd6, invalid
        ), ( 0xd7, simple I_XLATB []
        ), ( 0xd8, fpu
        ), ( 0xd9, fpu
        ), ( 0xda, fpu
        ), ( 0xdb, fpu
        ), ( 0xdc, fpu
        ), ( 0xdd, fpu
        ), ( 0xde, fpu
        ), ( 0xdf, fpu
        ), ( 0xe0, jshort I_LOOPNZ
        ), ( 0xe1, jshort I_LOOPE
        ), ( 0xe2, jshort I_LOOP
        ), ( 0xe3, jshort I_JECXZ
        ), ( 0xe4, inout Nothing
        ), ( 0xe5, inout Nothing
        ), ( 0xe6, inout Nothing
        ), ( 0xe7, inout Nothing
        ), ( 0xe8, jmpcall I_CALL
        ), ( 0xe9, jmpcall I_JMP
        ), ( 0xea, invalid
        ), ( 0xeb, jshort I_JMP
        ), ( 0xec, inout (Just (Op_Reg (Reg16 RDX)))
        ), ( 0xed, inout (Just (Op_Reg (Reg16 RDX)))
        ), ( 0xee, inout (Just (Op_Reg (Reg16 RDX)))
        ), ( 0xef, inout (Just (Op_Reg (Reg16 RDX)))
        ), ( 0xf0, applyPrefix
        ), ( 0xf1, simple I_INT1 []
        ), ( 0xf2, applyPrefix
        ), ( 0xf3, applyPrefix
        ), ( 0xf4, simple I_HLT []
        ), ( 0xf5, simple I_CMC []
        ), ( 0xf6, grpf6
        ), ( 0xf7, grpf6
        ), ( 0xf8, simple I_CLC []
        ), ( 0xf9, simple I_STC []
        ), ( 0xfa, simple I_CLI []
        ), ( 0xfb, simple I_STI []
        ), ( 0xfc, simple I_CLD []
        ), ( 0xfd, simple I_STD []
        ), ( 0xfe, grpfe
        ), ( 0xff, grpfe
        ) ]

--

bswap :: DisassemblerSingle
bswap opcode ds = let
    reg = selectreg 0 (bits 0 3 opcode) (if (bitTest 3 (dsRex ds)) then 64 else 32) (dsRex ds) False
    ep = dsPfx ds
    in return (Instruction ep I_BSWAP [Op_Reg reg])

grp0f00 :: DisassemblerSingle
grp0f00 opcode ds = let pfx = dsPfx ds in do
    rmb <- lookAhead getWord8
    case bits 3 3 rmb of
       0 -> do (rm, _, _, _, _) <- modrm ds 32; let ep = dsPfx ds in return (Instruction ep I_SLDT [rm])
       1 -> do (rm, _, _, _, _) <- modrm ds 16; let ep = dsPfx ds in return (Instruction ep I_STR [rm])
       _ -> fail "invalid"

opcodeMap0f :: OpcodeMap
opcodeMap0f = Map.fromList [
           ( 0x00, grp0f00
        ), ( 0x05, simple I_SYSCALL []
        ), ( 0x06, simple I_CLTS []
        ), ( 0x07, simple I_SYSRET []
        ), ( 0x08, simple I_INVD []
        ), ( 0xc8, bswap
        ), ( 0xc9, bswap
        ), ( 0xca, bswap
        ), ( 0xcb, bswap
        ), ( 0xcc, bswap
        ), ( 0xcd, bswap
        ), ( 0xce, bswap
        ), ( 0xcf, bswap
        ) ]

--

modrm ds opWidth = modrm' ds opWidth opWidth False

modrmFpu ds opWidth = modrm' ds opWidth opWidth True

modrm' ds opWidth opWidth' fpu = do
    modrm <- getWord8
    let b'mod = bits 6 2 modrm
        b'reg = bits 3 3 modrm
        b'rm  = bits 0 3 modrm
        aWidth = if (dsA32 ds) then 32 else 64
        reg = selectreg 2 b'reg opWidth' (dsRex ds) fpu
        hasSib = (b'mod /= 3 && b'rm == 4)
        dispSize = case (b'mod, b'rm) of
            (0,5) -> Just 32
            (1,_) -> Just 8
            (2,_) -> Just 32
            _     -> Nothing
        so = dsSeg ds
      in do
        (sib,dispSize') <- if hasSib then (parseSib b'mod dispSize (dsRex ds) aWidth) <$> getWord8
                                     else return ((RegNone,RegNone,0),dispSize)
        disp <- case dispSize' of
                    Just 8 -> (Immediate 8 . fromIntegral) <$> getInt8
                    Just 32 -> (Immediate 32 . fromIntegral) <$> getInt32le
                    _  -> return $ Immediate 0 0
        let rm = case (b'mod, b'rm) of
                (3,_) -> Op_Reg (selectreg 0 b'rm opWidth (dsRex ds) fpu)
                (0,5) -> Op_Mem opWidth aWidth ((if aWidth == 64 then Reg64 else Reg32) RIP) RegNone 0 disp so
                (0,4) -> let (br, ir, sc) = sib in Op_Mem opWidth aWidth br ir sc disp so
                (1,4) -> let (br, ir, sc) = sib in Op_Mem opWidth aWidth br ir sc disp so
                (2,4) -> let (br, ir, sc) = sib in Op_Mem opWidth aWidth br ir sc disp so
                (_,_) -> Op_Mem opWidth aWidth (selectreg 0 b'rm aWidth (dsRex ds) False) RegNone 0 disp so
          in return (rm, reg, b'reg, b'mod, b'rm)

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

selectreg :: Int -> Int -> Int -> Maybe Prefix -> Bool -> Register
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

