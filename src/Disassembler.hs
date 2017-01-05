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

-- data PrefixState = PrefixState {
--           pfxRex  ::  Maybe Word8
--         , pfxO16  :: Bool
--         , pfxA32  :: Bool
--         , pfxRep  :: Maybe Prefix
--         , pfxLock :: Bool
--         , pfxSeg  :: Maybe SReg
--     }
--
-- pfxNone = PrefixState Nothing False False Nothing False Nothing

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
movsxd opcode ds = let pfx = dsPfx ds in do
    (rm, reg, _, _, _) <- modrm' ds (if bitTest 3 (dsRex ds) then 64 else 32) (if (dsO16 ds) then 16 else 32) False
    let ops = [Op_Reg reg, rm]
        ep = pfx
      in return (Instruction ep I_MOVSXD ops)

pushImm :: DisassemblerSingle
pushImm opcode ds = let pfx = dsPfx ds; opWidth = case (dsO16 ds, opcode) of (_, 0x6a) -> 8; (True, 0x68) -> 16; (False, 0x68) -> 32 in do
    imm <- fetchImm opWidth
    let ep = pfx
      in return (Instruction ep I_PUSH [imm])

imul3 :: DisassemblerSingle
imul3 opcode ds = let pfx = dsPfx ds; opWidth = opOpWidth' opcode ds; immSize = case opcode of 0x69 -> opWidth; 0x6a -> 8 in do
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
        (opWidth, immSize) = case opcode of 0x80 -> (o, 8); 0x80 -> (o, o); 0x83 -> (o, 8)
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
    (rm, _, sr, _, _) <- modrm ds (opOpWidth opcode ds)
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


applyPrefix :: DisassemblerSingle
applyPrefix opcode ds = disassemble1' basicOpcodeMap (addPfx opcode ds)
  where addPfx o ds = ds { dsPfx = ((Map.!) prefixMap o):(dsPfx ds) }

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
        ), ( 0x6d, \opcode ds -> simple (if (dsO16 ds) then I_INSW else I_INSD) [] opcode ds
        ), ( 0x6e, simple I_OUTSB []
        ), ( 0x6f, \opcode ds -> simple (if (dsO16 ds) then I_OUTSW else I_OUTSD) [] opcode ds
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
--         ), ( 0xc0, shiftrot opWidth pfx
--         ), ( 0xc1, shiftrot opWidth pfx
--         ), ( 0xc2, do  i <- Immediate 16 <$> fromIntegral <$> getWord16le
--                     simple I_RET pfx [Op_Imm i]
--         ), ( 0xc3, simple I_RET pfx []
--         ), ( 0xc4, fail "invalid"
--         ), ( 0xc5, fail "invalid"
--         ), ( 0xc6, movimm pfx opWidth
--         ), ( 0xc7, movimm pfx opWidth
--         ), ( 0xc8, do  op1 <- fromIntegral <$> getWord16le
--                     op2 <- fromIntegral <$> getWord8
--                     simple I_ENTER pfx [Op_Imm (Immediate 16 op1), Op_Imm (Immediate 8 op2)]
--         ), ( 0xc9, simple I_LEAVE pfx []
--         ), ( 0xca, do op1 <- fromIntegral <$> getWord16le
--                    simple I_RETF pfx [Op_Imm (Immediate 16 op1)]
--         ), ( 0xcb, simple I_RETF pfx []
--         ), ( 0xcc, simple I_INT3 pfx []
--         ), ( 0xcd, do op1 <- fromIntegral <$> getWord8
--                    simple I_INT pfx [Op_Imm (Immediate 8 op1)]
--         ), ( 0xce, fail "invalid"
--         ), ( 0xcf, let i = case opWidth' of 64 -> I_IRETQ; 32 -> I_IRETD; 16 -> I_IRETW
--                     ep = (emitPfx True False pfx)
--                   in return (Instruction ep i [])
--         ), ( 0xd0, shiftrot1 opWidth pfx (Op_Const 1)
--         ), ( 0xd1, shiftrot1 opWidth pfx (Op_Const 1)
--         ), ( 0xd2, shiftrot1 opWidth pfx (Op_Reg (Reg8 RCX HalfL))
--         ), ( 0xd3, shiftrot1 opWidth pfx (Op_Reg (Reg8 RCX HalfL))
--         ), ( 0xd4, fail "invalid"
--         ), ( 0xd5, fail "invalid"
--         ), ( 0xd6, fail "invalid"
--         ), ( 0xd7, simple I_XLATB pfx []
--         ), ( 0xd8, fpu opcode pfx ofs
--         ), ( 0xd9, fpu opcode pfx ofs
--         ), ( 0xda, fpu opcode pfx ofs
--         ), ( 0xdb, fpu opcode pfx ofs
--         ), ( 0xdc, fpu opcode pfx ofs
--         ), ( 0xdd, fpu opcode pfx ofs
--         ), ( 0xde, fpu opcode pfx ofs
--         ), ( 0xdf, fpu opcode pfx ofs
--         ), ( 0xe0, jshort I_LOOPNZ pfx ofs
--         ), ( 0xe1, jshort I_LOOPE pfx ofs
--         ), ( 0xe2, jshort I_LOOP pfx ofs
--         ), ( 0xe3, jshort I_JRCXZ pfx ofs
--         ), ( 0xe4, do op1 <- Op_Imm <$> Immediate 8 <$> fromIntegral <$> getWord8
--                    inout pfx opWidthIO bitD op1
--         ), ( 0xe5, do op1 <- Op_Imm <$> Immediate 8 <$> fromIntegral <$> getWord8
--                    inout pfx opWidthIO bitD op1
--         ), ( 0xe6, do op1 <- Op_Imm <$> Immediate 8 <$> fromIntegral <$> getWord8
--                    inout pfx opWidthIO bitD op1
--         ), ( 0xe7, do op1 <- Op_Imm <$> Immediate 8 <$> fromIntegral <$> getWord8
--                    inout pfx opWidthIO bitD op1
--         ), ( 0xe8, jmpcall I_CALL pfx ofs
--         ), ( 0xe9, jmpcall I_JMP pfx ofs
--         ), ( 0xea, fail "invalid"
--         ), ( 0xeb, jshort I_JMP pfx ofs
--         ), ( 0xec, inout pfx opWidthIO bitD (Op_Reg (Reg16 RDX))
--         ), ( 0xed, inout pfx opWidthIO bitD (Op_Reg (Reg16 RDX))
--         ), ( 0xee, inout pfx opWidthIO bitD (Op_Reg (Reg16 RDX))
--         ), ( 0xef, inout pfx opWidthIO bitD (Op_Reg (Reg16 RDX))
--         ), ( 0xf0, disassemble1' (pfx { pfxLock = True }) ofs
--         ), ( 0xf1, simple I_INT1 pfx []
--         ), ( 0xf2, disassemble1' (pfx { pfxRep = Just PrefixRepNE }) ofs
--         ), ( 0xf3, disassemble1' (pfx { pfxRep = Just PrefixRep }) ofs
--         ), ( 0xf4, simple I_HLT pfx []
--         ), ( 0xf5, simple I_CMC pfx []
--         ), ( 0xf6, grpf6 pfx opWidth
--         ), ( 0xf7, grpf6 pfx opWidth
--         ), ( 0xf8, simple I_CLC pfx []
--         ), ( 0xf9, simple I_STC pfx []
--         ), ( 0xfa, simple I_CLI pfx []
--         ), ( 0xfb, simple I_STI pfx []
--         ), ( 0xfc, simple I_CLD pfx []
--         ), ( 0xfd, simple I_STD pfx []
--         ), ( 0xfe, grpfe pfx opWidth bitW
--         ), ( 0xff, grpfe pfx opWidth bitW
        ) ]


-- this is the long mode (64-bit) disassembler
disassemble1' :: OpcodeMap -> DisassemblerState -> Get Instruction
disassemble1' map ds = do opcode <- getWord8
                          case Map.lookup opcode map of
                              Nothing -> fail "invalid"
                              Just f  -> f opcode ds

--     let bitW = (opcode .&. (bit 0))
--         bitD = (opcode .&. (bit 1))
--         opWidthIO = o' bitW (bitTest 3 (dsRex ds)) (pfxO16 pfx)
--             where
--                 o' 0 _ _       = 8
--                 o' _ _ True    = 16
--                 o' 1 _ False   = 32
--         opWidthPI = o' bitW (bitTest 3 (dsRex ds)) (pfxO16 pfx)
--             where
--                 o' _ _ True    = 16
--                 o' _ _ False   = 32

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
       _ -> fail "invalid"

opcodeMap0f :: OpcodeMap
opcodeMap0f = Map.fromList [
           ( 0x00, grp0f00
        ), ( 0x05, simple I_SYSCALL []
        ), ( 0xc8, bswap
        ), ( 0xc9, bswap
        ), ( 0xca, bswap
        ), ( 0xcb, bswap
        ), ( 0xcc, bswap
        ), ( 0xcd, bswap
        ), ( 0xce, bswap
        ), ( 0xcf, bswap
        ) ]

-- inout pfx opWidth direction op1 = do
--      let op2 = selectreg 0 0 opWidth (Nothing :: Maybe Word8) False
--          (i,ops) = case direction of
--                     0 -> (I_IN, [Op_Reg op2, op1])
--                     _ -> (I_OUT, [op1, Op_Reg op2])
--          ep = emitPfx (opWidth /= 8) False pfx
--        in return (Instruction ep i ops)
--
--
-- grpf6 pfx opWidth = do
--         (rm, _, op, _, _) <- modrm pfx opWidth
--         let ep = emitPfx (opWidth /= 8) True pfx
--             in case op of
--                 0 -> f6test ep rm
--                 1 -> f6test ep rm
--                 2 -> return (Instruction ep I_NOT [rm])
--                 3 -> return (Instruction ep I_NEG [rm])
--                 4 -> return (Instruction ep I_MUL [rm])
--                 5 -> return (Instruction ep I_IMUL [rm])
--                 6 -> return (Instruction ep I_DIV [rm])
--                 7 -> return (Instruction ep I_IDIV [rm])
--     where f6test ep rm = do imm <- (Immediate opWidth) <$> case opWidth of
--                                        8  -> fromIntegral <$> getWord8
--                                        16 -> fromIntegral <$> getWord16le
--                                        32 -> fromIntegral <$> getWord32le
--                                        64 -> fromIntegral <$> getWord64le
--                             return (Instruction ep I_TEST [rm, Op_Imm imm])
--
-- grpfe pfx opWidth bitW = do
--      md <- lookAhead getWord8
--      let op = bits 3 3 md
--          ep = emitPfx (bitW == 1) True pfx
--         in case (bitW,op) of
--             (_,0) -> do (rm, _, op, mod, _) <- modrm pfx opWidth; return (Instruction ep I_INC [rm])
--             (_,1) -> do (rm, _, op, mod, _) <- modrm pfx opWidth; return (Instruction ep I_DEC [rm])
--             (1,2) -> do (rm, _, op, mod, _) <- modrm pfx 64; return (Instruction ep I_CALL [Op_Near rm])
--             (1,3) -> do (rm, _, op, mod, _) <- modrm pfx opWidth; return (Instruction ep I_CALL [Op_Far rm])
--             (1,4) -> do (rm, _, op, mod, _) <- modrm pfx 64; if (mod == 3) then fail "invalid" else return (Instruction ep I_JMP [Op_Near rm])
--             (1,5) -> do (rm, _, op, mod, _) <- modrm pfx 32; return (Instruction ep I_JMP [Op_Far rm])
--             (1,6) -> do (rm, _, op, mod, _) <- modrm pfx opWidth; return (Instruction ep I_PUSH [rm])
--             _     -> fail "invalid"
--
--
--
-- movimm pfx opWidth = do
--         (rm, _, op, _, _) <- modrm pfx opWidth
--         case op of 0 -> do imm <- (Immediate opWidth) <$> case opWidth of
--                                        8  -> fromIntegral <$> getWord8
--                                        16 -> fromIntegral <$> getWord16le
--                                        32 -> fromIntegral <$> getWord32le
--                                        64 -> fromIntegral <$> getWord32le -- FIXME?
--                            let ep = emitPfx (opWidth /= 8) False pfx
--                              in return (Instruction ep I_MOV [rm, Op_Imm imm])
--                    _ -> fail "invalid"
--
-- shiftrot opWidth pfx = do
--         (rm, _, op, _, _) <- modrm pfx opWidth
--         imm <- (Immediate 8 . fromIntegral) <$> getWord8
--         shiftrot' pfx op rm (Op_Imm imm) opWidth
--
-- shiftrot1 opWidth pfx op2 = do
--         (rm, _, op, _, _) <- modrm pfx opWidth
--         shiftrot' pfx op rm op2 opWidth
--
-- shiftrot' pfx op rm op2 opWidth =
--         let i = case op of
--                     0 -> I_ROL
--                     1 -> I_ROR
--                     2 -> I_RCL
--                     3 -> I_RCR
--                     4 -> I_SHL
--                     5 -> I_SHR
--                     6 -> I_SHL
--                     7 -> I_SAR
--             ep = emitPfx (opWidth /= 8) True pfx
--           in return (Instruction ep i [rm, op2])

-- datamov i pfx opl opwidth = let
--         seg = pfxSeg pfx
--         pe' = (maybe [] ((:[]).PrefixSeg) seg)
--         pe = case (i,seg) of
--                     (I_OUTSB, _)       -> []
--                     (I_OUTSW, _)       -> []
--                     (I_OUTSD, _)       -> []
--                     (I_OUTSQ, _)       -> []
--                     (I_INSB, _)        -> []
--                     (I_INSW, _)        -> []
--                     (I_INSD, _)        -> []
--                     (I_INSQ, _)        -> []
--                     (I_CMPSB, _)       -> []
--                     (I_CMPSW, _)       -> []
--                     (I_CMPSD, _)       -> []
--                     (I_CMPSQ, _)       -> []
--                     (I_SCASB, _)       -> []
--                     (I_SCASW, _)       -> []
--                     (I_SCASD, _)       -> []
--                     (I_SCASQ, _)       -> []
--                     (I_MOVSB, Just SS) -> pe'
--                     (I_MOVSW, Just SS) -> pe'
--                     (I_MOVSD, Just SS) -> pe'
--                     (I_MOVSQ, Just SS) -> pe'
--                     (I_LODSB, Just SS) -> pe'
--                     (I_LODSW, Just SS) -> pe'
--                     (I_LODSD, Just SS) -> pe'
--                     (I_LODSQ, Just SS) -> pe'
--                     (I_STOSB, Just SS) -> pe'
--                     (I_STOSW, Just SS) -> pe'
--                     (I_STOSD, Just SS) -> pe'
--                     (I_STOSQ, Just SS) -> pe'
--                     (_,       Just SS) -> []
--                     _                  -> pe'
--         ep = (emitPfx (opwidth /= 8) False pfx) ++ pe
--
--     in return (Instruction ep i opl)
--
-- jmpcall i pfx ofs = let
--         opWidth = case (pfxO16 pfx) of
--                  (True)  -> 16
--                  (False) -> 32
--          in do
--             disp <- case opWidth of
--                     16 -> fromIntegral <$> getInt16le
--                     32 -> fromIntegral <$> getInt32le
--             eip <- ((ofs+).fromIntegral <$> bytesRead)
--             let iv = bits 0 64 (eip + disp)
--                 imm = Immediate 64 iv
--                 ep = (emitPfx True False pfx)
--               in return (Instruction ep i [Op_Jmp imm])
--
-- fpu opcode pfx ofs = do
--             rmb <- lookAhead getWord8
--             let set = bits 0 3 opcode
--                 op = bits 0 3 rmb
--                 opWidth = case (set, op) of (3,0) -> 32; (3,_) -> 80; (4,_) -> 64; (5,_) -> 64; (6,_) -> 16; (7,7) -> 64; (7,5) -> 64; (7,0) -> 16; _ -> 32
--              in do
--                 (rm, _, op, mod, reg) <- modrmFpu pfx opWidth
--                 let ep = (emitPfx False True pfx)
--                     fpureg = (Op_Reg . RegFPU) ([ST0, ST1, ST2, ST3, ST4, ST5, ST6, ST7] !! reg)
--                     r i o = return (Instruction ep i o)
--                     st0 = Op_Reg (RegFPU ST0)
--                     rr i = if mod == 3 then r i [st0, rm] else r i [rm]
--                    in case (set, op, mod) of
--                         (0, 0, _) -> rr I_FADD
--                         (0, 1, _) -> rr I_FMUL
--                         (0, 2, _) -> rr I_FCOM
--                         (0, 3, _) -> rr I_FCOMP
--                         (0, 4, _) -> rr I_FSUB
--                         (0, 5, _) -> rr I_FSUBR
--                         (0, 6, _) -> rr I_FDIV
--                         (0, 7, _) -> rr I_FDIVR
--
--                         (1, 0, _) -> rr I_FLD
--                         (1, 1, _) -> rr I_FXCH
--                         (1, 2, 3) -> if reg == 0 then r I_FNOP [] else fail "invalid"
--                         (1, 2, _) -> rr I_FST
--                         (1, 3, _) -> rr I_FSTP
--                         (1, 3, _) -> rr I_FSTP
--                         (1, 4, _) -> rr I_FLDENV
--                         (1, 5, _) -> rr I_FLDCW
--
--                         (2, 0, 3) -> r I_FCMOVB [st0, fpureg]
--                         (2, 1, 3) -> r I_FCMOVE [st0, fpureg]
--                         (2, 2, 3) -> r I_FCMOVBE [st0, fpureg]
--                         (2, 3, 3) -> r I_FCMOVU [st0, fpureg]
--                         (2, 0, _) -> r I_FIADD [rm]
--                         (2, 1, _) -> r I_FIMUL [rm]
--                         (2, 2, _) -> r I_FICOM [rm]
--                         (2, 3, _) -> r I_FICOMP [rm]
--                         (2, 4, _) -> r I_FISUB [rm]
--                         (2, 5, _) -> r I_FISUBR [rm]
--                         (2, 6, _) -> r I_FIDIV [rm]
--                         (2, 7, _) -> r I_FIDIVR [rm]
--
--                         (3, 0, 3) -> r I_FCMOVNB [st0, fpureg]
--                         (3, 1, 3) -> r I_FCMOVNE [st0, fpureg]
--                         (3, 2, 3) -> r I_FCMOVNBE [st0, fpureg]
--                         (3, 3, 3) -> r I_FCMOVNU [st0, fpureg]
--                         (3, 0, _) -> r I_FILD [rm]
--                         (3, 1, _) -> r I_FISTTP [rm]
--                         (3, 7, _) -> r I_FSTP [rm]
--
--                         (4, 0, _) -> r I_FADD [rm]
--                         (4, 1, _) -> r I_FMUL [rm]
--                         (4, 2, 3) -> r I_FCOM2 [fpureg]
--                         (4, 2, _) -> r I_FCOM [rm]
--                         (4, 3, _) -> r I_FCOMP [rm]
--                         (4, 4, _) -> r I_FSUB [rm]
--                         (4, 5, _) -> r I_FSUBR [rm]
--                         (4, 6, _) -> r I_FDIV [rm]
--                         (4, 7, _) -> r I_FDIVR [rm]
--
--                         (5, 1, _) -> r I_FISTTP [rm]
--                         (5, 2, _) -> r I_FST [rm]
--                         (5, 3, _) -> r I_FSTP [rm]
--                         (5, 4, _) -> r I_FRSTOR [rm]
--                         (5, 0, 0) -> r I_FLD [rm]
--                         (5, 0, 3) -> r I_FFREE [rm]
--
--                         (6, 0, _) -> r I_FIADD [rm]
--                         (6, 1, _) -> r I_FIMUL [rm]
--                         (6, 2, _) -> r I_FICOM [rm]
--                         (6, 3, _) -> r I_FICOMP [rm]
--                         (6, 4, _) -> r I_FISUB [rm]
--                         (6, 5, _) -> r I_FISUBR [rm]
--                         (6, 6, _) -> r I_FIDIV [rm]
--                         (6, 7, _) -> r I_FIDIVR [rm]
--
--                         (7, 0, 3) -> r I_FFREEP [fpureg]
--                         (7, 0, _) -> r I_FILD [rm]
--                         (7, 1, 3) -> r I_FXCH7 [fpureg]
--                         (7, 1, _) -> r I_FISTTP [rm]
--                         (7, 2, 3) -> r I_FSTP8 [fpureg]
--                         (7, 2, _) -> r I_FIST [rm]
--                         (7, 3, 3) -> r I_FSTP9 [fpureg]
--                         (7, 3, _) -> r I_FISTP [rm]
--                         (7, 4, 3) -> case reg of
--                                     0 -> r I_FSTSW [Op_Reg (Reg16 RAX)]
--                                     _ -> fail "invalid"
--                         (7, 4, _) -> r I_FBLD [rm]
--                         (7, 5, 3) -> r I_FUCOMIP [fpureg]
--                         (7, 5, _) -> r I_FILD [rm]
--                         (7, 6, 3) -> r I_FCOMIP [fpureg]
--                         (7, 6, _) -> r I_FBSTP [rm]
--                         (7, 7, 3) -> fail "invalid"
--                         (7, 7, _) -> r I_FISTP [rm]
--                         _         -> fail "invalid"

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

