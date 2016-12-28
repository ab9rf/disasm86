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
import Data.Word (Word8, Word64)
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
      | I_LOOPZ
      | I_LOOP
      | I_JECXZ
      | I_FFREEP
      | I_FXCH7
      | I_FSTP8
      | I_FSTP9
      | I_FSTSW
      | I_FSTDW
      | I_FSTSG
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
      | I_STOSB
      | I_ENTER
      | I_ROL
      | I_ROR
      | I_RCL
      | I_RCR
      | I_SHL
      | I_SHR
      | I_SAL
      | I_SAR
    deriving (Show, Eq)

data Operand =
        Op_Mem {
                mSize  :: Int
              , mReg   :: Register
              , mIdx   :: Register
              , mScale :: Word8
              , mDisp  :: ImmediateS
              , mSeg   :: Maybe SReg
              }
      | Op_Reg Register
      | Op_Imm ImmediateU
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

data GPR = RAX | RCX | RDX | RBX | RSP | RBP | RSI | RDI | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
    deriving (Show, Eq, Ord, Enum)

data SReg = ES | CS | SS | DS | FS | GS
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
        t2 = intercalate ", " (map ot operands)
        tp = concat (map prefixtext p)
        ot = if (isAmbiguousSize oper) then operandtext' else operandtext
      in case t2 of "" -> t1
                    _  -> t1 ++ " " ++ t2

prefixtext PrefixA32 = "a32 "
prefixtext PrefixO16 = "o16 "
prefixtext PrefixRepNE = "repne "
prefixtext PrefixRep = "rep "
prefixtext PrefixLock = "lock "


operandtext :: Operand -> String
operandtext (Op_Reg r) = registertext r
operandtext (Op_Mem sz base idx sf ofs seg) =
    let bs = registertext base
        is = case (idx,sf)  of
                    (RegNone,_) -> ""
                    (_,1)       -> (registertext idx)
                    (_,_)       -> ((registertext idx) ++ "*" ++ (show sf))
        os = case ofs of Immediate 0 _         -> ""
                         Immediate _ 0         -> "0x0"
                         Immediate _ v | v > 0 -> ("0x" ++ (showHex v) "")
                                        | v < 0 -> ("-0x" ++ (showHex (negate v)) "")
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
     in "[" ++ so ++ str ++ "]"
operandtext (Op_Imm i) = immediatetext i
operandtext o = "!operand "++ (show o) ++ "!"

operandtext' o@(Op_Mem sz _ _ _ _ _) =
    (case sz of 8 -> "byte"; 16 -> "word"; 32 -> "long"; _ -> (show sz)) ++ " " ++ operandtext o
operandtext' o = operandtext o

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
        opWidth = o' bitW (fmap (\x -> (x .&. (bit 3)) /= 0) (pfxRex pfx)) (pfxO16 pfx)
            where
                o' _ (Just True)  _       = 64
                o' 0 _ _                  = 8
                o' 1 Nothing      False   = 32
                o' 1 Nothing      True    = 16
                o' 1 (Just False) False   = 32
                o' 1 (Just False) True    = 16
        opWidth' = o' (fmap (\x -> (x .&. (bit 3)) /= 0) (pfxRex pfx)) (pfxO16 pfx)
            where
                o' (Just True)  _       = 64
                o' _            False   = 32
                o' _            True    = 16
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
-- TODO: 0x0f

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

        0x50 -> grp50 I_PUSH (bits 0 3 opcode) opWidth pfx
        0x51 -> grp50 I_PUSH (bits 0 3 opcode) opWidth pfx
        0x52 -> grp50 I_PUSH (bits 0 3 opcode) opWidth pfx
        0x53 -> grp50 I_PUSH (bits 0 3 opcode) opWidth pfx
        0x54 -> grp50 I_PUSH (bits 0 3 opcode) opWidth pfx
        0x55 -> grp50 I_PUSH (bits 0 3 opcode) opWidth pfx
        0x56 -> grp50 I_PUSH (bits 0 3 opcode) opWidth pfx
        0x57 -> grp50 I_PUSH (bits 0 3 opcode) opWidth pfx
        0x58 -> grp50 I_POP (bits 0 3 opcode) opWidth pfx
        0x59 -> grp50 I_POP (bits 0 3 opcode) opWidth pfx
        0x5a -> grp50 I_POP (bits 0 3 opcode) opWidth pfx
        0x5b -> grp50 I_POP (bits 0 3 opcode) opWidth pfx
        0x5c -> grp50 I_POP (bits 0 3 opcode) opWidth pfx
        0x5d -> grp50 I_POP (bits 0 3 opcode) opWidth pfx
        0x5e -> grp50 I_POP (bits 0 3 opcode) opWidth pfx
        0x5f -> grp50 I_POP (bits 0 3 opcode) opWidth pfx

        0x64 -> disassemble1' (pfx { pfxSeg = Just FS }) ofs
        0x65 -> disassemble1' (pfx { pfxSeg = Just GS }) ofs
        0x66 -> disassemble1' (pfx { pfxO16 = True }) ofs
        0x67 -> disassemble1' (pfx { pfxA32 = True }) ofs

        0x99 -> let i = case opWidth' of 64 -> I_CQO; 32 -> I_CDQ; 16 -> I_CWD
                  in simple i pfx []

        0x9b -> simple I_WAIT pfx []

        0xaa -> simple I_STOSB pfx []

        0xc0 -> shiftrot 8 pfx

        0xc8 -> do  op1 <- fromIntegral <$> getWord16le
                    op2 <- fromIntegral <$> getWord8
                    simple I_ENTER pfx [Op_Imm (Immediate 16 op1), Op_Imm (Immediate 8 op2)]

        0xdf -> fpuDF pfx ofs

        0xe0 -> jshort I_LOOPNZ pfx ofs
        0xe1 -> jshort I_LOOPZ pfx ofs
        0xe2 -> jshort I_LOOP pfx ofs
        0xe3 -> jshort I_JECXZ pfx ofs

        0xe8 -> jmpcall I_CALL pfx ofs
        0xe9 -> jmpcall I_JMP pfx ofs

        0xec -> simple I_IN pfx [Op_Reg (Reg8 RAX HalfL), Op_Reg (Reg16 RDX)]

        0xee -> simple I_OUT pfx [Op_Reg (Reg16 RDX), Op_Reg (Reg8 RAX HalfL)]

        0xf0 -> disassemble1' (pfx { pfxLock = True }) ofs
        0xf2 -> disassemble1' (pfx { pfxRep = Just PrefixRepNE }) ofs
        0xf3 -> disassemble1' (pfx { pfxRep = Just PrefixRep }) ofs

        0xf4 -> simple I_HLT pfx []
        0xf5 -> simple I_CMC pfx []
-- TODO: 0xf6
-- TODO: 0xf7
        0xf8 -> simple I_CLC pfx []
        0xf9 -> simple I_STC pfx []
        0xfa -> simple I_CLI pfx []
        0xfb -> simple I_STI pfx []
        0xfc -> simple I_CLD pfx []
        0xfd -> simple I_STD pfx []

        _ -> fail ("invalid opcode " ++ show opcode)

emitPfx noo16 noa32 pfx =
    (if (not noo16) && pfxO16 pfx then [PrefixO16] else []) ++
    (if (not noa32) && pfxA32 pfx then [PrefixA32] else []) ++
    (if pfxLock pfx then [PrefixLock] else []) ++
    (maybe [] (:[]) (pfxRep pfx))

grp50 i r opWidth pfx = let
        reg = selectreg 0 r opWidth (pfxRex pfx)
        ep = emitPfx True False pfx
    in return (Instruction ep i [Op_Reg reg])

op2 i pfx opWidth direction = do
    (rm, reg, _, _, _) <- modrm pfx opWidth
    let ops = case direction of
                    0 -> [rm, Op_Reg reg]
                    _ -> [Op_Reg reg, rm]
        ep = emitPfx True True pfx
      in return (Instruction ep i ops)

shiftrot opWidth pfx = do
        (rm, _, op, _, _) <- modrm pfx opWidth
        imm <- (Immediate 8 . fromIntegral) <$> getWord8
        let i = case op of
                    0 -> I_ROL
                    1 -> I_ROR
                    2 -> I_RCL
                    3 -> I_RCR
                    4 -> I_SHL
                    5 -> I_SHR
                    6 -> I_SAL
                    7 -> I_SAR
            ep = emitPfx False True pfx
          in return (Instruction ep i [rm, Op_Imm imm])

modrm pfx opWidth = do
    modrm <- getWord8
    let b'mod = bits 6 2 modrm
        b'reg = bits 3 3 modrm
        b'rm  = bits 0 3 modrm
        aWidth = if (pfxA32 pfx) then 32 else 64
        reg = selectreg 2 b'reg opWidth (pfxRex pfx)
        hasSib = (b'mod /= 3 && b'rm == 4)
        dispSize = case (b'mod, b'rm) of
            (0,5) -> Just 32
            (1,_) -> Just 8
            (2,_) -> Just 32
            _     -> Nothing
        getDisp sz = case sz of
            Just 8 -> (Immediate 8 . fromIntegral) <$> getInt8
            Just 32 -> (Immediate 32 . fromIntegral) <$> getInt32le
            _  -> return $ Immediate 0 0
        so = (pfxSeg pfx)
      in do
        sib <- if hasSib then (parseSib (pfxRex pfx) aWidth <$> getWord8) else return (RegNone,RegNone,0)
        disp <- getDisp dispSize <|> (return $ Immediate 0 0)
        let rm = case (b'mod, b'rm) of
                (3,_) -> Op_Reg (selectreg 0 b'rm opWidth (pfxRex pfx))
                (0,4) -> let (br, sr, sc) = sib in Op_Mem opWidth br sr sc disp so
                (1,4) -> let (br, sr, sc) = sib in Op_Mem opWidth br sr sc disp so
                (2,4) -> let (br, sr, sc) = sib in Op_Mem opWidth br sr sc disp so
                (_,_) -> Op_Mem opWidth (selectreg 0 b'rm aWidth (pfxRex pfx)) RegNone 0 disp so
          in return (rm, reg, b'reg, b'mod, b'rm)

opImm i pfx opWidth = do
    iv <- case opWidth of
             8 -> fromIntegral <$> getWord8
             32 -> fromIntegral <$> getWord32le
    let reg = case opWidth of
             8 -> Reg8 RAX HalfL
             32 -> (if bitTest 3 (pfxRex pfx) then Reg64 else Reg32) RAX
        imm = Immediate opWidth iv
        ep = (emitPfx False False pfx)
      in return (Instruction ep i [Op_Reg reg, Op_Imm imm])

simple i pfx opl = let
        ep = (emitPfx False False pfx)
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
                ep = (emitPfx False False pfx)
              in return (Instruction ep i [Op_Imm imm])

jshort i pfx ofs = do
            disp <- fromIntegral <$> getInt8
            eip <- ((ofs+).fromIntegral <$> bytesRead)
            let iv = bits 0 64 (eip + disp)
                imm = Immediate 64 iv
                ep = (emitPfx False False pfx)
              in return (Instruction ep i [Op_Imm imm])

fpuDF pfx ofs = do
            (rm, _, op, mod, reg) <- modrm pfx 32
            let ep = (emitPfx False False pfx)
                fpureg = RegFPU ([ST0, ST1, ST2, ST3, ST4, ST5, ST6, ST7] !! reg)
               in case (op, mod) of
                    (0, 3) -> return (Instruction ep I_FFREEP [Op_Reg fpureg])
                    (1, 3) -> return (Instruction ep I_FXCH7 [Op_Reg fpureg])
                    (2, 3) -> return (Instruction ep I_FSTP8 [Op_Reg fpureg])
                    (3, 3) -> return (Instruction ep I_FSTP9 [Op_Reg fpureg])
                    (4, 3) -> case reg of
                                0 -> return (Instruction ep I_FSTSW [Op_Reg (Reg16 RAX)])
                                1 -> return (Instruction ep I_FSTDW [Op_Reg (Reg16 RAX)])
                                2 -> return (Instruction ep I_FSTSG [Op_Reg (Reg16 RAX)])
                                3 -> fail "invalid"
                    (5, 3) -> return (Instruction ep I_FUCOMIP [Op_Reg fpureg])
                    (6, 3) -> return (Instruction ep I_FCOMIP [Op_Reg fpureg])
                    (7, 3) -> fail "invalid"
                    (0, _) -> return (Instruction ep I_FILD [rm])
                    (1, _) -> return (Instruction ep I_FISTTP [rm])
                    (2, _) -> return (Instruction ep I_FIST [rm])
                    (3, _) -> return (Instruction ep I_FISTP [rm])
                    (4, _) -> return (Instruction ep I_FBLD [rm])
                    (5, _) -> return (Instruction ep I_FILD [rm])
                    (6, _) -> return (Instruction ep I_FBSTP [rm])
                    (7, _) -> return (Instruction ep I_FISTP [rm])

parseSib rex aw sib = let
                     br = bits 0 3 sib
                     ir = bits 3 3 sib
                     ss = bits 6 2 sib
                     breg = selectreg 0 br aw rex
                     ireg = selectreg 1 ir aw rex
                     sf = case ss of { 0 -> 1; 1 -> 2; 2 -> 4; 3 -> 8 }
                    in (breg, ireg, sf)

selectreg rexBit reg opWidth rex = let
                rvec' = case () of
                        _ | bitTest rexBit rex ->
                                [R8, R9, R10, R11, R12, R13, R14, R15]
                          | otherwise ->
                                [RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI]
                rvec = case opWidth of
                        8 | isNothing rex ->
                                [Reg8 RAX HalfL, Reg8 RCX HalfL, Reg8 RDX HalfL, Reg8 RBX HalfL,
                                 Reg8 RAX HalfH, Reg8 RCX HalfH, Reg8 RDX HalfH, Reg8 RBX HalfH]
                          | isJust rex -> map (\i -> Reg8 i HalfL) rvec'
                        16 -> map Reg16 rvec'
                        32 -> map Reg32 rvec'
                        64 -> map Reg64 rvec'
            in rvec !! reg



--

isAmbiguousSize :: Operation -> Bool
isAmbiguousSize I_ROL = True
isAmbiguousSize I_ROR = True
isAmbiguousSize I_RCL = True
isAmbiguousSize I_RCR = True
isAmbiguousSize I_SHL = True
isAmbiguousSize I_SHR = True
isAmbiguousSize I_SAL = True
isAmbiguousSize I_SAR = True
isAmbiguousSize _     = False

opertext :: Operation -> String
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
opertext I_LOOPZ = "loopz"
opertext I_LOOP = "loop"
opertext I_JECXZ = "jecxz"
opertext I_FFREEP = "ffreep"
opertext I_FXCH7 = "fxch7"
opertext I_FSTP8 = "fstp8"
opertext I_FSTP9 = "fstp9"
opertext I_FSTSW = "fstsw"
opertext I_FSTDW = "fstdw"
opertext I_FSTSG = "fstsg"
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
opertext I_STOSB = "stosb"
opertext I_ENTER = "enter"
opertext I_ROL = "rol"
opertext I_ROR = "ror"
opertext I_RCL = "rcl"
opertext I_RCR = "rcr"
opertext I_SHL = "shl"
opertext I_SHR = "shr"
opertext I_SAL = "sal"
opertext I_SAR = "sar"

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

registertext r = "!reg:" ++ (show r) ++ "!"
