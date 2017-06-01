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
import Data.Maybe (isJust, isNothing, fromJust, listToMaybe, mapMaybe, catMaybes, fromMaybe)
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
}

dsInitial ofs = DisassemblerState { dsOffset = ofs,
                                    dsLock = False, dsRepNE = False, dsRep = False,
                                    dsSegOverride = Nothing,
                                    dsRex = Nothing,
                                    dsOpWidthOverride = False,
                                    dsAdWidthOverride = False,
                                    dsOpWidth = 32, dsAdWidth = 32,
                                    dsModRM = Nothing, dsImmed = Nothing
                                  }

adv :: Int -> Disassembler ()
adv n = modify (\x -> x { dsOffset = (dsOffset x) + (fromIntegral n) })

mask :: Word8 -> Word8 -> Disassembler Word8
mask m v = (lift $ satisfy (\x -> (x .&. m) == v)) <* adv 1

opcode :: Word8 -> Disassembler Word8
opcode v = (lift $ word8 v) <* adv 1

disassemble1' :: Disassembler Instruction
disassemble1' = choice [
            option () (many' segover >> pure ()) >> option () (mask 0xf1 0x40 >> pure()) >> opcode 0x90 >> instr "nop" []
          , general
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
          do i <- mask 0xc7 0x00; opWidthB; modrm; instr (ext1A i) [modrm_rm, modrm_reg]
        , do i <- mask 0xc7 0x01; opWidthW; modrm; instr (ext1A i) [modrm_rm, modrm_reg]
        , do i <- mask 0xc7 0x02; opWidthB; modrm; instr (ext1A i) [modrm_reg, modrm_rm]
        , do i <- mask 0xc7 0x03; opWidthW; modrm; instr (ext1A i) [modrm_reg, modrm_rm]
        , do i <- mask 0xc7 0x04; opWidthB;   imm; instr (ext1A i) [accum, immed]
        , do i <- mask 0xc7 0x05; opWidthW;   imm; instr (ext1A i) [accum, immed]
        , do r <- mask 0xf8 0x50; opWidthX 64 64 16; instr "push" [reg (r .&. 0x07) id]
        , do r <- mask 0xf8 0x58; opWidthX 64 64 16; instr "pop" [reg (r .&. 0x07) id]
        , do opcode 0x80; opWidthB; modrm; i <- modopcode; imm; instr (ext1A i) [modrm_rm, immed]
        , opcode 0x63 >> opWidthW >> modrm >> instr "movsxd" [modrm_reg, modrm_rm]
        , opcode 0x6a >> opWidthB >> imm >> instr "push" [immed]
        , opcode 0x6c >> instr "insb" []
        , opcode 0x6d >> instr "insd" []
        , opcode 0x6e >> instr "outsb" []
        , opcode 0x6f >> instr "outsd" []
        , do i <- mask 0xf0 0x70; d <- displ; instr (shortjmp i) [pure d]
        , do r <- mask 0xf8 0x90; instr "xchg" [reg (r .&. 0x07) id, accum]
        , opcode 0x98 >> instr "cwde" []
        , opcode 0x99 >> instr "cdq" []
        , opcode 0x9b >> instr "wait" []
        , opcode 0x9c >> instr "pushfq" []
        , opcode 0x9d >> instr "popfq" []
        , opcode 0x9e >> instr "sahf" []
        , opcode 0x9f >> instr "lahf" []
        , opcode 0xa4 >> instr "movsb" []
        , opcode 0xa5 >> instr "movsd" []
        , opcode 0xa6 >> instr "cmpsb" []
        , opcode 0xa7 >> instr "cmpsd" []
        , opcode 0xaa >> instr "stosb" []
        , opcode 0xab >> instr "stosd" []
        , opcode 0xac >> instr "lodsb" []
        , opcode 0xad >> instr "lodsd" []
        , opcode 0xae >> instr "scasb" []
        , opcode 0xaf >> instr "scasd" []
        , opcode 0xc3 >> instr "ret" []
        , opcode 0xc9 >> instr "leave" []
        , opcode 0xcb >> instr "retf" []
        , opcode 0xcc >> instr "int3" []
        , opcode 0xcf >> forkX (instr "iretw" []) (instr "iretd" []) (instr "iretq" [])
        , opcode 0x84 >> opWidthB >> modrm >> instr "test" [modrm_rm, modrm_reg]
        , opcode 0x85 >> opWidthW >> modrm >> instr "test" [modrm_rm, modrm_reg]
        , opcode 0x86 >> opWidthB >> modrm >> instr "xchg" [modrm_rm, modrm_reg]
        , opcode 0x87 >> opWidthW >> modrm >> instr "xchg" [modrm_rm, modrm_reg]
        , opcode 0xa8 >> opWidthB >> imm >> instr "test" [accum, immed]
        , opcode 0xa9 >> opWidthW >> imm >> instr "test" [accum, immed]
        , do r <- mask 0xf8 0xb0; opWidthB; imm; instr "mov" [reg (r .&. 0x07) id, immed]
        , opcode 0x68 >> opWidthW >> imm >> instr "push" [immed]
        , opcode 0xcd >> opWidthB >> imm >> instr "int" [immed]
        , opcode 0xec >> opWidthB >> instr "in" [accum, pure (Op_Reg (Reg16 RDX))]
        , do opcode 0xd0; opWidthB; modrm; i <- ext2A; instr i [modrm_rm, pure (Op_Imm (Immediate 8 1))]
        , do opcode 0xd1; opWidthW; modrm; i <- ext2A; instr i [modrm_rm, pure (Op_Imm (Immediate 8 1))]
        , do opcode 0xd2; opWidthB; modrm; i <- ext2A; instr i [modrm_rm, pure (Op_Reg (Reg8 RCX HalfL))]
        , do opcode 0xd3; opWidthW; modrm; i <- ext2A; instr i [modrm_rm, pure (Op_Reg (Reg8 RCX HalfL))]
        , opcode 0xd7 >> instr "xlatb" []
        , opcode 0xec >> opWidthB >> instr "in" [accum, pure (Op_Reg (Reg16 RDX))]
        , opcode 0xed >> opWidthW >> instr "in" [accum, pure (Op_Reg (Reg16 RDX))]
        , opcode 0xee >> opWidthB >> instr "out" [pure (Op_Reg (Reg16 RDX)), accum]
        , opcode 0xef >> opWidthW >> instr "out" [pure (Op_Reg (Reg16 RDX)), accum]
        , opcode 0xf1 >> instr "int1" []
     ]

ext1A i = ["add", "or", "adc", "sbb", "and", "sub", "xor", "cmp"] !! (fromIntegral (bits 3 3 i))
shortjmp i = ["jo", "jno", "jb", "jae", "jz", "jnz", "jbe", "ja", "js", "jns", "jp", "jnp", "jl", "jge", "jle", "jg"]
                        !! (fromIntegral (bits 0 4 i))

ext2A = do i <- modopcode
           case i of 0 -> pure "rol"
                     1 -> pure "ror"
                     2 -> pure "rcl"
                     3 -> pure "rcr"
                     4 -> pure "shl"
                     5 -> pure "shr"
                     6 -> fail "invalid"
                     7 -> pure "sar"

opWidthB = opWidthX 8 8 8
opWidthW = opWidthX 64 32 16

forkX q d w = do o16 <- dsO16
                 rexW <- dsRexW
                 case (o16, rexW) of (_, 1) -> q; (True, 0) -> w; (False, 0) -> d

opWidthX q d w = do o16 <- dsO16
                    rexW <- dsRexW
                    let ow = case (o16, rexW) of (_, 1) -> q; (True, 0) -> w; (False, 0) -> d
                      in modify (\x -> x { dsOpWidth = ow })


adWidth n1 n2 = dsA32 >>= (\f -> modify (\x -> x { dsAdWidth = if f then n1 else n2 } ) )

instr :: String -> [Disassembler Operand] -> Disassembler Instruction
instr i ops = Instruction <$> pfx <*> pure (Operation i) <*> sequence ops

readBytes :: Int -> Disassembler ByteString
readBytes n = (lift $ (B.fromStrict <$> A.take n)) <* adv n

modopcode :: Disassembler Word8
modopcode = (gets dsModRM) >>= pure . modRM_breg . fromJust

imm :: Disassembler ()
imm = do
    ow <- gets dsOpWidth
    case ow of  8 ->  (readBytes 1) >>= pure . (\s -> fromIntegral (G.runGet G.getWord8 (s)))
                16 -> (readBytes 2) >>= pure . (\s -> fromIntegral (G.runGet G.getWord16le (s)))
                32 -> (readBytes 4) >>= pure . (\s -> fromIntegral (G.runGet G.getWord32le (s)))
                64 -> (readBytes 4) >>= pure . (\s -> fromIntegral (G.runGet G.getWord32le (s)))
      >>= \i -> modify (\x -> x { dsImmed = Just $ Op_Imm (Immediate ow (i)) } )

displ :: Disassembler Operand
displ = do  lock <- gets dsLock
            (dispSize, disp) <-
                case lock of False -> do s <- (readBytes 1); pure (8,  fromIntegral (G.runGet G.getInt8 (s)))
                             True  -> do s <- (readBytes 4); pure (32, fromIntegral (G.runGet G.getInt32le (s)))
            eip <- gets dsOffset
            let iv = bits 0 64 (eip + disp)
                imm = Immediate 64 iv
              in pure (Op_Jmp imm)

pfx :: Disassembler [Prefix]
pfx = pure []

modrm_rm :: Disassembler Operand
modrm_rm = (gets dsModRM) >>= pure . modRM_rm . fromJust

modrm_reg :: Disassembler Operand
modrm_reg = (gets dsModRM) >>= pure . Op_Reg . modRM_reg . fromJust

immed :: Disassembler Operand
immed = (gets dsImmed) >>= pure . fromJust

reg :: Word8 -> (Int -> Int) -> Disassembler Operand
reg rr ov = do
    ow <- gets dsOpWidth
    rex <- gets dsRex
    rexB <- dsRexB
    let r = selectreg rr (ov ow) rexB rex False
      in pure $ Op_Reg r

accum :: Disassembler Operand
accum = do
    ow <- gets dsOpWidth
    rex <- gets dsRex
    let r = selectreg 0 ow 0 Nothing False
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
    aWidth   <- dsA32 >>= pure . (\x -> if x then 32 :: Int else 64 :: Int)
    rex      <- gets dsRex
    rexR     <- dsRexR
    rexX     <- dsRexX
    rexB     <- dsRexB
    opWidth  <- gets dsOpWidth
    fpu      <- pure False
    so       <- dsSeg
    let b'mod = bits 6 2 val
        b'reg = bits 3 3 val
        b'rm  = bits 0 3 val
        reg = selectreg b'reg opWidth rexR rex fpu
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
                (3,_) -> Op_Reg (selectreg b'rm opWidth rexB rex fpu)
                (0,5) -> Op_Mem opWidth aWidth ((if aWidth == 64 then Reg64 else Reg32) RIP) RegNone 0 disp so
                (0,4) -> let (br, ir, sc) = sib in Op_Mem opWidth aWidth br ir sc disp so
                (1,4) -> let (br, ir, sc) = sib in Op_Mem opWidth aWidth br ir sc disp so
                (2,4) -> let (br, ir, sc) = sib in Op_Mem opWidth aWidth br ir sc disp so
                (_,_) -> Op_Mem opWidth aWidth (selectreg b'rm aWidth rexB rex False) RegNone 0 disp so
          in
            modify $ \x -> x { dsModRM = Just $ ModRM rm reg b'reg b'mod b'rm }
    where
        getWord8   = (lift $ anyWord8) <* adv 1
        getInt8    = (readBytes 1) >>= \s -> pure (G.runGet G.getInt8 (s))
        getInt32le = (readBytes 4) >>= \s -> pure (G.runGet G.getInt32le (s))

        parseSib m dispSize rex rexB rexX aw sib = let
                                 br = (bits 0 3 sib)
                                 ir = (bits 3 3 sib)
                                 ss = (bits 6 2 sib)
                                 sp = (case aw of 16 -> Reg16; 32 -> Reg32; 64 -> Reg64) RSP
                                 breg = selectreg br aw rexB rex False
                                 ireg = selectreg ir aw rexX rex False
                                 sf = case ss of { 0 -> 1; 1 -> 2; 2 -> 4; 3 -> 8 }
                            in case (m, br) of (0, 5) -> ((RegNone, if ireg == sp then RegNone else ireg, sf), Just 32)
                                               _      -> ((breg, if ireg == sp then RegNone else ireg, sf), dispSize)

selectreg :: Word8 -> Int -> Word8 -> Maybe Word8 -> Bool -> Register
selectreg reg opWidth rex rex' fpu = let
                rvec' = case rex of
                        1 -> [R8, R9, R10, R11, R12, R13, R14, R15]
                        0 -> [RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI]
                rvec = case (fpu, opWidth, rex') of
                        (True, _, _) -> map RegFPU [ST0, ST1, ST2, ST3, ST4, ST5, ST6, ST7]
                        (False, 8, Just _)  -> map (\i -> Reg8 i HalfL) rvec'
                        (False, 8, Nothing) -> [Reg8 RAX HalfL, Reg8 RCX HalfL, Reg8 RDX HalfL, Reg8 RBX HalfL,
                                                Reg8 RAX HalfH, Reg8 RCX HalfH, Reg8 RDX HalfH, Reg8 RBX HalfH]
                        (False, 16, _) -> map Reg16 rvec'
                        (False, 32, _) -> map Reg32 rvec'
                        (False, 64, _) -> map Reg64 rvec'
            in rvec !! (fromIntegral reg)

bits :: (Integral a, Bits a) => Int-> Int -> a -> a
bits s l i = fromIntegral $ (i `shiftR` s) .&. ((1 `shiftL` l) - 1)

bitTest :: Int -> Maybe Prefix -> Bool
bitTest i v = case v of
                Nothing -> False
                Just (PrefixRex n) -> n .&. (bit i) /= 0


data ModRM = ModRM {
        modRM_rm :: Operand
      , modRM_reg :: Register
      , modRM_breg :: Word8
      , modRM_mod :: Word8
      , modRM_brm :: Word8
      }


--
-- prefixMap = Map.fromList ([ (0x26, PrefixSeg ES), (0x2e, PrefixSeg CS), (0x36, PrefixSeg SS), (0x3e, PrefixSeg DS),
--                            (0x64, PrefixSeg FS), (0x65, PrefixSeg GS), (0x66, PrefixO16), (0x67, PrefixA32),
--                            (0xF0, PrefixLock), (0xF2, PrefixRepNE), (0xF3, PrefixRep)
--                           ] ++ [ (x, PrefixRex x) | x <- [0x40..0x4f] ])
--
-- dsRex :: DisassemblerState -> Maybe Prefix
-- dsRex ds = find (\x -> case x of PrefixRex _ -> True; _ -> False) (dsPfx ds)
-- dsO16 ds = PrefixO16 `elem` (dsPfx ds)
-- dsA32 ds = PrefixA32 `elem` (dsPfx ds)
-- dsRep ds = PrefixRep `elem` (dsPfx ds)
-- dsSeg ds = listToMaybe (catMaybes (map (\p -> case p of (PrefixSeg s) -> Just s; _ -> Nothing) (dsPfx ds)))
--
-- opOpWidth :: Word8 -> DisassemblerState -> Int
-- opOpWidth o ds =
--         o' (o .&. (bit 0)) (bitTest 3 (dsRex ds)) (dsO16 ds)
--             where
--                 o' 0 _     _     = 8
--                 o' _ False True  = 16
--                 o' 1 False False = 32
--                 o' _ True  _     = 64
--
-- opOpWidth' o ds =
--         o' (bitTest 3 (dsRex ds)) (dsO16 ds)
--             where
--                 o' True _       = 64
--                 o' _    False   = 32
--                 o' _    True    = 16
--
-- opOpWidthA o ds = o' (bitTest 3 (dsRex ds)) (dsO16 ds)
--             where
--                 o' _ False   = 64
--                 o' _ True    = 16
-- --
--
-- bits s l i = fromIntegral $ (i `shiftR` s) .&. ((1 `shiftL` l) - 1)
--
-- bitTest :: Int -> Maybe Prefix -> Bool
-- bitTest i v = case v of
--                 Nothing -> False
--                 Just (PrefixRex n) -> n .&. (bit i) /= 0
--
-- fetchImm opWidth = Op_Imm <$> (case opWidth of 8  -> Immediate 8 . fromIntegral <$> getWord8
--                                                16 -> Immediate 16 . fromIntegral <$> getWord16le
--                                                32 -> Immediate 32 . fromIntegral <$> getWord32le
--                                                64 -> Immediate 64 . fromIntegral <$> getWord32le)
--
-- --
--
-- op2 :: Operation -> DisassemblerSingle
-- op2 i opcode ds = op2aux (opOpWidth opcode ds) (bits 1 1 opcode) i ds
--
-- op2xt :: Operation -> DisassemblerSingle
-- op2xt i opcode ds = op2aux (opOpWidth opcode ds) 0 i ds
--
-- op2aux opWidth direction i ds = do
--         (rm, reg, _, _, _) <- modrm ds opWidth
--         let ops = case direction of
--                         0 -> [rm, Op_Reg reg]
--                         _ -> [Op_Reg reg, rm]
--             ep = dsPfx ds
--           in return (Instruction ep i ops)
--
-- opImm :: Operation -> DisassemblerSingle
-- opImm i opcode ds = let
--         pfx = dsPfx ds
--         opWidth = opOpWidth opcode ds
--     in do
--         imm <- fetchImm opWidth
--         let reg = case opWidth of
--                  8 -> Reg8 RAX HalfL
--                  16 -> Reg16 RAX
--                  32 -> (if bitTest 3 (dsRex ds) then Reg64 else Reg32) RAX
--                  64 -> Reg64 RAX
--             ep = pfx
--           in return (Instruction ep i [Op_Reg reg, imm])
--
-- grp50 :: Operation -> DisassemblerSingle
-- grp50 i opcode ds = let
--         r = bits 0 3 opcode
--         opWidth = opOpWidthA opcode ds
--         pfx = dsPfx ds
--         reg = selectreg 0 r opWidth (dsRex ds) False
--         ep = pfx
--     in return (Instruction ep i [Op_Reg reg])
--
-- movsxd :: DisassemblerSingle
-- movsxd opcode ds = let pfx = dsPfx ds
--                        o16 = dsO16 ds
--                        rexW = bitTest 3 (dsRex ds)
--                        (o1w, o2w) = case (o16, rexW) of
--                                     (False, False) -> (32, 32)
--                                     (True,  False) -> (16, 32)
--                                     (False, True)  -> (64, 32)
--                                     (True,  True)  -> (64, 32)
--     in do (rm, reg, _, _, _) <- modrm' ds o2w o1w False
--           let ops = [Op_Reg reg, rm]
--               ep = pfx
--             in return (Instruction ep (Operation "movsxd") ops)
--
-- pushImm :: DisassemblerSingle
-- pushImm opcode ds = let pfx = dsPfx ds; opWidth = case (dsO16 ds, opcode) of (_, 0x6a) -> 8; (True, 0x68) -> 16; (False, 0x68) -> 32 in do
--     imm <- fetchImm opWidth
--     let ep = pfx
--       in return (Instruction ep (Operation "push") [imm])
--
-- imul3 :: DisassemblerSingle
-- imul3 opcode ds = let pfx = dsPfx ds; opWidth = opOpWidth' opcode ds; immSize = case opcode of 0x69 -> opWidth; 0x6b -> 8 in do
--     (rm, reg, _, _, _) <- modrm ds opWidth
--     imm <- fetchImm immSize
--     let ep = pfx
--       in return (Instruction ep (Operation "imul") [Op_Reg reg, rm, imm])
--
-- simple :: Operation -> [Operand] -> DisassemblerSingle
-- simple i opl opcode ds = let
--         pfx = dsPfx ds
--         ep = pfx
--     in return (Instruction ep i opl)
--
-- jshort :: Operation -> DisassemblerSingle
-- jshort i opcode ds = do
--             disp <- fromIntegral <$> getInt8
--             eip <- (((dsOffset ds)+).fromIntegral <$> bytesRead)
--             let iv = bits 0 64 (eip + disp)
--                 imm = Immediate 64 iv
--                 ep = dsPfx ds
--               in return (Instruction ep i [Op_Jmp imm])
--
-- grp80 :: DisassemblerSingle
-- grp80 opcode ds = let
--         (opWidth, immSize) = case opcode of 0x80 -> (o, 8); 0x81 -> (o, o); 0x83 -> (o, 8)
--         o = opOpWidth opcode ds
--     in do
--         (rm, _, op, _, _) <- modrm ds opWidth
--         imm <- fetchImm immSize
--         let i = case op of
--                     0 -> (Operation "add")
--                     1 -> (Operation "or")
--                     2 -> (Operation "adc")
--                     3 -> (Operation "sbb")
--                     4 -> (Operation "and")
--                     5 -> (Operation "sub")
--                     6 -> (Operation "xor")
--                     7 -> (Operation "cmp")
--             ep = dsPfx ds
--           in return (Instruction ep i [rm, imm])
--
-- movsr opcode ds = do
--     (rm, _, sr, _, _) <- modrm ds (if dsO16 ds then 16 else 32)
--     let sreg = RegSeg ( [ES, CS, SS, DS, FS, GS, SR6, SR7] !! sr )
--         ops = case (bits 1 1 opcode) of
--                     0 -> [rm, Op_Reg sreg]
--                     _ -> [Op_Reg sreg, rm]
--         ep = dsPfx ds
--       in return (Instruction ep (Operation "mov") ops)
--
-- lea opcode ds = do
--     (rm, reg, _, mod, _) <- modrm' ds 64 (opOpWidth opcode ds) False
--     if mod == 3 then fail "invalid" else
--         let ops = [Op_Reg reg, rm]
--             ep = dsPfx ds
--           in return (Instruction ep (Operation "lea") ops)
--
-- pushpop i opcode ds = do
--     (rm, _, op, _, _) <- modrm ds 64
--     case op of 0 -> return (Instruction (dsPfx ds) i [rm])
--                _ -> fail "invalid"
--
-- xchg opcode ds = let
--         opWidth = opOpWidth' opcode ds
--         reg1 = selectreg 0 (bits 0 3 opcode) opWidth (dsRex ds) False
--         reg2 = selectreg 0 0 opWidth (Nothing :: Maybe Prefix) False
--     in if (reg1 == reg2)
--          then return (Instruction (dsPfx ds) (if (dsRep ds) then (Operation "pause") else (Operation "nop")) [])
--          else return (Instruction (dsPfx ds) (Operation "xchg") [Op_Reg reg1, Op_Reg reg2])
--
-- sized i16 i32 i64 opcode ds =
--     let i = case opOpWidth' opcode ds of 64 -> i64; 32 -> i32; 16 -> i16
--     in return (Instruction (dsPfx ds) i [])
--
-- sized' i16 i32 opcode ds = let r i = return (Instruction (dsPfx ds) i [])
--     in case opOpWidth' opcode ds of 32 -> r i32; 16 -> r i16; _ -> fail "invalid"
--
-- movaddr opcode ds = let
--     aWidth = case (dsA32 ds) of
--                  (True)  -> 32
--                  (False) -> 64
--     opWidth = opOpWidth opcode ds
--     direction = bits 1 1 opcode
--      in do
--         disp <- case aWidth of
--                 64 -> fromIntegral <$> getWord64le
--                 32 -> fromIntegral <$> getWord32le
--         let imm = Op_Mem opWidth aWidth RegNone RegNone 0 (Immediate aWidth disp) (dsSeg ds)
--             ep = dsPfx ds
--             reg = selectreg 0 0 opWidth (Nothing :: Maybe Prefix) False
--             ops = case direction of
--                                 0 -> [Op_Reg reg, imm]
--                                 _ -> [imm, Op_Reg reg]
--           in return (Instruction ep (Operation "mov") ops)
--
-- testRax opcode ds = let opWidth = opOpWidth opcode ds in do
--     imm <- fetchImm opWidth
--     let reg = selectreg 0 0 opWidth (Nothing :: Maybe Prefix) False
--         ep = dsPfx ds
--       in return (Instruction ep (Operation "test") [Op_Reg reg, imm])
--
-- movreg opcode ds = let
--         r = bits 0 3 opcode
--         opWidth = case (bits 3 1 opcode) of 0 -> 8; _ -> opOpWidth' opcode ds
--         reg = selectreg 0 r opWidth (dsRex ds) False
--         ep = dsPfx ds
--     in do
--         imm <- (Immediate opWidth) <$> case opWidth of 8  -> fromIntegral <$> getWord8
--                                                        16 -> fromIntegral <$> getWord16le
--                                                        32 -> fromIntegral <$> getWord32le
--                                                        64 -> fromIntegral <$> getWord64le
--         return (Instruction ep (Operation "mov") [Op_Reg reg, Op_Imm imm])
--
-- shiftrot opcode ds = let opWidth = opOpWidth opcode ds
--     in do
--         (rm, _, op, _, _) <- modrm ds opWidth
--         imm <- (Immediate 8 . fromIntegral) <$> getWord8
--         shiftrot' ds op rm (Op_Imm imm) opWidth
--
-- shiftrot1 op2 opcode ds = let opWidth = opOpWidth opcode ds
--     in do
--         (rm, _, op, _, _) <- modrm ds opWidth
--         shiftrot' ds op rm op2 opWidth
--
-- shiftrot' ds op rm op2 opWidth =
--         let i = case op of
--                     0 -> (Operation "rol")
--                     1 -> (Operation "ror")
--                     2 -> (Operation "rcl")
--                     3 -> (Operation "rcr")
--                     4 -> (Operation "shl")
--                     5 -> (Operation "shr")
--                     6 -> (Operation "shl")
--                     7 -> (Operation "sar")
--             ep = dsPfx ds
--           in return (Instruction ep i [rm, op2])
--
-- ret opcode ds = do
--     i <- Immediate 16 <$> fromIntegral <$> getWord16le
--     simple (Operation "ret") [Op_Imm i] opcode ds
--
-- retf opcode ds = do
--     i <- Immediate 16 <$> fromIntegral <$> getWord16le
--     simple (Operation "retf") [Op_Imm i] opcode ds
--
-- movimm opcode ds = let opWidth = opOpWidth opcode ds
--     in do
--         (rm, _, op, _, _) <- modrm ds opWidth
--         case op of 0 -> do imm <- fetchImm opWidth
--                            return (Instruction (dsPfx ds) (Operation "mov") [rm, imm])
--                    _ -> fail "invalid"
--
-- enter opcode ds = do
--     op1 <- Immediate 16 <$> fromIntegral <$> getWord16le
--     op2 <- Immediate 8 <$> fromIntegral <$> getWord8
--     simple (Operation "enter") [Op_Imm op1, Op_Imm op2] opcode ds
--
-- int opcode ds = do
--     op1 <- Immediate 8 <$> fromIntegral <$> getWord8
--     simple (Operation "int") [Op_Imm op1] opcode ds
--
-- inout o opcode ds = let
--     opWidth = o' (bits 0 1 opcode) (bitTest 3 (dsRex ds)) (dsO16 ds)
--                   where
--                       o' 0 _ _       = 8
--                       o' _ _ True    = 16
--                       o' 1 _ False   = 32
--     direction = bits 1 1 opcode
--     op2 = selectreg 0 0 opWidth (Nothing :: Maybe Prefix) False
--     in do
--         op1 <- case o of Nothing -> Op_Imm <$> Immediate 8 <$> fromIntegral <$> getWord8
--                          Just o' -> return o'
--         let
--             (i,ops) = case direction of
--                        0 -> ((Operation "in"), [Op_Reg op2, op1])
--                        _ -> ((Operation "out"), [op1, Op_Reg op2])
--             ep = dsPfx ds
--           in return (Instruction ep i ops)
--
-- jmpcall i opcode ds = let
--         opWidth = case (dsO16 ds) of
--                  (True)  -> 16
--                  (False) -> 32
--         ofs = dsOffset ds
--          in do
--             disp <- case opWidth of
--                     16 -> fromIntegral <$> getInt16le
--                     32 -> fromIntegral <$> getInt32le
--             eip <- ((ofs+).fromIntegral <$> bytesRead)
--             let iv = bits 0 64 (eip + disp)
--                 imm = Immediate 64 iv
--                 ep = dsPfx ds
--               in return (Instruction ep i [Op_Jmp imm])
--
-- grpf6 opcode ds = let opWidth = opOpWidth opcode ds
--     in do
--         (rm, _, op, _, _) <- modrm ds opWidth
--         let ep = dsPfx ds
--             f6test ep rm = do imm <- (Immediate opWidth) <$> case opWidth of
--                                                        8  -> fromIntegral <$> getWord8
--                                                        16 -> fromIntegral <$> getWord16le
--                                                        32 -> fromIntegral <$> getWord32le
--                                                        64 -> fromIntegral <$> getWord64le
--                               return (Instruction ep (Operation "test") [rm, Op_Imm imm])
--             in case op of
--                 0 -> f6test ep rm
--                 1 -> f6test ep rm
--                 2 -> return (Instruction ep (Operation "not") [rm])
--                 3 -> return (Instruction ep (Operation "neg") [rm])
--                 4 -> return (Instruction ep (Operation "mul") [rm])
--                 5 -> return (Instruction ep (Operation "imul") [rm])
--                 6 -> return (Instruction ep (Operation "div") [rm])
--                 7 -> return (Instruction ep (Operation "idiv") [rm])
--
-- grpfe opcode ds = let bitW = bits 0 1 opcode
--                       opWidth = opOpWidth opcode ds
--                     in do
--                          md <- lookAhead getWord8
--                          let op = bits 3 3 md
--                              ep = dsPfx ds
--                             in case (bitW,op) of
--                                 (_,0) -> do (rm, _, op, mod, _) <- modrm ds opWidth; return (Instruction ep (Operation "inc") [rm])
--                                 (_,1) -> do (rm, _, op, mod, _) <- modrm ds opWidth; return (Instruction ep (Operation "dec") [rm])
--                                 (1,2) -> do (rm, _, op, mod, _) <- modrm ds opWidth; return (Instruction ep (Operation "call") [Op_Near rm])
--                                 (1,3) -> do (rm, _, op, mod, _) <- modrm ds opWidth; return (Instruction ep (Operation "call") [Op_Far rm])
--                                 (1,4) -> do (rm, _, op, mod, _) <- modrm ds 64; if (mod == 3) then fail "invalid" else return (Instruction ep (Operation "jmp") [Op_Near rm])
--                                 (1,5) -> do (rm, _, op, mod, _) <- modrm ds 32; return (Instruction ep (Operation "jmp") [Op_Far rm])
--                                 (1,6) -> do (rm, _, op, mod, _) <- modrm ds opWidth; return (Instruction ep (Operation "push") [rm])
--                                 _     -> fail "invalid"
--
--
-- fpu opcode ds = do
--             rmb <- lookAhead getWord8
--             let set = bits 0 3 opcode
--                 op = bits 0 3 rmb
--                 opWidth = case (set, op) of (3,0) -> 32; (3,1) -> 32; (3,2) -> 32; (3,3) -> 32; (3,_) -> 64; (4,_) -> 64; (5,_) -> 64; (6,_) -> 16; (7,7) -> 64; (7,5) -> 64; (7,0) -> 16; _ -> 32
--              in do
--                 (rm, _, op, mod, reg) <- modrmFpu ds opWidth
--                 let ep = dsPfx ds
--                     fpureg = (Op_Reg . RegFPU) ([ST0, ST1, ST2, ST3, ST4, ST5, ST6, ST7] !! reg)
--                     r i o = return (Instruction ep i o)
--                     st0 = Op_Reg (RegFPU ST0)
--                     rr i = if mod == 3 then r i [st0, rm] else r i [rm]
--                    in case (set, op, mod) of
--                         (0, 0, _) -> rr (Operation "fadd")
--                         (0, 1, _) -> rr (Operation "fmul")
--                         (0, 2, _) -> rr (Operation "fcom")
--                         (0, 3, _) -> rr (Operation "fcomp")
--                         (0, 4, _) -> rr (Operation "fsub")
--                         (0, 5, _) -> rr (Operation "fsubr")
--                         (0, 6, _) -> rr (Operation "fdiv")
--                         (0, 7, _) -> rr (Operation "fdivr")
--
--                         (1, 0, _) -> rr (Operation "fld")
--                         (1, 1, 3) -> rr (Operation "fxch")
--                         (1, 2, 3) -> if reg == 0 then r (Operation "fnop") [] else fail "invalid"
--                         (1, 2, _) -> rr (Operation "fst")
--                         (1, 3, 3) -> r (Operation "fstp1") [fpureg]
--                         (1, 3, _) -> rr (Operation "fstp")
--                         (1, 4, 3) -> case reg of
--                                         0 -> r (Operation "fchs") []
--                                         1 -> r (Operation "fabs") []
--                                         4 -> r (Operation "ftst") []
--                                         5 -> r (Operation "fxam") []
--                                         _ -> fail "invalid"
--                         (1, 4, _) -> rr (Operation "fldenv")
--                         (1, 5, 3) -> case reg of
--                                         0 -> r (Operation "fld1") []
--                                         1 -> r (Operation "fldl2t") []
--                                         2 -> r (Operation "fldl2e") []
--                                         3 -> r (Operation "fldpi") []
--                                         4 -> r (Operation "fldlg2") []
--                                         5 -> r (Operation "fldln2") []
--                                         6 -> r (Operation "fldz") []
--                                         _ -> fail "invalid"
--                         (1, 5, _) -> rr (Operation "fldcw")
--                         (1, 6, 3) -> case reg of
--                                         0 -> r (Operation "f2xm1") []
--                                         1 -> r (Operation "fyl2x") []
--                                         2 -> r (Operation "fptan") []
--                                         3 -> r (Operation "fpatan") []
--                                         4 -> r (Operation "fpxtract") []
--                                         5 -> r (Operation "fprem1") []
--                                         6 -> r (Operation "fdecstp") []
--                                         7 -> r (Operation "fincstp") []
--                         (1, 6, _) -> rr (Operation "fstenv")
--                         (1, 7, 3) -> case reg of
--                                         0 -> r (Operation "fprem") []
--                                         1 -> r (Operation "fyl2xp1") []
--                                         2 -> r (Operation "fsqrt") []
--                                         3 -> r (Operation "fsincos") []
--                                         4 -> r (Operation "frndint") []
--                                         5 -> r (Operation "fscale") []
--                                         6 -> r (Operation "fsin") []
--                                         7 -> r (Operation "fcos") []
--                         (1, 7, _) -> rr (Operation "fstcw")
--
--                         (2, 0, 3) -> r (Operation "fcmovb") [st0, fpureg]
--                         (2, 1, 3) -> r (Operation "fcmove") [st0, fpureg]
--                         (2, 2, 3) -> r (Operation "fcmovbe") [st0, fpureg]
--                         (2, 3, 3) -> r (Operation "fcmovu") [st0, fpureg]
--                         (2, 4, 3) -> fail "invalid"
--                         (2, 5, 3) -> if reg == 1 then r (Operation "fucompp") [] else fail "invalid"
--                         (2, 6, 3) -> fail "invalid"
--                         (2, 7, 3) -> fail "invalid"
--                         (2, 0, _) -> rr (Operation "fiadd")
--                         (2, 1, _) -> rr (Operation "fimul")
--                         (2, 2, _) -> rr (Operation "ficom")
--                         (2, 3, _) -> rr (Operation "ficomp")
--                         (2, 4, _) -> rr (Operation "fisub")
--                         (2, 5, _) -> rr (Operation "fisubr")
--                         (2, 6, _) -> rr (Operation "fidiv")
--                         (2, 7, _) -> rr (Operation "fidivr")
--
--                         (3, 0, 3) -> r (Operation "fcmovnb") [st0, fpureg]
--                         (3, 1, 3) -> r (Operation "fcmovne") [st0, fpureg]
--                         (3, 2, 3) -> r (Operation "fcmovnbe") [st0, fpureg]
--                         (3, 3, 3) -> r (Operation "fcmovnu") [st0, fpureg]
--                         (3, 0, _) -> rr (Operation "fild")
--                         (3, 1, _) -> rr (Operation "fisttp")
--                         (3, 7, _) -> rr (Operation "fstp")
--
--                         (4, 0, _) -> rr (Operation "fadd")
--                         (4, 1, _) -> rr (Operation "fmul")
--                         (4, 2, 3) -> r (Operation "fcom2") [fpureg]
--                         (4, 2, _) -> rr (Operation "fcom")
--                         (4, 3, _) -> rr (Operation "fcomp")
--                         (4, 4, _) -> rr (Operation "fsub")
--                         (4, 5, _) -> rr (Operation "fsubr")
--                         (4, 6, _) -> rr (Operation "fdiv")
--                         (4, 7, _) -> rr (Operation "fdivr")
--
--                         (5, 1, _) -> r (Operation "fisttp") [rm]
--                         (5, 2, _) -> r (Operation "fst") [rm]
--                         (5, 3, _) -> r (Operation "fstp") [rm]
--                         (5, 4, _) -> r (Operation "frstor") [rm]
--                         (5, 0, 0) -> r (Operation "fld") [rm]
--                         (5, 0, 3) -> r (Operation "ffree") [rm]
--
--                         (6, 0, 3) -> r (Operation "faddp") [rm]
--                         (6, 0, _) -> r (Operation "fiadd") [rm]
--                         (6, 1, 3) -> r (Operation "fmulp") [rm]
--                         (6, 2, 3) -> r (Operation "fcomp") [rm]
--                         (6, 3, 3) -> r (Operation "fcompp") [rm]
--                         (6, 4, 3) -> r (Operation "fsubp") [rm]
--                         (6, 5, 3) -> r (Operation "fsubrp") [rm]
--                         (6, 6, 3) -> r (Operation "fdivp") [rm]
--                         (6, 7, 3) -> r (Operation "fdivrp") [rm]
--
--                         (7, 0, 3) -> r (Operation "ffreep") [fpureg]
--                         (7, 0, _) -> r (Operation "fild") [rm]
--                         (7, 1, 3) -> r (Operation "fxch7") [fpureg]
--                         (7, 1, _) -> r (Operation "fisttp") [rm]
--                         (7, 2, 3) -> r (Operation "fstp8") [fpureg]
--                         (7, 2, _) -> r (Operation "fist") [rm]
--                         (7, 3, 3) -> r (Operation "fstp9") [fpureg]
--                         (7, 3, _) -> r (Operation "fistp") [rm]
--                         (7, 4, 3) -> case reg of
--                                     0 -> r (Operation "fstsw") [Op_Reg (Reg16 RAX)]
--                                     _ -> fail "invalid"
--                         (7, 4, _) -> r (Operation "fbld") [rm]
--                         (7, 5, 3) -> r (Operation "fucomip") [fpureg]
--                         (7, 5, _) -> r (Operation "fild") [rm]
--                         (7, 6, 3) -> r (Operation "fcomip") [fpureg]
--                         (7, 6, _) -> r (Operation "fbstp") [rm]
--                         (7, 7, 3) -> fail "invalid"
--                         (7, 7, _) -> r (Operation "fistp") [rm]
--                         _         -> fail "invalid"
--
--
-- applyPrefix :: DisassemblerSingle
-- applyPrefix opcode ds = disassemble1' basicOpcodeMap (addPfx opcode ds)
--   where addPfx o ds = let newpfx = (Map.!) prefixMap o
--                           isseg = case newpfx of (PrefixSeg _) -> True; _ -> False
--                           fi pfx = case pfx of (PrefixRex _) -> False;
--                                                (PrefixSeg _) -> not isseg
--                                                _ | pfx == newpfx -> False
--                                                  | otherwise -> True
--                           pfx' = filter fi (dsPfx ds)
--                         in ds { dsPfx = ((Map.!) prefixMap o):pfx' }
--
-- invalid :: DisassemblerSingle
-- invalid _ _ = fail "invalid"
--
-- basicOpcodeMap :: OpcodeMap
-- basicOpcodeMap = Map.fromList [
--            ( 0x00, op2 (Operation "add")
--         ), ( 0x01, op2 (Operation "add")
--         ), ( 0x02, op2 (Operation "add")
--         ), ( 0x03, op2 (Operation "add")
--         ), ( 0x04, opImm (Operation "add")
--         ), ( 0x05, opImm (Operation "add")
--         ), ( 0x06, invalid
--         ), ( 0x07, invalid
--         ), ( 0x08, op2 (Operation "or")
--         ), ( 0x09, op2 (Operation "or")
--         ), ( 0x0a, op2 (Operation "or")
--         ), ( 0x0b, op2 (Operation "or")
--         ), ( 0x0c, opImm (Operation "or")
--         ), ( 0x0d, opImm (Operation "or")
--         ), ( 0x0e, invalid
--         ), ( 0x0f, \opcode ds -> disassemble1' opcodeMap0f ds
--         ), ( 0x10, op2 (Operation "adc")
--         ), ( 0x11, op2 (Operation "adc")
--         ), ( 0x12, op2 (Operation "adc")
--         ), ( 0x13, op2 (Operation "adc")
--         ), ( 0x14, opImm (Operation "adc")
--         ), ( 0x15, opImm (Operation "adc")
--         ), ( 0x16, invalid
--         ), ( 0x17, invalid
--         ), ( 0x18, op2 (Operation "sbb")
--         ), ( 0x19, op2 (Operation "sbb")
--         ), ( 0x1a, op2 (Operation "sbb")
--         ), ( 0x1b, op2 (Operation "sbb")
--         ), ( 0x1c, opImm (Operation "sbb")
--         ), ( 0x1d, opImm (Operation "sbb")
--         ), ( 0x1e, invalid
--         ), ( 0x1f, invalid
--         ), ( 0x20, op2 (Operation "and")
--         ), ( 0x21, op2 (Operation "and")
--         ), ( 0x22, op2 (Operation "and")
--         ), ( 0x23, op2 (Operation "and")
--         ), ( 0x24, opImm (Operation "and")
--         ), ( 0x25, opImm (Operation "and")
--         ), ( 0x26, applyPrefix -- es
--         ), ( 0x27, invalid
--         ), ( 0x28, op2 (Operation "sub")
--         ), ( 0x29, op2 (Operation "sub")
--         ), ( 0x2a, op2 (Operation "sub")
--         ), ( 0x2b, op2 (Operation "sub")
--         ), ( 0x2c, opImm (Operation "sub")
--         ), ( 0x2d, opImm (Operation "sub")
--         ), ( 0x2e, applyPrefix -- cs
--         ), ( 0x2f, invalid
--         ), ( 0x30, op2 (Operation "xor")
--         ), ( 0x31, op2 (Operation "xor")
--         ), ( 0x32, op2 (Operation "xor")
--         ), ( 0x33, op2 (Operation "xor")
--         ), ( 0x34, opImm (Operation "xor")
--         ), ( 0x35, opImm (Operation "xor")
--         ), ( 0x36, applyPrefix -- ss
--         ), ( 0x37, invalid
--         ), ( 0x38, op2 (Operation "cmp")
--         ), ( 0x39, op2 (Operation "cmp")
--         ), ( 0x3a, op2 (Operation "cmp")
--         ), ( 0x3b, op2 (Operation "cmp")
--         ), ( 0x3c, opImm (Operation "cmp")
--         ), ( 0x3d, opImm (Operation "cmp")
--         ), ( 0x3e, applyPrefix -- ds
--         ), ( 0x3f, invalid
--         ), ( 0x40, applyPrefix -- rex
--         ), ( 0x41, applyPrefix -- rex
--         ), ( 0x42, applyPrefix -- rex
--         ), ( 0x43, applyPrefix -- rex
--         ), ( 0x44, applyPrefix -- rex
--         ), ( 0x45, applyPrefix -- rex
--         ), ( 0x46, applyPrefix -- rex
--         ), ( 0x47, applyPrefix -- rex
--         ), ( 0x48, applyPrefix -- rex
--         ), ( 0x49, applyPrefix -- rex
--         ), ( 0x4a, applyPrefix -- rex
--         ), ( 0x4b, applyPrefix -- rex
--         ), ( 0x4c, applyPrefix -- rex
--         ), ( 0x4d, applyPrefix -- rex
--         ), ( 0x4e, applyPrefix -- rex
--         ), ( 0x4f, applyPrefix -- rex
--         ), ( 0x50, grp50 (Operation "push")
--         ), ( 0x51, grp50 (Operation "push")
--         ), ( 0x52, grp50 (Operation "push")
--         ), ( 0x53, grp50 (Operation "push")
--         ), ( 0x54, grp50 (Operation "push")
--         ), ( 0x55, grp50 (Operation "push")
--         ), ( 0x56, grp50 (Operation "push")
--         ), ( 0x57, grp50 (Operation "push")
--         ), ( 0x58, grp50 (Operation "pop")
--         ), ( 0x59, grp50 (Operation "pop")
--         ), ( 0x5a, grp50 (Operation "pop")
--         ), ( 0x5b, grp50 (Operation "pop")
--         ), ( 0x5c, grp50 (Operation "pop")
--         ), ( 0x5d, grp50 (Operation "pop")
--         ), ( 0x5e, grp50 (Operation "pop")
--         ), ( 0x5f, grp50 (Operation "pop")
--         ), ( 0x60, invalid
--         ), ( 0x61, invalid
--         ), ( 0x62, invalid
--         ), ( 0x63, movsxd
--         ), ( 0x64, applyPrefix -- fs
--         ), ( 0x65, applyPrefix -- gs
--         ), ( 0x66, applyPrefix -- o16
--         ), ( 0x67, applyPrefix -- a32
--         ), ( 0x68, pushImm
--         ), ( 0x69, imul3
--         ), ( 0x6a, pushImm
--         ), ( 0x6b, imul3
--         ), ( 0x6c, simple (Operation "insb") []
--         ), ( 0x6d, sized' (Operation "insw") (Operation "insd")
--         ), ( 0x6e, simple (Operation "outsb") []
--         ), ( 0x6f, sized (Operation "outsw") (Operation "outsd") (Operation "outsq")
--         ), ( 0x70, jshort (Operation "jo")
--         ), ( 0x71, jshort (Operation "jno")
--         ), ( 0x72, jshort (Operation "jb")
--         ), ( 0x73, jshort (Operation "jae")
--         ), ( 0x74, jshort (Operation "jz")
--         ), ( 0x75, jshort (Operation "jnz")
--         ), ( 0x76, jshort (Operation "jbe")
--         ), ( 0x77, jshort (Operation "ja")
--         ), ( 0x78, jshort (Operation "js")
--         ), ( 0x79, jshort (Operation "jns")
--         ), ( 0x7a, jshort (Operation "jp")
--         ), ( 0x7b, jshort (Operation "jnp")
--         ), ( 0x7c, jshort (Operation "jl")
--         ), ( 0x7d, jshort (Operation "jge")
--         ), ( 0x7e, jshort (Operation "jle")
--         ), ( 0x7f, jshort (Operation "jg")
--         ), ( 0x80, grp80
--         ), ( 0x81, grp80
--         ), ( 0x82, invalid
--         ), ( 0x83, grp80
--         ), ( 0x84, op2xt (Operation "test")
--         ), ( 0x85, op2xt (Operation "test")
--         ), ( 0x86, op2xt (Operation "xchg")
--         ), ( 0x87, op2xt (Operation "xchg")
--         ), ( 0x88, op2 (Operation "mov")
--         ), ( 0x89, op2 (Operation "mov")
--         ), ( 0x8a, op2 (Operation "mov")
--         ), ( 0x8b, op2 (Operation "mov")
--         ), ( 0x8c, movsr
--         ), ( 0x8d, lea
--         ), ( 0x8e, movsr
--         ), ( 0x8f, pushpop (Operation "pop")
--         ), ( 0x90, xchg
--         ), ( 0x91, xchg
--         ), ( 0x92, xchg
--         ), ( 0x93, xchg
--         ), ( 0x94, xchg
--         ), ( 0x95, xchg
--         ), ( 0x96, xchg
--         ), ( 0x97, xchg
--         ), ( 0x98, sized (Operation "cbw") (Operation "cwde") (Operation "cdqe")
--         ), ( 0x99, sized (Operation "cwd") (Operation "cdq") (Operation "cqo")
--         ), ( 0x9a, invalid
--         ), ( 0x9b, simple (Operation "wait") []
--         ), ( 0x9c, simple (Operation "pushfq") []
--         ), ( 0x9d, simple (Operation "popfq") []
--         ), ( 0x9e, simple (Operation "sahf") []
--         ), ( 0x9f, simple (Operation "lahf") []
--         ), ( 0xa0, movaddr
--         ), ( 0xa1, movaddr
--         ), ( 0xa2, movaddr
--         ), ( 0xa3, movaddr
--         ), ( 0xa4, simple (Operation "movsb") []
--         ), ( 0xa5, sized (Operation "movsw") (Operation "movsd") (Operation "movsq")
--         ), ( 0xa6, simple (Operation "cmpsb") []
--         ), ( 0xa7, sized (Operation "cmpsw") (Operation "cmpsd") (Operation "cmpsq")
--         ), ( 0xa8, testRax
--         ), ( 0xa9, testRax
--         ), ( 0xaa, simple (Operation "stosb") []
--         ), ( 0xab, sized (Operation "stosw") (Operation "stosd") (Operation "stosq")
--         ), ( 0xac, simple (Operation "lodsb") []
--         ), ( 0xad, sized (Operation "lodsw") (Operation "lodsd") (Operation "lodsq")
--         ), ( 0xae, simple (Operation "scasb") []
--         ), ( 0xaf, sized (Operation "scasw") (Operation "scasd") (Operation "scasq")
--         ), ( 0xb0, movreg
--         ), ( 0xb1, movreg
--         ), ( 0xb2, movreg
--         ), ( 0xb3, movreg
--         ), ( 0xb4, movreg
--         ), ( 0xb5, movreg
--         ), ( 0xb6, movreg
--         ), ( 0xb7, movreg
--         ), ( 0xb8, movreg
--         ), ( 0xb9, movreg
--         ), ( 0xba, movreg
--         ), ( 0xbb, movreg
--         ), ( 0xbc, movreg
--         ), ( 0xbd, movreg
--         ), ( 0xbe, movreg
--         ), ( 0xbf, movreg
--         ), ( 0xc0, shiftrot
--         ), ( 0xc1, shiftrot
--         ), ( 0xc2, ret
--         ), ( 0xc3, simple (Operation "ret") []
--         ), ( 0xc4, invalid
--         ), ( 0xc5, invalid
--         ), ( 0xc6, movimm
--         ), ( 0xc7, movimm
--         ), ( 0xc8, enter
--         ), ( 0xc9, simple (Operation "leave") []
--         ), ( 0xca, retf
--         ), ( 0xcb, simple (Operation "retf") []
--         ), ( 0xcc, simple (Operation "int3") []
--         ), ( 0xcd, int
--         ), ( 0xce, invalid
--         ), ( 0xcf, sized (Operation "iretw") (Operation "iretd") (Operation "iretq")
--         ), ( 0xd0, shiftrot1 (Op_Const 1)
--         ), ( 0xd1, shiftrot1 (Op_Const 1)
--         ), ( 0xd2, shiftrot1 (Op_Reg (Reg8 RCX HalfL))
--         ), ( 0xd3, shiftrot1 (Op_Reg (Reg8 RCX HalfL))
--         ), ( 0xd4, invalid
--         ), ( 0xd5, invalid
--         ), ( 0xd6, invalid
--         ), ( 0xd7, simple (Operation "xlatb") []
--         ), ( 0xd8, fpu
--         ), ( 0xd9, fpu
--         ), ( 0xda, fpu
--         ), ( 0xdb, fpu
--         ), ( 0xdc, fpu
--         ), ( 0xdd, fpu
--         ), ( 0xde, fpu
--         ), ( 0xdf, fpu
--         ), ( 0xe0, jshort (Operation "loopnz")
--         ), ( 0xe1, jshort (Operation "loope")
--         ), ( 0xe2, jshort (Operation "loop")
--         ), ( 0xe3, jshort (Operation "jecxz")
--         ), ( 0xe4, inout Nothing
--         ), ( 0xe5, inout Nothing
--         ), ( 0xe6, inout Nothing
--         ), ( 0xe7, inout Nothing
--         ), ( 0xe8, jmpcall (Operation "call")
--         ), ( 0xe9, jmpcall (Operation "jmp")
--         ), ( 0xea, invalid
--         ), ( 0xeb, jshort (Operation "jmp")
--         ), ( 0xec, inout (Just (Op_Reg (Reg16 RDX)))
--         ), ( 0xed, inout (Just (Op_Reg (Reg16 RDX)))
--         ), ( 0xee, inout (Just (Op_Reg (Reg16 RDX)))
--         ), ( 0xef, inout (Just (Op_Reg (Reg16 RDX)))
--         ), ( 0xf0, applyPrefix
--         ), ( 0xf1, simple (Operation "int1") []
--         ), ( 0xf2, applyPrefix
--         ), ( 0xf3, applyPrefix
--         ), ( 0xf4, simple (Operation "hlt") []
--         ), ( 0xf5, simple (Operation "cmc") []
--         ), ( 0xf6, grpf6
--         ), ( 0xf7, grpf6
--         ), ( 0xf8, simple (Operation "clc") []
--         ), ( 0xf9, simple (Operation "stc") []
--         ), ( 0xfa, simple (Operation "cli") []
--         ), ( 0xfb, simple (Operation "sti") []
--         ), ( 0xfc, simple (Operation "cld") []
--         ), ( 0xfd, simple (Operation "std") []
--         ), ( 0xfe, grpfe
--         ), ( 0xff, grpfe
--         ) ]
--
-- --
--
-- bswap :: DisassemblerSingle
-- bswap opcode ds = let
--     reg = selectreg 0 (bits 0 3 opcode) (if (bitTest 3 (dsRex ds)) then 64 else 32) (dsRex ds) False
--     ep = dsPfx ds
--     in return (Instruction ep (Operation "bswap") [Op_Reg reg])
--
-- grp0f00 :: DisassemblerSingle
-- grp0f00 opcode ds = let pfx = dsPfx ds in do
--     rmb <- lookAhead getWord8
--     case bits 3 3 rmb of
--        0 -> do (rm, _, _, _, _) <- modrm ds 32; let ep = dsPfx ds in return (Instruction ep (Operation "sldt") [rm])
--        1 -> do (rm, _, _, _, _) <- modrm ds 16; let ep = dsPfx ds in return (Instruction ep (Operation "str") [rm])
--        _ -> fail "invalid"
--
-- opcodeMap0f :: OpcodeMap
-- opcodeMap0f = Map.fromList [
--            ( 0x00, grp0f00
--         ), ( 0x05, simple (Operation "syscall") []
--         ), ( 0x06, simple (Operation "clts") []
--         ), ( 0x07, simple (Operation "sysret") []
--         ), ( 0x08, simple (Operation "invd") []
--         ), ( 0xc8, bswap
--         ), ( 0xc9, bswap
--         ), ( 0xca, bswap
--         ), ( 0xcb, bswap
--         ), ( 0xcc, bswap
--         ), ( 0xcd, bswap
--         ), ( 0xce, bswap
--         ), ( 0xcf, bswap
--         ) ]
--
-- --
--
-- modrm ds opWidth = modrm' ds opWidth opWidth False
--
-- modrmFpu ds opWidth = modrm' ds opWidth opWidth True
--
-- modrm' ds opWidth opWidth' fpu = do
--     modrm <- getWord8
--     let b'mod = bits 6 2 modrm
--         b'reg = bits 3 3 modrm
--         b'rm  = bits 0 3 modrm
--         aWidth = if (dsA32 ds) then 32 else 64
--         reg = selectreg 2 b'reg opWidth' (dsRex ds) fpu
--         hasSib = (b'mod /= 3 && b'rm == 4)
--         dispSize = case (b'mod, b'rm) of
--             (0,5) -> Just 32
--             (1,_) -> Just 8
--             (2,_) -> Just 32
--             _     -> Nothing
--         so = dsSeg ds
--       in do
--         (sib,dispSize') <- if hasSib then (parseSib b'mod dispSize (dsRex ds) aWidth) <$> getWord8
--                                      else return ((RegNone,RegNone,0),dispSize)
--         disp <- case dispSize' of
--                     Just 8 -> (Immediate 8 . fromIntegral) <$> getInt8
--                     Just 32 -> (Immediate 32 . fromIntegral) <$> getInt32le
--                     _  -> return $ Immediate 0 0
--         let rm = case (b'mod, b'rm) of
--                 (3,_) -> Op_Reg (selectreg 0 b'rm opWidth (dsRex ds) fpu)
--                 (0,5) -> Op_Mem opWidth aWidth ((if aWidth == 64 then Reg64 else Reg32) RIP) RegNone 0 disp so
--                 (0,4) -> let (br, ir, sc) = sib in Op_Mem opWidth aWidth br ir sc disp so
--                 (1,4) -> let (br, ir, sc) = sib in Op_Mem opWidth aWidth br ir sc disp so
--                 (2,4) -> let (br, ir, sc) = sib in Op_Mem opWidth aWidth br ir sc disp so
--                 (_,_) -> Op_Mem opWidth aWidth (selectreg 0 b'rm aWidth (dsRex ds) False) RegNone 0 disp so
--           in return (rm, reg, b'reg, b'mod, b'rm)
--
-- parseSib m dispSize rex aw sib = let
--                          br = (bits 0 3 sib)
--                          ir = (bits 3 3 sib)
--                          ss = (bits 6 2 sib)
--                          sp = (case aw of 16 -> Reg16; 32 -> Reg32; 64 -> Reg64) RSP
--                          breg = selectreg 0 br aw rex False
--                          ireg = selectreg 1 ir aw rex False
--                          sf = case ss of { 0 -> 1; 1 -> 2; 2 -> 4; 3 -> 8 }
--                     in case (m, br) of (0, 5) -> ((RegNone, if ireg == sp then RegNone else ireg, sf), Just 32)
--                                        _      -> ((breg, if ireg == sp then RegNone else ireg, sf), dispSize)
--
-- selectreg :: Int -> Int -> Int -> Maybe Prefix -> Bool -> Register
-- selectreg rexBit reg opWidth rex fpu = let
--                 rvec' = case () of
--                         _ | bitTest rexBit rex ->
--                                 [R8, R9, R10, R11, R12, R13, R14, R15]
--                           | otherwise ->
--                                 [RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI]
--                 rvec = case (fpu, opWidth) of
--                         (True, _) -> map RegFPU [ST0, ST1, ST2, ST3, ST4, ST5, ST6, ST7]
--                         (False, 8) | isNothing rex ->
--                                          [Reg8 RAX HalfL, Reg8 RCX HalfL, Reg8 RDX HalfL, Reg8 RBX HalfL,
--                                           Reg8 RAX HalfH, Reg8 RCX HalfH, Reg8 RDX HalfH, Reg8 RBX HalfH]
--                                    | isJust rex -> map (\i -> Reg8 i HalfL) rvec'
--                         (False, 16) -> map Reg16 rvec'
--                         (False, 32) -> map Reg32 rvec'
--                         (False, 64) -> map Reg64 rvec'
--             in rvec !! reg
--
