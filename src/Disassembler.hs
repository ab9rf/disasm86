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

import qualified Data.ByteString.Lazy as B

data Instruction = Instruction {
      inPrefix    :: [Prefix]
    , inOperation :: Operation
    , inOperands  :: [Operand]
    } deriving (Show, Eq)

data Prefix = Prefix
    deriving (Show, Eq)

data Operation =
        I_ADD
    deriving (Show, Eq)

data Operand =
        Op_Mem {
                mSize  :: Int
              , mReg   :: Register
              , mIdx   :: Register
              , mScale :: Word8
              , mDisp  :: ImmediateS
              }
      | Op_Reg Register
    deriving (Show, Eq)

data Register =
        RegNone
      | Reg8 GPR RegHalf
      | Reg16 GPR
      | Reg32 GPR
      | Reg64 GPR
      | RegSeg SReg
    deriving (Show, Eq)

data GPR = RAX | RCX | RDX | RBX | RSP | RBP | RSI | RDI | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
    deriving (Show, Eq, Ord, Enum)

data SReg = ES | CS | SS | DS | FS | GS
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
    let t1 = opertext oper
        t2 = intercalate ", " (map operandtext operands)
      in case t2 of "" -> t1
                    _  -> t1 ++ " " ++ t2

opertext :: Operation -> String
opertext I_ADD = "add"

operandtext :: Operand -> String
operandtext (Op_Reg r) = registertext r
operandtext (Op_Mem _ base RegNone _ (Immediate _ 0)) = "[" ++ registertext base ++ "]"

registertext :: Register -> String
registertext (Reg64 RAX) = "rax"
registertext (Reg8 RAX HalfL) = "al"

sibtext :: (Register, Word8) -> String
sibtext _ = "<<sib>>" -- TODO

disassemble :: ByteString -> ([Instruction], ByteString)
disassemble s = case runGetOrFail disassemble1 s of
                    Left _          -> ([], s)
                    Right (r, _, i) -> let (i',r') = disassemble r in (i:i', r')

disassemble1 :: Get Instruction
disassemble1 =  disassemble1' pfxNone

data PrefixState = PrefixState {
          pfxRex ::  Maybe Word8
        , pfxO16 :: Bool
        , pfxA32 :: Bool
    }

pfxNone = PrefixState Nothing False False

--

bits s l i = fromIntegral $ (i `shiftR` s) .&. ((1 `shiftL` l) - 1)

bitTest i v = case v of
                Nothing -> False
                Just n  -> n .&. (bit i) /= 0

-- this is the long mode (64-bit) disassembler
disassemble1' :: PrefixState -> Get Instruction
disassemble1' pfx = do
    opcode <- getWord8
    let bitW = (opcode .&. (bit 0))
        opWidth = o' bitW (fmap (\x -> (x .&. (bit 4)) /= 0) (pfxRex pfx)) (pfxO16 pfx)
            where o' 0 _ _                  = 8
                  o' 1 Nothing      False   = 32
                  o' 1 Nothing      True    = 16
                  o' 1 (Just False) False   = 32
                  o' 1 (Just False) True    = 16
                  o' 1 (Just True)  _       = 64
      in case opcode of
        0x00 -> op2 I_ADD pfx opWidth
        _ -> fail ("invalid opcode " ++ show opcode)
  where
    op2 i pfx opWidth = do
        modrm <- getWord8
        let b'mod = bits 6 2 modrm
            b'reg = bits 3 3 modrm
            b'rm  = bits 0 3 modrm
            reg = selectreg 2 b'reg opWidth (pfxRex pfx)
            hasSib = (b'mod /= 3 && b'rm == 4)
            dispSize = case (b'mod, b'rm) of
                (0,5) -> Just 32
                (0,6) -> Just 32
                (1,_) -> Just 8
                (2,_) -> Just 32
                _     -> Nothing
            getDisp sz = case sz of
                Just 8 -> (Immediate 8 . fromIntegral) <$> getWord8
                Just 32 -> (Immediate 32 . fromIntegral) <$> getWord32le
                _  -> return $ Immediate 0 0
            parseSib sib = (RegNone, 0) -- FIXME
          in do
            sib <- if hasSib then (parseSib <$> getWord8) else return (RegNone,0)
            disp <- getDisp dispSize <|> (return $ Immediate 0 0)
            rm <- return $ case b'mod of
                    0 -> Op_Mem opWidth (selectreg 0 b'rm 64 (pfxRex pfx)) (fst sib) (snd sib) disp
                    3 -> Op_Reg (selectreg 0 b'rm opWidth (pfxRex pfx))
            return (Instruction [] i [rm, Op_Reg reg])

selectreg rexBit reg opWidth rex = let
                rvec' = case () of
                        _ | bitTest rexBit rex ->
                                [R8, R9, R10, R11, R12, R13, R14, R15]
                          | otherwise ->
                                [RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI]
                rvec = case opWidth of
                        8 | isNothing rex ->
                                [Reg8 RAX HalfL, Reg8 RCX HalfL, Reg8 RDX HalfL, Reg8 RDX HalfL,
                                    Reg8 RAX HalfH, Reg8 RCX HalfH, Reg8 RDX HalfH, Reg8 RDX HalfH]
                          | isJust rex -> map (\i -> Reg8 i HalfL) rvec'
                        16 -> map Reg16 rvec'
                        32 -> map Reg32 rvec'
                        64 -> map Reg64 rvec'
            in rvec !! reg

