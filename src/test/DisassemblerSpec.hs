module DisassemblerSpec (spec)
where

import Test.Hspec
import Test.QuickCheck hiding ((.&.))

import qualified Disassembler as D
import qualified Data.ByteString.Lazy as B

import Data.Word (Word8, Word64)
import Data.Maybe (catMaybes)
import Data.List (intercalate, (\\), union)
import Data.Bits ((.&.))

import Hdis86

import System.Random

spec :: Spec
spec = do
    describe "basic test" $ do
        it "Empty bytestring" $ D.disassemble 0x1000 B.empty `shouldBe` ([], B.empty)

    describe "basic disassembly" $ do
-- 0x0000: add [rax], al
        it "0000" $ D.disassemble 0x1000 (B.pack [0x00, 0x00]) `shouldBe`
            ([D.Instruction [] D.I_ADD [ D.Op_Mem 8 (D.Reg64 D.RAX) (D.RegNone) 0 (D.Immediate 0 0) Nothing
                                       , D.Op_Reg (D.Reg8 D.RAX D.HalfL)]]
            , B.empty)
    describe "disassembler" $ do
        mapM
            (\bs -> let t = makeTest' bs
                         in it (show t) $ testdis t `shouldBe` refValue t)
            statictests
        it "matches reference" $ property $ \t -> testdis t `shouldBe` refValue t

toBS :: String -> [Word8]
toBS []        = []
toBS (f1:f2:r) = (read ("0x" ++ [f1,f2])) : toBS r

allopcodes = let
        opcodes1 = [ [o] | o <- [0x00 .. 0xFF] \\ prefixes ]
        opcodes2 = [ [p, o] | p <- tbPfx, o <- [0x00, 0xff] ]
    in opcodes1 ++ opcodes2

allmodrm :: [ [Word8] ]
allmodrm = let
        hassib modrm = (modrm .&. 0x07 == 4 && modrm .&. 0xC0 /= 0xE0)
        hasdisp modrm = (modrm .&. 0xC7 == 0x5 ||
                         modrm .&. 0xC0 == 0x40 ||
                         modrm .&. 0xC0 == 0x80)
        onemodrm mrm = if hasdisp mrm
                        then [ m ++ d | m <- mrm', d <- [[0x00,0x00,0x00,0x00], [0x7f, 0x7f, 0x7f, 0x7f], [0xff, 0xff, 0xff, 0xff]] ]
                        else mrm'
                where mrm' = if hassib mrm
                                then [ mrm : s : [] | s <- [0x00..0xff] ]
                                else [ mrm : [] ]
    in concatMap onemodrm [0x00..0xff]

(prefixes, allprefix, tbPfx) = let
        prefixes = foldr1 union [ insPfx, adPfx, opPfx, sgPfx, rexPfx, tbPfx ]
        allprefix = [ i ++ a ++ o ++ s ++ r | i <- wrap insPfx,
                                                       a <- wrap adPfx,
                                                       o <- wrap opPfx,
                                                       s <- wrap sgPfx,
                                                       r <- wrap rexPfx ]
        wrap l = [] : map (:[]) l
        tbPfx = [ 0x0f ]
        rexPfx = [ 0x40..0x4f ]
        insPfx = [ 0xf0, 0xf2, 0xf3, 0x9b]
        adPfx = [ 0x67 ]
        opPfx = [ 0x66 ]
        sgPfx = [ 0x26, 0x2e, 0x36, 0x3e, 0x66, 0x67 ]
    in (prefixes, allprefix, tbPfx)

data TPrefix = TPrefix [ Word8 ]
    deriving (Show, Eq)
data TOpcode = TOpcode [ Word8 ]
    deriving (Show, Eq)
data TSuffix = TSuffix [ Word8 ]
    deriving (Show, Eq)
data TPad = TPad ( Word8 )
    deriving (Show, Eq)

data Test = Test {
      bytes     :: B.ByteString
    , descr     :: String
    , refValue  :: String
    }
    deriving (Eq)

instance Show Test where show (Test _ d r) = d ++ " -> " ++ r

instance Arbitrary TPrefix where arbitrary = TPrefix <$> elements (allprefix)
instance Arbitrary TOpcode where arbitrary = TOpcode <$> elements (allopcodes)
instance Arbitrary TSuffix where arbitrary = TSuffix <$> elements (allmodrm)
instance Arbitrary TPad    where arbitrary = TPad <$> elements [0x00..0xff]
instance Arbitrary Test    where arbitrary = makeTest <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

makeTest (TPrefix p) (TOpcode o) (TSuffix r) (TPad pad) = makeTest' (p ++ o ++ r ++ (replicate 15 pad))

makeTest' bs = let
        bytes = B.pack bs
        cfg = Config Intel Mode64 SyntaxIntel (0x1000 :: Word64)
        m = head (disassembleMetadata cfg (B.toStrict bytes))
        descr = mdHex m
        ref'' = mdAssembly m
        ref' = (if (last ref'' == ' ') then init else id) ref''
        ref = case ref' of "invalid" -> ""
                           _         -> ref'
    in Test bytes descr ref

testdis t = intercalate "\n" (map D.textrep (take 1 (fst (D.disassemble 0x1000 (bytes t)))))

----

statictests = map toBS [
      "0000"
    , "9b"
    , "f067662e4b29743a00"
    ]


