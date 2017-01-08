module DisassemblerSpec (spec)
where

import Test.Hspec
import Test.QuickCheck hiding ((.&.))

import qualified Disassembler as D
import qualified Disassembler.TextRep.Intel as DTI
import qualified Data.ByteString.Lazy as B

import Data.Word (Word8, Word64)
import Data.Maybe (catMaybes)
import Data.List (intercalate, (\\), union)
import Data.Bits ((.&.))
import Data.Char (toLower)
import Numeric (showHex)

import Hdis86

import System.Random

spec :: Spec
spec = do
    describe "basic test" $ do
        it "Empty bytestring" $ D.disassemble 0x1000 B.empty `shouldBe` ([], B.empty)

    describe "basic disassembly" $ do
-- 0x0000: add [rax], al
        it "0000" $ D.disassemble 0x1000 (B.pack [0x00, 0x00]) `shouldBe`
            ([D.Instruction [] D.I_ADD [ D.Op_Mem 8 64 (D.Reg64 D.RAX) (D.RegNone) 0 (D.Immediate 0 0) Nothing
                                       , D.Op_Reg (D.Reg8 D.RAX D.HalfL)]]
            , B.empty)
    describe "static disassembly tests" $ do
        mapM_
            (\bs -> let t = makeTest' bs
                         in it (show t) $ testdis t `shouldBe` refValue t)
            statictests
    describe "quickcheck tests" $ do
        it "matches reference (a)" $ property $ \t -> testdis t `shouldBe` refValue t

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
        sgPfx = [ 0x26, 0x2e, 0x36, 0x3e, 0x64, 0x65 ]
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

instance Arbitrary TPrefix where
    arbitrary = TPrefix <$> elements (allprefix)
    shrink (TPrefix a) = map TPrefix (shrink a)

instance Arbitrary TOpcode where arbitrary = TOpcode <$> elements (allopcodes)
instance Arbitrary TSuffix where arbitrary = TSuffix <$> elements (allmodrm)
instance Arbitrary TPad    where arbitrary = TPad <$> elements [0x00..0xff]
instance Arbitrary Test    where
    arbitrary = makeTest <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    shrink (Test bs _ _) = map makeTest' (shrink (B.unpack bs))

type Test' = [Word8]

makeTest (TPrefix p) (TOpcode o) (TSuffix r) (TPad pad) = makeTest' (take 15 (p ++ o ++ r ++ (replicate 15 pad)))

hexstring bs = B.foldr s "" bs
    where s b = if b > 15 then showHex b else ('0':).(showHex b)

makeTest' bs = let
        bytes = B.pack bs
        cfg = Config Intel Mode64 SyntaxIntel (0x1000 :: Word64)
        dm = disassembleMetadata cfg (B.toStrict bytes)
        descr = hexstring bytes
        ref = case dm of
            []    -> ""
            (m:_) -> let ref'' = mdAssembly m
                         ref' = (if (last ref'' == ' ') then init else id) ref''
                         last7 = reverse (take 7 (reverse ref'))
                      in case last7 of "invalid" -> ""
                                       _         -> (map toLower ref')
      in Test bytes descr ref

testdis t = intercalate "\n" (map DTI.textrep (take 1 (fst (D.disassemble 0x1000 (bytes t)))))

----

statictests = map toBS [
      "0000"
    , "d8c0"
    , "6300"
    , "666300"
    , "676300"
    , "66676300"
    , "67666300"
    , "6766486300"
    , "486300"
    , "66486300"
    , "67486300"
    , "6667486300"
    , "6766486300"
    , "676648486300"
    , "001c6500000080"
    , "673400"
    , "67a100000000"
    , "a10000000000000000"
    , "a10000000000000000"
    , "8cc0"
    , "488cc0"
    , "678cc0"
    , "67488cc0"
    , "668cc0"
    , "66488cc0"
    , "66678cc0"
    , "6667488cc0"

   ]


