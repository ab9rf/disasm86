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

instance Arbitrary TPrefix where arbitrary = TPrefix <$> elements (allprefix)
instance Arbitrary TOpcode where arbitrary = TOpcode <$> elements (allopcodes)
instance Arbitrary TSuffix where arbitrary = TSuffix <$> elements (allmodrm)
instance Arbitrary TPad    where arbitrary = TPad <$> elements [0x00..0xff]
instance Arbitrary Test    where arbitrary = makeTest <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

makeTest (TPrefix p) (TOpcode o) (TSuffix r) (TPad pad) = makeTest' (take 15 (p ++ o ++ r ++ (replicate 15 pad)))

hexstring bs = B.foldr s "" bs
    where s b = if b > 15 then showHex b else ('0':).(showHex b)

makeTest' bs = let
        bytes = B.pack bs
        cfg = Config Intel Mode64 SyntaxIntel (0x1000 :: Word64)
        m = head (disassembleMetadata cfg (B.toStrict bytes))
        descr = hexstring bytes
        ref'' = mdAssembly m
        ref' = (if (last ref'' == ' ') then init else id) ref''
        last7 = reverse (take 7 (reverse ref'))
        ref = case last7 of "invalid" -> ""
                            _         -> map toLower ref'
    in Test bytes descr ref

testdis t = intercalate "\n" (map D.textrep (take 1 (fst (D.disassemble 0x1000 (bytes t)))))

----

statictests = map toBS [
      "0000"
    , "7f7f7f7fb6b6b6b6b6"
    , "817f7f7f7fb6b6b6b6b6"
    , "44817f7f7f7fb6b6b6b6b6"
    , "6644817f7f7f7fb6b6b6b6b6"
    , "6744817f7f7f7fb6b6b6b6b6"
    , "676644817f7f7f7fb6b6b6b6b6"
    , "a144817f7f7f7fb6b6b6b6b6"
    , "42a144817f7f7f7fb6b6b6b6b6"
    , "6642a144817f7f7f7fb6b6b6b6b6"
    , "676642a144817f7f7f7fb6b6b6b6b6"
    , "e4b72929292929292929"
    , "fee4b72929292929292929"
    , "4bfee4b72929292929292929"
    , "364bfee4b72929292929292929"
    , "67364bfee4b72929292929292929"
    , "f367364bfee4b72929292929292929"
    , "5c6300000000e2e2e2e2"
    , "385c6300000000e2e2e2e2"
    , "48385c6300000000e2e2e2e2"
    , "6448385c6300000000e2e2e2e2"
    , "676448385c6300000000e2e2e2e2"
    , "f2676448385c6300000000e2e2e2e2"
    , "98645a7f7f7f7fc4c4c4c4"
    , "4998645a7f7f7f7fc4c4c4c4"
    , "654998645a7f7f7f7fc4c4c4c4"
    , "66654998645a7f7f7f7fc4c4c4c4"
    , "f066654998645a7f7f7f7fc4c4c4c4"
    , "1e1e1e1e1e1e1e1e1e1e1e"
    , "ca1e1e1e1e1e1e1e1e1e1e1e"
    , "c7ca1e1e1e1e1e1e1e1e1e1e1e"
    , "3ec7ca1e1e1e1e1e1e1e1e1e1e1e"
    , "f33ec7ca1e1e1e1e1e1e1e1e1e1e1e"
    , "f0673e496e2cab1313131313131313"
    , "bcf1ffffffff9f9f9f9f"
    , "2bbcf1ffffffff9f9f9f9f"
    , "442bbcf1ffffffff9f9f9f9f"
    , "66442bbcf1ffffffff9f9f9f9f"
    , "6766442bbcf1ffffffff9f9f9f9f"
    , "f36766442bbcf1ffffffff9f9f9f9f"
    , "7f7f7f7f2525252525"
    , "7d7f7f7f7f2525252525"
    , "947d7f7f7f7f2525252525"
    , "ef947d7f7f7f7f2525252525"
    , "49ef947d7f7f7f7f2525252525"
    , "3e49ef947d7f7f7f7f2525252525"
    , "f33e49ef947d7f7f7f7f2525252525"
    , "d4657a7a7a7a7a7a7a7a"
    , "9cd4657a7a7a7a7a7a7a7a"
    , "459cd4657a7a7a7a7a7a7a7a"
    , "26459cd4657a7a7a7a7a7a7a7a"
    , "6626459cd4657a7a7a7a7a7a7a7a"
    , "676626459cd4657a7a7a7a7a7a7a7a"
    , "7f7f7f7fbfbfbfbfbfbf"
    , "8f7f7f7f7fbfbfbfbfbfbf"
    , "b48f7f7f7f7fbfbfbfbfbfbf"
    , "dbb48f7f7f7f7fbfbfbfbfbfbf"
    , "43dbb48f7f7f7f7fbfbfbfbfbfbf"
    , "f043dbb48f7f7f7f7fbfbfbfbfbfbf"
    , "54eaffffffff46464646"
    , "a954eaffffffff46464646"
    , "42a954eaffffffff46464646"
    , "2e42a954eaffffffff46464646"
    , "662e42a954eaffffffff46464646"
    , "67662e42a954eaffffffff46464646"
    , "cc3f292929292929292929"
    , "68cc3f292929292929292929"
    , "4c68cc3f292929292929292929"
    , "264c68cc3f292929292929292929"
    , "66264c68cc3f292929292929292929"
    , "6fbc197f7f7f7f383838"
    , "496fbc197f7f7f7f383838"
    , "26496fbc197f7f7f7f383838"
    , "6626496fbc197f7f7f7f383838"
    , "676626496fbc197f7f7f7f383838"
    , "f3676626496fbc197f7f7f7f383838"
    , "33dbdbdbdbdbdbdbdb"
    , "f433dbdbdbdbdbdbdbdb"
    , "fef433dbdbdbdbdbdbdbdb"
    , "4efef433dbdbdbdbdbdbdbdb"
    , "364efef433dbdbdbdbdbdbdbdb"
    , "66364efef433dbdbdbdbdbdbdbdb"
    , "f366364efef433dbdbdbdbdbdbdbdb"
    , "13131313131313131313"
    , "0d13131313131313131313"
    , "0c0d13131313131313131313"
    , "850c0d13131313131313131313"
    , "40850c0d13131313131313131313"
    , "2640850c0d13131313131313131313"
    , "3196969696969696"
    , "e43196969696969696"
    , "dfe43196969696969696"
    , "4cdfe43196969696969696"
    , "2e4cdfe43196969696969696"
    , "662e4cdfe43196969696969696"
    , "67662e4cdfe43196969696969696"
    , "f367662e4cdfe43196969696969696"
    , "818caf7f7f7f7f2929292929"
    , "4c818caf7f7f7f7f2929292929"
    , "654c818caf7f7f7f7f2929292929"
    , "f0654c818caf7f7f7f7f2929292929"
    , "7aaaaaaaaaaaaaaaaa"
    , "fc7aaaaaaaaaaaaaaaaa"
    , "6dfc7aaaaaaaaaaaaaaaaa"
    , "4b6dfc7aaaaaaaaaaaaaaaaa"
    , "2e4b6dfc7aaaaaaaaaaaaaaaaa"
    , "662e4b6dfc7aaaaaaaaaaaaaaaaa"
    , "67662e4b6dfc7aaaaaaaaaaaaaaaaa"
    , "7f7f7f7f9b9b9b9b9b9b9b"
    , "8e7f7f7f7f9b9b9b9b9b9b9b"
    , "b48e7f7f7f7f9b9b9b9b9b9b9b"
    , "e9b48e7f7f7f7f9b9b9b9b9b9b9b"
    , "44e9b48e7f7f7f7f9b9b9b9b9b9b9b"
    , "cdcdcdcdcdcdcdcd"
    , "82cdcdcdcdcdcdcdcd"
    , "2482cdcdcdcdcdcdcdcd"
    , "ff2482cdcdcdcdcdcdcdcd"
    , "4dff2482cdcdcdcdcdcdcdcd"
    , "264dff2482cdcdcdcdcdcdcdcd"
    , "66264dff2482cdcdcdcdcdcdcdcd"
    , "f366264dff2482cdcdcdcdcdcdcdcd"
    ]


