module DisassemblerSpec (spec)
where

import Test.Hspec
import Test.QuickCheck (property)

import qualified Disassembler as D
import qualified Data.ByteString.Lazy as B

import Data.Word (Word8, Word64)
import Data.Maybe (catMaybes)
import Data.List (intercalate)

import Hdis86

import System.Random

spec :: Spec
spec = do
    describe "basic test" $ do
        it "Empty bytestring" $ D.disassemble 0x1000 B.empty `shouldBe` ([], B.empty)

    describe "basic disassembly" $ do
-- 0x0000: add [rax], al
        it "00" $ D.disassemble 0x1000 (B.pack [0x00, 0x00]) `shouldBe`
            ([D.Instruction [] D.I_ADD [ D.Op_Mem 8 (D.Reg64 D.RAX) (D.RegNone) 0 (D.Immediate 0 0)
                                       , D.Op_Reg (D.Reg8 D.RAX D.HalfL)]]
            , B.empty)
    describe "text representations" $ do
        it "0000" $ D.textrep (D.Instruction [] D.I_ADD [ D.Op_Mem 8 (D.Reg64 D.RAX) (D.RegNone) 0 (D.Immediate 0 0)
                                                        , D.Op_Reg (D.Reg8 D.RAX D.HalfL)])
                      `shouldBe` "add [rax], al"
    describe "instructions" $ do
        mapM_
            (\(Test d i o) -> (it d $ (intercalate "\n" (map D.textrep (take 1 (fst (D.disassemble 0x1000 i)))))
                                    `shouldBe` o))
            tests

data Test = Test
    { description :: String
    , input :: B.ByteString
    , output :: String
    }

tests = let
           input = [ B.pack (pfx1 ++ pfx2 ++ (x : replicate 15 sfx))
                        | x <- [0x00 .. 0xFF]
                        , sfx <- [0x00 .. 0xFF]
                        , pfx1 <- [ [], [0x66], [0x67] ]
                        , pfx2 <- [[]] ++ [ [r] | r <- [0x40..0x4f] ]
                        ]
           cfg = Config Intel Mode64 SyntaxIntel (0x1000 :: Word64)
           one bs = let m = head (disassembleMetadata cfg (B.toStrict bs))
                        s'' = mdAssembly m
                        s' = (if (last s'' == ' ') then init else id) s''
                        s = case s' of "invalid" -> ""
                                       _         -> s'
                    in Test (mdHex m) bs s
           all = map one input
           cnt = length all
           rg = mkStdGen 0
           rands = map (<100) $ randomRs (0, cnt-1) rg
           all' = zipWith (\i b -> if i then Just b else Nothing) rands all
        in (Test "0000" (B.pack [0,0]) "add [rax], al") : (catMaybes all')