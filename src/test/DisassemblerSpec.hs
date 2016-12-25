module DisassemblerSpec (spec)
where

import Test.Hspec
import Test.QuickCheck (property)

import qualified Disassembler as D
import qualified Data.ByteString.Lazy as B

spec :: Spec
spec = do
    describe "basic test" $ do
        it "Empty bytestring" $ D.disassemble B.empty `shouldBe` ([], B.empty)

    describe "basic opcodes" $ do
-- 0x0000: add [rax], al
        it "00" $ D.disassemble (B.pack [0x00, 0x00]) `shouldBe`
            ([D.Instruction [] D.I_ADD [ D.Op_Mem 8 (D.Reg64 D.RAX) (D.RegNone) 0 (D.Immediate 0 0)
                                       , D.Op_Reg (D.Reg8 D.RAX D.HalfL)]]
            , B.empty)
    describe "text representations" $ do
        it "0000" $ D.textrep (D.Instruction [] D.I_ADD [ D.Op_Mem 8 (D.Reg64 D.RAX) (D.RegNone) 0 (D.Immediate 0 0)
                                                        , D.Op_Reg (D.Reg8 D.RAX D.HalfL)])
                      `shouldBe` "add [rax], al"
