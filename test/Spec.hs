module Spec where
import Test.Hspec
import Test.QuickCheck

import Intro01

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "intro01" $ do
    describe "luhn" $ do
      it "luhn 5594589764218858 = True " $ do
        luhn 5594589764218858 `shouldBe` True
      it "luhn 1234567898765432 = False" $ do
        luhn 1234567898765432 `shouldBe` False
--      it "luhn 1234567898765432 = False" $ property $
--        \str -> strip str == strip (strip str)
    describe "hanoi" $ do
      it "one move should be direct" $ property $
        \(a,b,c) -> hanoi 1 a b c == [(a,b)]
      it "two move should use a jump" $ do
        hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
      it "15 disks, move size should be 32767" $ property $
        \(a,b,c) -> length (hanoi 15 a b c) ==  32767
      it "15 disks, with four pegs, the moves size should be 129" $ property $
        \(a,b,c,d) -> length (hanoi4 15 a b c d) == 129
