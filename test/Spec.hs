import Test.Hspec
import Test.QuickCheck

import Intro01

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "luhn" $ do
    it "luhn 5594589764218858 = True " $ do
      luhn 5594589764218858 `shouldBe` True
    it "luhn 1234567898765432 = False" $ do
      luhn 1234567898765432 `shouldBe` False
--    it "luhn 1234567898765432 = False" $ property $
--      \str -> strip str == strip (strip str)
