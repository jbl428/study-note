import qualified Day1.Solution as D1
import qualified Day2.Solution as D2
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "Day 1" $ do
    it "should success" $ do
      D1.solve "123\n234\n121\n323" `shouldBe` 1

  describe "Day 2" $ do
    it "should success" $ do
      D2.solve "123\n234\n121\n323" `shouldBe` 0