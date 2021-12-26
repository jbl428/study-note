import qualified Day1.Solution as D1
import qualified Day2.Solution as D2
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "Day 1" $ do
    let input = unlines ["199", "200", "208", "210", "200", "207", "240", "269", "260", "263"]
    it "should success" $ do
      D1.solve input `shouldBe` 5

  describe "Day 2" $ do
    let input = unlines ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]
    it "should success" $ do
      D2.solve input `shouldBe` 900
