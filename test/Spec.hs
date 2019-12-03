import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Partitions

main :: IO ()
main = hspec $ do
  describe "partitionSet" $ do
    it "n = 0" $do
      (length $ partitionSet []) `shouldBe` (1 :: Int)
    it "n = 1" $do
      (length $ partitionSet [1]) `shouldBe` (1 :: Int)
    it "n = 2" $do
      (length $ partitionSet [1,2]) `shouldBe` (2 :: Int)
    it "n = 3" $do
      (length $ partitionSet [1..3]) `shouldBe` (5 :: Int)
    it "n = 4" $do
      (length $ partitionSet [1..4]) `shouldBe` (15 :: Int)
    it "n = 5" $do
      (length $ partitionSet [1..5]) `shouldBe` (52 :: Int)
    it "n = 6" $do
      (length $ partitionSet [1..6]) `shouldBe` (203 :: Int)
    it "n = 7" $do
      (length $ partitionSet [1..7]) `shouldBe` (877 :: Int)
    it "n = 8" $do
      (length $ partitionSet [1..8]) `shouldBe` (4140 :: Int)