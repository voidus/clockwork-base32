module Main (main) where

import           ClockworkBase32        (maxInList)
import           Test.Hspec


main :: IO ()
main = hspec $ do
  describe "maxInList" $ do
    it "returns Nothing for an empty list" $ do
      maxInList ([] :: [Int]) `shouldBe` Nothing

    it "returns the maximum element for a non-empty list" $ do
      maxInList ([1, 2, 3] :: [Int]) `shouldBe` Just 3
