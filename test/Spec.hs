module Main where

import Test.Hspec
import Lib (listLength)

main :: IO ()
main = hspec $ do
  describe "listLength" $ do
    it "returns the length of the list" $ do
        (listLength [1, 2, 3]) `shouldBe` (3 :: Integer)