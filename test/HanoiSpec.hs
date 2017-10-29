module HanoiSpec (main, spec) where

import Test.Hspec
import Hanoi (hanoi, Move)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "hanoi" $ do
    it "returns empty move list for 0 discs" $ do
      (hanoi 0 "a" "b" "c") `shouldBe` ([] :: [Move])
    it "should return list of correct moves" $ do
      (hanoi 1 "a" "b" "c") `shouldBe` ([("a", "b")] :: [Move])
      (hanoi 2 "a" "b" "c") `shouldBe` ([("a","c"), ("a","b"), ("c","b")] :: [Move])
      (hanoi 3 "a" "b" "c") `shouldBe` ([("a","b"), ("a","c"), ("b","c"), ("a", "b"), ("c", "a"), ("c", "b"), ("a", "b")] :: [Move])