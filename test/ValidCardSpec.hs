module ValidCardSpec (main, spec) where

import Test.Hspec
import ValidCard (listLength, reverseList, toDigitsRev, toDigits, doubleEveryOther, sumDigits, validate, mergeDigits)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "listLength" $ do
    it "returns the length of the list" $ do
      (listLength [1, 2, 3]) `shouldBe` (3 :: Integer)
  describe "reverseList" $ do
    it "returns list in reversed orderd" $ do
      (reverseList [3, 2, 1]) `shouldBe` ([1, 2, 3] :: [Integer])
  describe "toDigits" $ do
    it "converts positive Integer to list of digits" $ do
      (toDigits 1234) `shouldBe` ([1, 2, 3, 4] :: [Integer])
    it "returns empty list for not positive integers" $ do
       (toDigits 0) `shouldBe` ([] :: [Integer])
       (toDigits (-17)) `shouldBe` ([] :: [Integer])
  describe "toDigitsRev" $ do
    it "converts positive Integer to reversed list of digits" $ do
      (toDigitsRev 1234) `shouldBe` ([4, 3, 2, 1] :: [Integer])
    it "returns empty list for not positive integers" $ do
      (toDigitsRev 0) `shouldBe` ([] :: [Integer])
      (toDigitsRev (-5)) `shouldBe` ([] :: [Integer])
  describe "doubleEveryOther" $ do
    it "doubles every second (going from the right) value of the list" $ do
      (doubleEveryOther [1, 2, 3, 4]) `shouldBe` ([2, 2, 6, 4] :: [Integer])
      (doubleEveryOther [1, 2, 3]) `shouldBe` ([1, 4, 3] :: [Integer])
  describe "sumDigits" $ do
    it "sums all digits in the list" $ do
      (sumDigits [1, 2, 3, 4]) `shouldBe` (10 :: Integer)
      (sumDigits []) `shouldBe` (0 :: Integer)
  describe "validate" $ do
    it "returns True for valid card number" $ do
      (validate 4012888888881881) `shouldBe` (True :: Bool)
    it "returns False for invalid card number" $ do
      (validate 4012888888881882) `shouldBe` (False :: Bool)
