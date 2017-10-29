module ValidCard
  (
    listLength,
    reverseList,
    toDigitsRev,
    toDigits,
    doubleEveryOtherFromLeft,
    doubleEveryOther,
    sumDigits,
    validate,
    mergeDigits
  ) where

listLength :: [Integer] -> Integer
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise =  (mod n 10) : toDigitsRev(div n 10)

toDigits :: Integer -> [Integer]
toDigits n = reverseList (toDigitsRev n)

doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft [] = []
doubleEveryOtherFromLeft (x:xs)
  | (mod (listLength xs) 2 ) == 0 = x * 2 : doubleEveryOtherFromLeft xs
  | otherwise = x : doubleEveryOtherFromLeft xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:xs)
  | (mod (listLength xs) 2) == 1 = x * 2 : doubleEveryOther xs
  | otherwise = x : doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = x + sumDigits xs

merge :: [Integer] -> [Integer] -> [Integer]
merge [] ys = ys
merge (x:xs) ys = x:merge ys xs

mergeDigits :: [Integer] -> [Integer]
mergeDigits [] = []
mergeDigits (x:xs) = merge (toDigits x) (mergeDigits xs)

test :: [Integer] -> Integer
test n = sumDigits (mergeDigits (doubleEveryOther n))

validate :: Integer -> Bool
validate n
  | rem (sumDigits (mergeDigits (doubleEveryOther (toDigits n)))) 10 == 0 = True
  | otherwise = False