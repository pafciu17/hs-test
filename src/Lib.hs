module Lib
    ( someFunc
    ) where

someFunc :: IO ()

reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList (x:xs) = reverseList(xs) ++ [x]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | otherwise =  (mod n 10) : toDigitsRev(div n 10)

toDigits :: Integer -> [Integer]
toDigits n = reverseList(toDigitsRev(n))

someFunc = print (toDigits (5069109))
