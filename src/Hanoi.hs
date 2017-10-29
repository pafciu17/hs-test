module Hanoi
  (
    hanoi,
    Move
  )
  where

type Peg = String
type Move = (Peg, Peg)

merge :: [Move] -> [Move] -> [Move]
merge [] ys = ys
merge (x:xs) ys = x:merge xs ys

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi s a b c
  | s == 0 = []
  | s == 1 = [(a, b)]
  | s == 2 = [(a, c), (a, b), (c, b)]
  | otherwise = merge (hanoi (s - 1) a c b) ((a, b) : (hanoi (s - 1) c b a))