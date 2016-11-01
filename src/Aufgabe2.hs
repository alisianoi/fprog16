module Aufgabe2 where

factorial :: Integer -> Integer
factorial n
  | n < 0 = error "Factorial is defined for non-negatives only!"
  | n == 0 = 1
  | otherwise = n * factorial (n - 1)

facLst :: Integer -> [Integer]
facLst n
  | n < 0 = []
  | otherwise = [factorial m | m <- [0, 1 .. n]]

factsL :: Integer -> [Integer]
factsL n
  | n < 0 = []
  | otherwise = [factorial m | m <- [n, n - 1 .. 0]]
