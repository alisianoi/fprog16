module Aufgabe2 where

import Data.Char (isDigit, isAlpha)

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

extractNumerals :: String -> [String]
extractNumerals "" = []
extractNumerals s@(c:_)
  | isDigit c =
    let
      t = span isDigit s
    in
      let
        digits = fst t
        suffix = snd t
      in
        digits : extractNumerals suffix
  | otherwise = extractNumerals $ dropWhile isAlpha s

isPowOf2 :: Int -> (Bool, Int)
isPowOf2 n
  | n <= 0 = (False, -1)
  | otherwise = isPowOf2' n 0

isPowOf2' :: Int -> Int -> (Bool, Int)
isPowOf2' 1 m = (True, m)
isPowOf2' n m
  | n `mod` 2 == 0 = isPowOf2' (n `div` 2) (m + 1)
  | otherwise = (False, -1)

-- sL2pO2 :: [String] -> [Int]
