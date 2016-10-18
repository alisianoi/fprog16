module Aufgabe1 where

import Data.Char (isDigit)

facInv :: Integer -> Integer
facInv m
  | m <= 0 = -1
  | otherwise = facInv' m 1

facInv' :: Integer -> Integer -> Integer
facInv' 1 x = x - 1
facInv' m x
  | mod m x == 0 = facInv' (div m x) (x + 1)
  | otherwise = -1

extractDigits :: String -> String
extractDigits [] = []
extractDigits (x:xs)
  | isDigit x = x : extractDigits xs
  | otherwise = extractDigits xs

convert :: String -> Integer
convert s
  | extractDigits s == "" = 0
  | otherwise = read $ extractDigits s :: Integer

isPrime :: Integer -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = null [d | d <- [2..n - 1], mod n d == 0]

findLeftMostPrime :: String -> Int -> Integer
findLeftMostPrime _ 0 = 0
findLeftMostPrime s n = findLeftMostPrime' (extractDigits s) n

findLeftMostPrime' :: String -> Int -> Integer
findLeftMostPrime' s n
  | length s < n = 0
findLeftMostPrime' s@(x:xs) n
  | convert [x] == 0 = findLeftMostPrime' xs n
  | isPrime $ convert $ take n s = convert $ take n s
  | otherwise = findLeftMostPrime' xs n

findAllPrimes :: String -> Int -> [Integer]
findAllPrimes _ 0 = []
findAllPrimes s n = findAllPrimes' (extractDigits s) n

findAllPrimes' :: String -> Int -> [Integer]
findAllPrimes' s n
  | length s < n = []
findAllPrimes' s@(x:xs) n
  | convert [x] == 0 = findAllPrimes' xs n
  | isPrime $ convert $ take n s =
    (convert $ take n s) : findAllPrimes' xs n
  | otherwise = findAllPrimes' xs n
