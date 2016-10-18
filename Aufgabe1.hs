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
  | otherwise = (read $ extractDigits s) :: Integer
