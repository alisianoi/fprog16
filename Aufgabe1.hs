facInv :: Integer -> Integer
facInv m
  | m <= 0 = -1
  | otherwise = facInv' m 1

facInv' :: Integer -> Integer -> Integer
facInv' 1 x = x - 1
facInv' m x
  | mod m x == 0 = facInv' (div m x) (x + 1)
  | otherwise = -1
