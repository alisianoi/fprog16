module Aufgabe5 where

import Data.List

data Digit  = Zero | One | Two deriving (Enum)
type Digits = [Digit]
data Sign   = Pos | Neg

newtype Numeral = Num (Sign,Digits)


-- probably remove from here
canonize :: Numeral -> Numeral
canonize (Num (_, []))         = error "Invalid Argument"
canonize (Num (Neg, [Zero]))   = Num (Pos, [Zero])
canonize (Num (s  , t@(x:xs)))
  | x == Zero && not (null xs) = canonize (Num (s, xs))
  | otherwise                  = Num (s, t)

int2num :: Integer -> Numeral
int2num n
  | n < 0     = canonize (Num (Neg, f (-n)))
  | otherwise = canonize (Num (Pos, f   n ))
  where
    f x = reverse $ int2num' x

int2num' :: Integer -> Digits
int2num' 0 = [Zero]
int2num' n
  | r == 0    = Zero : int2num' q
  | r == 1    = One  : int2num' q
  | otherwise = Two  : int2num' q
  where
    q = div n 3
    r = mod n 3

num2int :: Numeral -> Integer
num2int n = num2int' $ canonize n

num2int' :: Numeral -> Integer
num2int' (Num (s, ds))
  | s == Neg = -f ds
  | otherwise = f ds
  where f xs = num2int'' 0 $ xs


num2int'' :: Integer -> Digits -> Integer
num2int'' total [] = total
num2int'' total (d:ds)
  | d == Zero = num2int'' (3 * total + 0) ds
  | d == One  = num2int'' (3 * total + 1) ds
  | otherwise = num2int'' (3 * total + 2) ds
-- probably remove up to here


-- Digit, Sign, Numeral

instance Eq Digit where
  Zero == Zero = True
  One  == One  = True
  Two  == Two  = True
  _    == _    = False

instance Eq Sign where
  Pos == Pos = True
  Neg == Neg = True
  _   == _   = False

instance Eq Numeral where
  Num (ls, lds) == Num (rs, rds)
    | not $ ls == rs = False
    | otherwise = all (== True) $ zipWith (==) lds rds

instance Show Digit where
  show Zero = show 0
  show One  = show 1
  show Two  = show 2

instance Show Sign where
  show Pos = "+"
  show Neg = "-"

instance Show Numeral where
  show n = showCanon $ canonize n

showCanon :: Numeral -> String
showCanon (Num (s, ds)) = (show s) ++ (intercalate "" $ map (show) ds)

instance Ord Digit where
  compare l r
    | l == r = EQ
    | l == Zero = LT
    | l == One && r == Two = LT
    | otherwise = GT

instance Ord Numeral where
  compare (Num (Neg, _)) (Num (Pos, _)) = LT
  compare (Num (Pos, _)) (Num (Neg, _)) = GT
  compare l@(Num (ls, lds)) r@(Num (rs, rds)) =
    if ls == Neg && rs == Neg then
      if result == LT then GT else if result == GT then LT else EQ
    else if ls == Pos && rs == Pos then
      result
    else
      error "You have mixed up the signs in compare for Numeral"
    where result = foo lds rds

foo :: Digits -> Digits -> Ordering
foo [] [] = EQ
foo [] _  = LT
foo _  [] = GT
foo xs ys =
  if result == EQ then compare x y else result
  where x      = tail xs
        y      = tail ys
        result = foo (init xs) (init ys)
