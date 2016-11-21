module Aufgabe5 where

data Digit  = Zero | One | Two deriving (Enum,Show)
type Digits = [Digit]
data Sign   = Pos | Neg deriving (Show)

newtype Numeral = Num (Sign,Digits) deriving (Show)


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
