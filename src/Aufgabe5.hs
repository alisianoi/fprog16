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

inc :: Numeral -> Numeral
inc n = canonize $ inc' $ canonize n

dec :: Numeral -> Numeral
dec n = canonize $ dec' $ canonize n

inc' :: Numeral -> Numeral
inc' (Num (Neg, [One])) = Num (Pos, [Zero])
inc' (Num (Pos, ds))    = Num (Pos, xfwd ds)
inc' (Num (Neg, ds))    = Num (Neg, xbwd ds)

dec' :: Numeral -> Numeral
dec' (Num (Pos, [Zero])) = Num (Neg, [One])
dec' (Num (Pos, ds))     = Num (Pos, xbwd ds)
dec' (Num (Neg, ds))     = Num (Neg, xfwd ds)

xfwd :: Digits -> Digits
xfwd xs = reverse $ fwd' $ reverse xs

xbwd :: Digits -> Digits
xbwd xs = reverse $ bwd' $ reverse xs

fwd' :: Digits -> Digits
fwd' [] = [One]
fwd' (d:ds)
  | d == Zero = One : ds
  | d == One  = Two : ds
  | otherwise = Zero : fwd' ds

bwd' :: Digits -> Digits
bwd' [] = [Zero]
bwd' (d:ds)
  | d == One  = Zero : ds
  | d == Two  = One  : ds
  | otherwise = Two  : bwd' ds

numAdd :: Numeral -> Numeral -> Numeral
numAdd x y = canonize $ numAdd' (canonize x) (canonize y)

numAdd' :: Numeral -> Numeral -> Numeral
numAdd' x y = numAdd'' (Num (Pos, [Zero])) x y

numAdd'' :: Numeral -> Numeral -> Numeral -> Numeral
numAdd'' t x@(Num (sx, dx)) y@(Num (sy, dy))
  | sx == Pos && dx /= [Zero] = numAdd'' (inc t) (dec x) y
  | sx == Neg                 = numAdd'' (dec t) (inc x) y
  | sy == Pos && dy /= [Zero] = numAdd'' (inc t) x (dec y)
  | sy == Neg                 = numAdd'' (dec t) x (inc y)
  | otherwise = t

numMult :: Numeral -> Numeral -> Numeral
numMult x y = canonize $ numMult' (canonize x) (canonize y)

numMult' :: Numeral -> Numeral -> Numeral
numMult' x@(Num (sx, dx)) y@(Num (sy, dy))
  | sx == Pos && dx == [Zero] = (Num (Pos, [Zero]))
  | sy == Pos && dy == [Zero] = (Num (Pos, [Zero]))
  | sx == Pos && sy == Pos    = numMult'' Pos x               y
  | sx == Neg && sy == Neg    = numMult'' Pos (Num (Pos, dx)) (Num (Pos, dy))
  | sx == Pos && sy == Neg    = numMult'' Neg x               (Num (Pos, dy))
  | sx == Neg && sy == Pos    = numMult'' Neg (Num (Pos, dx)) y
  | otherwise                 = error "(Num (_, _)) (Num (_, _))?"

numMult'' :: Sign -> Numeral -> Numeral -> Numeral
numMult'' s x y = numMult''' s x x y

numMult''' :: Sign -> Numeral -> Numeral -> Numeral -> Numeral
numMult''' s (Num (_, tx)) _ (Num (Pos, [One])) = (Num (s, tx))
numMult''' s t x y = numMult''' s (numAdd t x) x (dec y)

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
  show Zero = show (0 :: Integer)
  show One  = show (1 :: Integer)
  show Two  = show (2 :: Integer)

instance Show Sign where
  show Pos = "+"
  show Neg = "-"

instance Show Numeral where
  show n = showCanon $ canonize n

showCanon :: Numeral -> String
showCanon (Num (s, ds)) = (show s) ++ (concat(intersperse "" $ map (show) ds))

instance Ord Digit where
  compare l r
    | l == r = EQ
    | l == Zero = LT
    | l == One && r == Two = LT
    | otherwise = GT

instance Ord Numeral where
  compare (Num (Neg, _)) (Num (Pos, _)) = LT
  compare (Num (Pos, _)) (Num (Neg, _)) = GT
  compare (Num (ls, lds)) (Num (rs, rds)) =
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


instance Num Numeral where
  negate (Num (s, ds)) =
    if s == Neg then (Num (Pos, ds)) else canonize (Num (Neg, ds))
  abs (Num (_, ds)) = (Num (Pos, ds))
  signum (Num (s, _)) = (Num (s, [One]))
  (-) l r = numAdd l $ negate r
  (+) l r = numAdd l r
  (*) l r = numMult l r
  fromInteger i = int2num i
