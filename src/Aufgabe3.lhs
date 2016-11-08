\begin{code}
module Aufgabe3 where
\end{code}

\begin{code}
data Digit  = Zero | One | Two deriving (Eq,Enum,Show)
type Digits = [Digit]
data Sign   = Pos | Neg deriving (Eq,Show)

newtype Numeral = Num (Sign,Digits) deriving (Eq,Show)
\end{code}


Treat Num (_, []) with an error because cabal build complains about
unmatched patterns otherwise. Use this error further in num2int.
\begin{code}
canonize :: Numeral -> Numeral
canonize (Num (_, []))         = error "Invalid Argument"
canonize (Num (Neg, [Zero]))   = Num (Pos, [Zero])
canonize (Num (s  , t@(x:xs)))
  | x == Zero && not (null xs) = canonize (Num (s, xs))
  | otherwise                  = Num (s, t)
\end{code}

\begin{code}
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
\end{code}

Implementing it with a foldl did not look "good", so just plain guards
and helper functions to the rescue once again.

\begin{code}
num2int :: Numeral -> Integer
num2int n = num2int' $ canonize n

num2int' :: Numeral -> Integer
num2int' (Num (s, ds))
  | s == Neg = -f ds
  | otherwise = f ds
  where f xs = num2int'' 0 $ reverse xs


num2int'' :: Integer -> Digits -> Integer
num2int'' total [] = total
num2int'' total (d:ds)
  | d == Zero = num2int'' (3 * total + 0) ds
  | d == One  = num2int'' (3 * total + 1) ds
  | otherwise = num2int'' (3 * total + 2) ds
\end{code}

\begin{code}
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
\end{code}

\begin{code}
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
\end{code}
