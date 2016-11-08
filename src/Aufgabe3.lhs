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
unmatched patterns otherwise
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
