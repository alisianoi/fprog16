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
  | r == 0 = Zero : int2num' q
  | r == 1 = One  : int2num' q
  | r == 2 = Two  : int2num' q
  where
    q = div n 3
    r = mod n 3
\end{code}
