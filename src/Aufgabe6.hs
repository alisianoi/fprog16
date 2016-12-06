module Aufgabe6 where

data Stack a = Stk [a]| NoStk deriving (Eq,Show)
data Maybe a = Just a | Nothing deriving (Eq,Show)

empty :: (Eq a,Show a) => Stack a
empty = Stk []

isEmpty :: (Eq a,Show a) => Stack a -> Bool
isEmpty (Stk []) = True
isEmpty _        = False

top1 :: (Eq a,Show a) => Stack a -> a
top1 NoStk        = error "Invalid Argument"
top1 (Stk [])     = error "Invalid Argument"
top1 (Stk (l:ls)) = l

top2 :: (Eq a,Show a) => Stack a -> Aufgabe6.Maybe a
top2 NoStk        = Aufgabe6.Nothing
top2 (Stk [])     = Aufgabe6.Nothing
top2 (Stk (l:ls)) = Aufgabe6.Just l

pop :: (Eq a,Show a) => Stack a -> Stack a
pop NoStk    = NoStk
pop (Stk []) = NoStk
pop (Stk l)  = Stk $ tail l

push :: (Eq a,Show a) => a -> Stack a -> Stack a
push _ NoStk    = NoStk
push l (Stk ls) = Stk (l:ls)

-- task 3/task 5 data begins here
data Digit  = Zero | One | Two deriving (Eq,Enum,Show)
type Digits = [Digit]
data Sign   = Pos | Neg deriving (Eq,Show)

newtype Numeral = Num (Sign,Digits) deriving (Eq,Show)

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
-- task 3/task 5 data ends here

data Operator = Plus | Times | Minus deriving (Eq,Show)

data Variable = A | B | C deriving (Eq,Show)

data Expression = Cst Numeral
  | Var Variable
  | Exp Expression Expression Operator
  deriving (Eq,Show)

type State = Variable -> Numeral

eval :: Expression -> State -> Integer
eval (Cst c) _ = num2int c
eval (Var v) s = num2int $ s v
eval (Exp lft rgt op) s
  | op == Plus =  (+) (eval lft s) (eval rgt s)
  | op == Minus = (-) (eval lft s) (eval rgt s)
  | op == Times = (*) (eval lft s) (eval rgt s)
  | otherwise   = error "Unknown operator in expression"

data CVO = Cop Numeral
  | Vop Char
  | OpPlus
  | OpTimes
  | OpMinus
  deriving (Eq,Show)

type Expr = [CVO]
type State2 = Char -> Numeral

eval2 :: Expr -> State2 -> Integer
eval2 e s = eval2' e s empty

eval2' :: Expr -> State2 -> Stack Integer -> Integer
eval2' [] _ t = top1 t
eval2' ((Cop e):es) s t = eval2' es s $ push (num2int e) t
eval2' ((Vop e):es) s t = eval2' es s $ push (num2int $ s e) t
eval2' (OpPlus:es)  s t = eval2' es s
  $ push ((+) (top1 $ pop t) $ top1 t) $ pop $ pop t
eval2' (OpMinus:es)  s t = eval2' es s
  $ push ((-) (top1 $ pop t) $ top1 t) $ pop $ pop t
eval2' (OpTimes:es)  s t = eval2' es s
  $ push ((*) (top1 $ pop t) $ top1 t) $ pop $ pop t
