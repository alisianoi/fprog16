module Aufgabe7 where

data Tree a = Nil | Node a Int (Tree a) (Tree a) deriving (Eq,Ord,Show)

type Multiset a = Tree a
type Mset a = Multiset a

data ThreeValuedBool = TT | FF | Invalid deriving (Eq,Show)
data Order           = Up | Down deriving (Eq,Show)

isMultiset :: Ord a => Tree a -> Bool
isMultiset Nil = True
isMultiset (Node x i l r) =
  if i < 0 then False else isMultisetL l x && isMultisetR r x

isMultisetL :: Ord a => Tree a -> a -> Bool
isMultisetL Nil _ = True
isMultisetL (Node x i l r) y
  | i < 0 || x >= y = False
  | otherwise = isMultisetL l x && isMultisetLR r x y

isMultisetR :: Ord a => Tree a -> a -> Bool
isMultisetR Nil _ = True
isMultisetR (Node x i l r) y
  | i < 0 || x <= y = False
  | otherwise = isMultisetRL l x y && isMultisetR r x

isMultisetLR :: Ord a => Tree a -> a -> a -> Bool
isMultisetLR Nil _ _ = True
isMultisetLR (Node x i l r) y z
  | i < 0 || x <= y || x >= z = False
  | otherwise = isMultisetRL l x y && isMultisetR r x

isMultisetRL :: Ord a => Tree a -> a -> a -> Bool
isMultisetRL Nil _ _ = True
isMultisetRL (Node x i l r) y z
  | i < 0 || x >= y || x <= z = False
  | otherwise = isMultisetL l x && isMultisetLR r x y

isCanonicalMultiset :: Ord a => Tree a -> Bool
isCanonicalMultiset Nil = True
isCanonicalMultiset (Node x i l r) =
  if i <= 0 then False else isCanonicalMultisetL l x && isCanonicalMultisetR r x

isCanonicalMultisetL :: Ord a => Tree a -> a -> Bool
isCanonicalMultisetL Nil _ = True
isCanonicalMultisetL (Node x i l r) y
  | i <= 0 || x >= y = False
  | otherwise = isCanonicalMultisetL l x && isCanonicalMultisetLR r x y

isCanonicalMultisetR :: Ord a => Tree a -> a -> Bool
isCanonicalMultisetR Nil _ = True
isCanonicalMultisetR (Node x i l r) y
  | i <= 0 || x <= y = False
  | otherwise = isCanonicalMultisetRL l x y && isCanonicalMultisetR r x

isCanonicalMultisetLR :: Ord a => Tree a -> a -> a -> Bool
isCanonicalMultisetLR Nil _ _ = True
isCanonicalMultisetLR (Node x i l r) y z
  | i <= 0 || x <= y || x >= z = False
  | otherwise = isCanonicalMultisetRL l x y && isCanonicalMultisetR r x

isCanonicalMultisetRL :: Ord a => Tree a -> a -> a -> Bool
isCanonicalMultisetRL Nil _ _ = True
isCanonicalMultisetRL (Node x i l r) y z
  | i <= 0 || x >= y || x <= z = False
  | otherwise = isCanonicalMultisetL l x && isCanonicalMultisetLR r x y

insert :: Ord a => Multiset a -> a -> Int -> Multiset a
insert Nil x i = Node x i Nil Nil
insert (Node x i l r) y j
  | y == x    = Node x (i + j) l              r
  | y <  x    = Node x i       (insert l y j) r
  | y >  x    = Node x i       l              (insert r y j)
  | otherwise = error "Fix insert"

captox :: (Ord a, Show a) => Int -> Multiset a -> Multiset a
captox _ Nil = Nil
captox x (Node v i l r) = if i < x
  then Node v x (captox x l) (captox x r)
  else Node v i (captox x l) (captox x r)

mkMultiset :: (Ord a, Show a) => Tree a -> Multiset a
mkMultiset tree = captox 0 $ mkMultiset' tree Nil

mkMultiset' :: (Ord a, Show a) => Tree a -> Multiset a -> Multiset a
mkMultiset' Nil mset = mset
mkMultiset' (Node x i l r) mset =
  let lset = mkMultiset' l mset in
    let rset = mkMultiset' r lset in
      insert rset x i

mkCanonicalMultiset :: (Ord a, Show a) => Tree a -> Multiset a
mkCanonicalMultiset tree = captox 1 $ mkMultiset' tree Nil

flatten :: (Ord a, Show a) => Order -> Multiset a -> [(a, Int)]
flatten order mset =
  if isMultiset mset then
    if order == Up then flattenLR mset else flattenRL mset
  else
    []

flattenLR :: (Ord a, Show a) => Multiset a -> [(a, Int)]
flattenLR Nil = []
flattenLR (Node v i l r) = if i <= 0
  then flattenLR l ++ flattenLR r
  else flattenLR l ++ [(v, i)] ++ flattenLR r

flattenRL :: (Ord a, Show a) => Multiset a -> [(a, Int)]
flattenRL Nil = []
flattenRL (Node v i l r) = if i <= 0
  then flattenRL r ++ flattenRL l
  else flattenRL r ++ [(v, i)] ++ flattenRL l

isElement :: Ord a => a -> Multiset a -> Int
isElement v mset = if isMultiset mset then isElement' v mset else -1

isElement' :: Ord a => a -> Multiset a -> Int
isElement' _ Nil = 0
isElement' v (Node w i l r)
  | v == w = i
  | v <  w = isElement' v l
  | v >  w = isElement' v r
  | otherwise = error "Fix isElement'"

isSubset :: Ord a => Multiset a -> Multiset a -> ThreeValuedBool
isSubset m1 m2 =
  if isMultiset m1 && isMultiset m2 then isSubset' m1 m2 else Invalid

isSubset' :: Ord a => Multiset a -> Multiset a -> ThreeValuedBool
isSubset' Nil _ = TT
isSubset' (Node x i l r) m2
  | i <= isElement' x m2 =
    if isSubset' l m2 == TT && isSubset' r m2 == TT then TT else FF
  | otherwise = FF

join :: (Ord a, Show a) => Multiset a -> Multiset a -> Multiset a
join m1 m2 =
  if isMultiset m1 && isMultiset m2
  then mkCanonicalMultiset $ join' m1 m2 else Nil

join' :: (Ord a, Show a) => Multiset a -> Multiset a -> Multiset a
join' Nil m2 = m2
join' (Node v i l r) m2 =
  let lm = join' l m2 in
    let rm = join' r lm in
      insert rm v i

meet :: (Ord a, Show a) => Multiset a -> Multiset a -> Multiset a
meet m1 m2 =
  if isMultiset m1 && isMultiset m2
  then mkCanonicalMultiset $ meet' Nil m1 m2 else Nil

count' :: (Ord a, Show a) => Mset a -> a -> Int
count' Nil _ = 0
count' (Node v i l r) w
  | v == w = i
  | v <  w = count' r w
  | v >  w = count' l w
  | otherwise = error "Fix count'"

meet' :: (Ord a, Show a) => Mset a -> Mset a -> Mset a -> Mset a
meet' m0 Nil _ = m0
meet' m0 (Node v i l r) m2 =
  let ml = meet' m0 l m2 in
    let mr = meet' ml r m2 in
      let j = count' m2 v in
        if min i j <= 0 then mr else insert mr v $ min i j

subtract :: (Ord a, Show a) => Multiset a -> Multiset a -> Multiset a
subtract m1 m2 =
  if isMultiset m1 && isMultiset m2
  then mkCanonicalMultiset $ subtract' Nil m1 m2 else Nil

subtract' :: (Ord a, Show a) => Mset a -> Mset a -> Mset a -> Mset a
subtract' m0 Nil _ = m0
subtract' m0 (Node v i l r) m2 =
  let ml = subtract' m0 l m2 in
    let mr = subtract' ml r m2 in
      let j = count' m2 v in
        if i - j <= 0 then mr else insert mr v $ i - j
