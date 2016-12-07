module Aufgabe7 where

data Tree a = Nil | Node a Int (Tree a) (Tree a) deriving (Eq,Ord,Show)

type Multiset a = Tree a

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
captox x Nil = Nil
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
isSubset' Nil m2 = TT
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

bigtree0 :: Tree Integer
bigtree0 = (
  Node 128 1 (
      Node 32 1 (Node 16 1 Nil Nil) (Node 64 1 Nil Nil)
      ) (
      Node 512 1 (Node 256 1 Nil Nil) (Node 1024 1 Nil Nil)
      )
  )

bigtree1 :: Tree Integer
bigtree1 = (Node 2048 (-1) bigtree0 bigtree0)
