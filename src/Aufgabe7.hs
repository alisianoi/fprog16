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


bigtree0 :: Tree Integer
bigtree0 = (
  Node 128 1 (
      Node 32 1 (Node 16 1 Nil Nil) (Node 64 1 Nil Nil)
      ) (
      Node 512 1 (Node 256 1 Nil Nil) (Node 1024 1 Nil Nil)
      )
  )
