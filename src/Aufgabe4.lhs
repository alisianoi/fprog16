\begin{code}
module Aufgabe4 where
\end{code}

\begin{code}
data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

data Order = Up | Down deriving (Eq, Show)
\end{code}

\begin{code}
nil :: Tree a
nil = Nil

isNilTree :: Tree a -> Bool
isNilTree Nil = True
isNilTree _   = False

isNodeTree :: Tree a -> Bool
isNodeTree t = not $ isNilTree t

leftSubTree :: Tree a -> Tree a
leftSubTree Nil = error "Empty tree as Argument"
leftSubTree (Node _ lft _) = lft

rightSubTree :: Tree a -> Tree a
rightSubTree Nil = error "Empty tree as Argument"
rightSubTree (Node _ _ rgt) = rgt

treeValue :: Tree a -> a
treeValue Nil = error "Empty tree as Argument"
treeValue (Node val _ _) = val

isValueOf :: Eq a => a -> Tree a -> Bool
isValueOf _ Nil = False
isValueOf v (Node x l r) = v == x || isValueOf v l || isValueOf v r

isOrderedTree :: Ord a => Tree a -> Bool
isOrderedTree Nil = True
isOrderedTree (Node x l r) = isOrdL x l && isOrdR x r

isOrdL :: Ord a => a -> Tree a -> Bool
isOrdL _ Nil = True
isOrdL x (Node v l r)
  | x <= v    = False
  | otherwise = isOrdL v l && isOrdLR x v r

isOrdR :: Ord a => a -> Tree a -> Bool
isOrdR _ Nil = True
isOrdR x (Node v l r)
  | v <= x    = False
  | otherwise = isOrdRL x v l && isOrdR v r

isOrdLR :: Ord a => a -> a -> Tree a -> Bool
isOrdLR _ _ Nil = True
isOrdLR x v (Node w l r)
  | w >= x || w <= v = False
  | otherwise        = isOrdR w r && isOrdRL v w l

isOrdRL :: Ord a => a -> a -> Tree a -> Bool
isOrdRL _ _ Nil = True
isOrdRL x v (Node w l r)
  | w <= x || x >= v = False
  | otherwise        = isOrdL w l && isOrdLR v w r

insert :: Ord a => a -> Tree a -> Tree a
insert x t
  | not $ isOrderedTree t = error "Argument Tree not Ordered"
  | otherwise = insert' x t

insert' :: Ord a => a -> Tree a -> Tree a
insert' x Nil = Node x Nil Nil
insert' x t@(Node val lft rgt)
  | x == val  = t
  | x < val   = Node val (insert' x lft) rgt
  | otherwise = Node val lft (insert' x rgt)
\end{code}

\begin{code}
delete :: Ord a => a -> Tree a -> Tree a
delete x t
  | not $ isOrderedTree t = error "Argument Tree not Ordered"
  | otherwise = delete' x t

delete' :: Ord a => a -> Tree a -> Tree a
delete' _ Nil = Nil
delete' v t@(Node x l r)
  | v < x     = (Node x (delete' v l) r)
  | v > x     = (Node x l (delete' v r))
  | otherwise = delete'' t

delete'' :: Tree a -> Tree a
delete'' Nil = error "delete'' never sees Nil"
delete'' (Node _ Nil Nil) = Nil
delete'' (Node _ l Nil) = l
delete'' (Node _ Nil r) = r
delete'' (Node _ l r) = (Node x' l' r)
  where (x', l') = delete''' l

delete''' :: Tree a -> (a, Tree a)
delete''' Nil = error "delete''' never sees Nil"
delete''' (Node x l Nil) = (x, l)
delete''' (Node x l r)   = (x', Node x l r')
  where (x', r') = delete''' r
\end{code}

\begin{code}
flatten :: Ord a => Order -> Tree a -> [a]
flatten o t
  | not $ isOrderedTree t = error "Argument Tree not Ordered"
  | otherwise = flatten' o t

flatten' :: Ord a => Order -> Tree a -> [a]
flatten' _ Nil = []
flatten' o (Node x l r)
  | o == Up = (flatten' o l) ++ [x] ++ (flatten' o r)
  | otherwise = (flatten' o r) ++ [x] ++ (flatten' o l)
\end{code}
