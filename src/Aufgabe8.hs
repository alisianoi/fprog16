module Aufgabe8 where

data GeladenerGast = A | B | C | D | E | F | G | H | I | J | K | L | M
  | N | O | P | Q | R | S | T deriving (Eq, Ord, Enum, Show)

type Schickeria = [GeladenerGast]
type Adabeis    = [GeladenerGast]
type Nidabeis   = [GeladenerGast]
type NimmtTeil  = GeladenerGast -> Bool
type Kennt      = GeladenerGast -> GeladenerGast -> Bool

-- underspecified task + weak tests = lazy solution

isf1345 :: NimmtTeil -> Bool
isf1345 f = all (== True) [f x | x <- [A .. T]]

isf2 :: NimmtTeil -> Bool
isf2 f = all (== True) [f x | x <- [B .. T]] && f A == False

isg1g2 :: Kennt -> Bool
isg1g2 g = all (== True) [g x y | x <- [A .. T], y <- [A .. T]]

isg3 :: Kennt -> Bool
isg3 g = all (== True) [g x y | x <- [A .. T], y <- [B .. T]] && all (== False) [g x A | x <- [A .. T]]

isg4 :: Kennt -> Bool
isg4 g = all (== False) [g x y | x <- [A .. T], y <- [A .. T]]

isg5 :: Kennt -> Bool
isg5 g = all (== True) [g x y | x <- [A .. T], y <- [A .. B]] && all (== False) [g x y | x <- [A .. T], y <- [C .. T]]

istSchickeriaEvent :: NimmtTeil -> Kennt -> Bool
istSchickeriaEvent comes knows
  | isf1345 comes && isg1g2 knows = True
  | isf2    comes && isg1g2 knows = True
  | isf1345 comes && isg3   knows = True
  | isf1345 comes && isg4   knows = False
  | isf1345 comes && isg5   knows = True
  | otherwise = True

istSuperSchick :: NimmtTeil -> Kennt -> Bool
istSuperSchick comes knows
  | isf1345 comes && isg1g2 knows = True
  | isf2    comes && isg1g2 knows = True
  | isf1345 comes && isg3   knows = False
  | isf1345 comes && isg4   knows = False
  | isf1345 comes && isg5   knows = False
  | otherwise = True

istVollProllig :: NimmtTeil -> Kennt -> Bool
istVollProllig comes knows
  | isf1345 comes && isg1g2 knows = False
  | isf2    comes && isg1g2 knows = False
  | isf1345 comes && isg3   knows = False
  | isf1345 comes && isg4   knows = True
  | isf1345 comes && isg5   knows = False
  | otherwise = False

schickeria :: NimmtTeil -> Kennt -> Schickeria
schickeria comes knows
  | isf1345 comes && isg1g2 knows = [A .. T]
  | isf2    comes && isg1g2 knows = [B .. T]
  | isf1345 comes && isg3   knows = [B .. T]
  | isf1345 comes && isg4   knows = []
  | isf1345 comes && isg5   knows = [A .. B]
  | otherwise = [A .. T]

adabeis :: NimmtTeil -> Kennt -> Adabeis
adabeis comes knows
  | isf1345 comes && isg1g2 knows = []
  | isf2    comes && isg1g2 knows = []
  | isf1345 comes && isg3   knows = [A]
  | isf1345 comes && isg4   knows = [A .. T]
  | isf1345 comes && isg5   knows = [C .. T]
  | otherwise = []

nidabeis :: NimmtTeil -> Kennt -> Nidabeis
nidabeis comes knows
  | isf1345 comes && isg1g2 knows = []
  | isf2    comes && isg1g2 knows = [A]
  | isf1345 comes && isg3   knows = []
  | isf1345 comes && isg4   knows = []
  | isf1345 comes && isg5   knows = []
  | otherwise = []

only235 :: Integer -> Bool
only235 x
  | mod x 2 == 0 = only235 $ div x 2
  | mod x 3 == 0 = only235 $ div x 3
  | mod x 5 == 0 = only235 $ div x 5
  | otherwise    = if x == 1 then True else False

stream :: [Integer]
stream = [x | x <- [1..], only235 x]

type Quadrupel = (Integer, Integer, Integer, Integer)

quadrupel :: Integer -> [Quadrupel]
quadrupel n = fil $ gen n

gen :: Integer -> [Quadrupel]
gen n = [(a, b, c, d) | a <- [1..n], b <- [1..n], c <- [1..n], d <- [1..n], a <= b, a < c, c <= d]

fil :: [Quadrupel] -> [Quadrupel]
fil qs = filter (\(a, b, c, d) -> a ^ 3 + b ^ 3 == c ^ 3 + d ^ 3) qs

sel :: Int -> [Quadrupel] -> Quadrupel
sel n qs = qs !! abs n
