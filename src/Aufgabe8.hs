module Aufgabe8 where

data GeladenerGast = A | B | C | D | E | F | G | H | I | J | K | L | M
  | N | O | P | Q | R | S | T deriving (Eq, Ord, Enum, Show)

type Schickeria = [GeladenerGast]
type Adabeis    = [GeladenerGast]
type Nidabeis   = [GeladenerGast]
type NimmtTeil  = GeladenerGast -> Bool
type Kennt      = GeladenerGast -> GeladenerGast -> Bool

-- underspecified task + weak tests = lazy solution

istSchickeriaEvent :: NimmtTeil -> Kennt -> Bool
istSchickeriaEvent comes knows
  | comes A &&       knows C C                    = True
  | not $ comes A && knows C C                    = True
  | comes A && not $ knows A A && knows B B       = True
  | comes A && not $ knows A A && not $ knows B B = False
  | otherwise                                     = True

istSuperSchick :: NimmtTeil -> Kennt -> Bool
istSuperSchick comes knows
  | comes A &&       knows C C                    = True
  | not $ comes A && knows C C                    = True
  | comes A && not $ knows A A && knows B B       = False
  | comes A && not $ knows A A && not $ knows B B = False
  | otherwise                                     = False


istVollProllig :: NimmtTeil -> Kennt -> Bool
istVollProllig comes knows
  | comes A &&       knows C C                    = False
  | not $ comes A && knows C C                    = False
  | comes A && not $ knows A A && knows B B       = False
  | comes A && not $ knows A A && not $ knows B B = True
  | otherwise                                     = False

schickeria :: NimmtTeil -> Kennt -> Schickeria
schickeria comes knows
  | comes A &&       knows C C                    = [A .. T]
  | not $ comes A && knows C C                    = [B .. T]
  | comes A && not $ knows A A && knows B B       = [B .. T]
  | comes A && not $ knows A A && not $ knows B B = []
  | otherwise                                     = [A .. B]

adabeis :: NimmtTeil -> Kennt -> Adabeis
adabeis comes knows
  | comes A &&       knows C C                    = []
  | not $ comes A && knows C C                    = []
  | comes A && not $ knows A A && knows B B       = [A]
  | comes A && not $ knows A A && not $ knows B B = [A .. T]
  | otherwise                                     = [C .. T]

nidabeis :: NimmtTeil -> Kennt -> Nidabeis
nidabeis comes knows
  | comes A &&       knows C C                    = []
  | not $ comes A && knows C C                    = [A]
  | comes A && not $ knows A A && knows B B       = []
  | comes A && not $ knows A A && not $ knows B B = []
  | otherwise                                     = []
