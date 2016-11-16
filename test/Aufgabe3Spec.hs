module Aufgabe3Spec (spec) where

import Test.Hspec

import Aufgabe3

spec :: Spec
spec = do
  describe "canonize" $ do
    it "Num (Neg, [Zero])" $ do
      canonize (Num (Neg, [Zero]))
        `shouldBe` Num (Pos, [Zero])
    it "Num (Pos, [Zero, Zero, Zero])" $ do
      canonize (Num (Pos, [Zero, Zero, Zero]))
        `shouldBe` Num (Pos, [Zero])
    it "Num (Neg, [Zero, Zero])" $ do
      canonize (Num (Neg, [Zero, Zero]))
        `shouldBe` Num (Pos, [Zero])
    it "Num (Neg, [Zero, Zero, Zero])" $ do
      canonize (Num (Neg, [Zero, Zero, Zero]))
        `shouldBe` Num (Pos, [Zero])
    it "Num (Neg, [Zero, Zero, Two, One, Zero])" $ do
      canonize (Num (Neg, [Zero, Zero, Two, One, Zero]))
        `shouldBe` Num (Neg, [Two, One, Zero])
  describe "int2num" $ do
    it "0" $ do
      int2num 0 `shouldBe` Num (Pos, [Zero])
    it "-0" $ do
      int2num (-0) `shouldBe` Num (Pos, [Zero])
    it "1" $ do
      int2num 1 `shouldBe` Num (Pos, [One])
    it "-1" $ do
      int2num (-1) `shouldBe` Num (Neg, [One])
    it "2" $ do
      int2num 2 `shouldBe` Num (Pos, [Two])
    it "-2" $ do
      int2num (-2) `shouldBe` Num (Neg, [Two])
  describe "num2int" $ do
    it "Num (Pos, [One, Zero])" $ do
      num2int (Num (Pos, [One, Zero])) `shouldBe` 3
    it "Num (Neg, [One, Zero])" $ do
      num2int (Num (Neg, [One, Zero])) `shouldBe` -3
    it "Num (Neg, [One, Zero, One])" $ do
      num2int (Num (Neg, [One, Zero, One])) `shouldBe` -10
    it "Num (Neg, [Zero, Zero, Zero, One, Zero, One])" $ do
      num2int (Num (Neg, [Zero, Zero, Zero, One, Zero, One]))
        `shouldBe` -10
    it "Num (Neg, [Zero])" $ do
      num2int (Num (Neg, [Zero])) `shouldBe` 0
  describe "inc" $ do
    it "Num (Pos, [Zero])" $ do
      inc (Num (Pos, [Zero])) `shouldBe` Num (Pos, [One])
    it "Num (Pos, [One])" $ do
      inc (Num (Pos, [One])) `shouldBe` Num (Pos, [Two])
    it "Num (Pos, [Two])" $ do
      inc (Num (Pos, [Two])) `shouldBe` Num (Pos, [One, Zero])
    it "Num (Pos, [Two, Two, Two])" $ do
      inc (Num (Pos, [Two, Two, Two]))
        `shouldBe` Num (Pos, [One, Zero, Zero, Zero])
    it "Num (Neg, [One])" $ do
      inc (Num (Neg, [One]))
        `shouldBe` Num (Pos, [Zero])
    it "Num (Neg, [Two])" $ do
      inc (Num (Neg, [Two]))
        `shouldBe` Num (Neg, [One])
    it "Num (Neg, [One, Zero])" $ do
      inc (Num (Neg, [One, Zero]))
        `shouldBe` Num (Neg, [Two])
    it "Num (Neg, [One, Zero, Zero])" $ do
      inc (Num (Neg, [One, Zero, Zero]))
        `shouldBe` Num (Neg, [Two, Two])
  describe "dec" $ do
    it "Num (Pos, [Zero])" $ do
      dec (Num (Pos, [Zero]))
        `shouldBe` Num (Neg, [One])
    it "Num (Pos, [One])" $ do
      dec (Num (Pos, [One]))
        `shouldBe` Num (Pos, [Zero])
    it "Num (Pos [Two])" $ do
      dec (Num (Pos, [Two]))
        `shouldBe` Num (Pos, [One])
    it "Num (Pos, [One, Zero])" $ do
      dec (Num (Pos, [One, Zero]))
        `shouldBe` Num (Pos, [Two])
    it "Num (Pos, [One, Zero, Zero])" $ do
      dec (Num (Pos, [One, Zero, Zero]))
        `shouldBe` Num (Pos, [Two, Two])
  describe "numAdd" $ do
    it "0 + 0" $ do
      numAdd (Num (Pos, [Zero])) (Num (Pos, [Zero]))
        `shouldBe` (Num (Pos, [Zero]))
    it "2 + 1" $ do
      numAdd (Num (Pos, [Two])) (Num (Pos, [One]))
        `shouldBe` (Num (Pos, [One, Zero]))
    it "1 + 2" $ do
      numAdd (Num (Pos, [One])) (Num (Pos, [Two]))
        `shouldBe` (Num (Pos, [One, Zero]))
    it "1 - 2" $ do
      numAdd (Num (Pos, [One])) (Num (Neg, [Two]))
        `shouldBe` (Num (Neg, [One]))
    it "-2 + 1" $ do
      numAdd (Num (Neg, [Two])) (Num (Pos, [One]))
        `shouldBe` (Num (Neg, [One]))
    it "2 - 1" $ do
      numAdd (Num (Pos, [Two])) (Num (Neg, [One]))
        `shouldBe` (Num (Pos, [One]))
    it "-1 + 2" $ do
      numAdd (Num (Neg, [One])) (Num (Pos, [Two]))
        `shouldBe` (Num (Pos, [One]))
  describe "numMult" $ do
    it "42 * 0" $ do
      numMult (int2num 42) (int2num 0) `shouldBe` int2num 0
    it "0 * 42" $ do
      numMult (int2num 0) (int2num 42) `shouldBe` int2num 0
    it "(-42) * 0" $ do
      numMult (int2num (-42)) (int2num 0) `shouldBe` int2num 0
    it "0 * (-42)" $ do
      numMult (int2num 0) (int2num (-42)) `shouldBe` int2num 0
    it "1 * 42" $ do
      numMult (int2num 1) (int2num 42) `shouldBe` int2num 42
    it "42 * 1" $ do
      numMult (int2num 42) (int2num 1) `shouldBe` int2num 42
    it "1 * (-42)" $ do
      numMult (int2num 1) (int2num (-42)) `shouldBe` int2num (-42)
    it "(-42) * 1" $ do
      numMult (int2num (-42)) (int2num 1) `shouldBe` int2num (-42)
    it "2 * 2" $ do
      numMult (int2num 2) (int2num 2) `shouldBe` int2num 4
    it "(-2) * 2" $ do
      numMult (int2num (-2)) (int2num 2) `shouldBe` int2num (-4)
    it "2 * -2" $ do
      numMult (int2num 2) (int2num (-2)) `shouldBe` int2num (-4)
    it "-2 * -2" $ do
      numMult (int2num (-2)) (int2num (-2)) `shouldBe` int2num 4
  describe "int2num -> num2int" $ do
    it "from 0 to hero" $ do
      map (num2int . int2num) [(-100),(-99)..100] `shouldBe` [(-100),(-99)..100]
  describe "inc" $ do
    it "is strong" $ do
      map (num2int . inc . int2num) [(-100),(-99)..100] `shouldBe` [(-99),(-98)..101]
  describe "dec" $ do
    it "is strong" $ do
      map (num2int . dec . int2num) [(-100),(-99)..100] `shouldBe` [(-101),(-100)..99]

