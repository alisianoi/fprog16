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
    it "Num (Neg, [One, Zero, One])" $ do
      num2int (Num (Neg, [One, Zero, One])) `shouldBe` -10
    it "Num (Neg, [Zero, Zero, Zero, One, Zero, One])" $ do
      num2int (Num (Neg, [Zero, Zero, Zero, One, Zero, One]))
        `shouldBe` -10
    it "Num (Neg, [Zero])" $ do
      num2int (Num (Neg, [Zero])) `shouldBe` 0
