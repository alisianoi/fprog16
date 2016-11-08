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
