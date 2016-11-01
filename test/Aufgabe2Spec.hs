module Aufgabe2Spec (spec) where

import Test.Hspec
-- import Test.Hspec.QuickCheck

import Aufgabe2

spec :: Spec
spec = do
  describe "facLst" $ do
    it "n = -42" $ do
      facLst (-42) `shouldBe` []
    it "n = 0" $ do
      facLst 0 `shouldBe` [1]
    it "n = 1" $ do
      facLst 1 `shouldBe` [1, 1]
    it "n = 5" $ do
      facLst 5 `shouldBe` [1, 1, 2, 6, 24, 120]
    it "n = -5" $ do
      facLst (-5) `shouldBe` []
  describe "factsL" $ do
    it "n = 5" $ do
      factsL 5 `shouldBe` [120, 24, 6, 2, 1, 1]
    it "n = -5" $ do
      factsL (-5) `shouldBe` []
  describe "extractNumerals" $ do
    it "empty" $ do
      extractNumerals "" `shouldBe` []
    it "no digits" $ do
      extractNumerals "abcDEFghi" `shouldBe` []
    it "a16B008Lk123n1151248cvK" $ do
      extractNumerals "a16B008Lk1234n1151248cvK"
        `shouldBe` ["16", "008", "1234", "1151248"]
