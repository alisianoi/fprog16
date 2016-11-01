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
  describe "isPowOf2" $ do
    it "negative" $ do
      isPowOf2 (-42) `shouldBe` (False, -1)
    it "zero" $ do
      isPowOf2 0 `shouldBe` (False, -1)
    it "one" $ do
      isPowOf2 1 `shouldBe` (True, 0)
    it "two" $ do
      isPowOf2 2 `shouldBe` (True, 1)
    it "three" $ do
      isPowOf2 3 `shouldBe` (False, -1)
    it "four" $ do
      isPowOf2 4 `shouldBe` (True, 2)
    it "five" $ do
      isPowOf2 5 `shouldBe` (False, -1)
  describe "sL2pO2" $ do
    it "empty" $ do
      sL2pO2 [] `shouldBe` []
    it "negative" $ do
      sL2pO2 ["-42"] `shouldBe` [-1]
    it "a few others" $ do
      sL2pO2 ["0", "1", "2", "3", "4"] `shouldBe` [-1, 0, 1, -1, 2]
    it "a few from the task" $ do
      sL2pO2 ["00", "007", "008"] `shouldBe` [-1, -1, 3]
