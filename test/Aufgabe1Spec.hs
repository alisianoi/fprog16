module Aufgabe1Spec (spec) where

import Test.Hspec
-- import Test.Hspec.QuickCheck

import Aufgabe1

spec :: Spec
spec = do
  describe "factorial" $ do
    it "m = 1" $ do
      facInv 1 `shouldBe` 0
    it "m = 2" $ do
      facInv 2 `shouldBe` 2
    it "m = 3" $ do
      facInv 3 `shouldBe` -1
    it "m = 4" $ do
      facInv 4 `shouldBe` -1
    it "m = 5" $ do
      facInv 5 `shouldBe` -1
    it "m = 6" $ do
      facInv 6 `shouldBe` 3
    it "m = -1" $ do
      facInv (-1) `shouldBe` -1
  describe "extractDigits" $ do
    it "B007pL1234Q1151248cvK" $ do
      extractDigits "a5B007pL1234Q1151248cvK" `shouldBe` "500712341151248"
    it "abcDEFghi" $ do
      extractDigits "abcDEFghi" `shouldBe` ""
    it "" $ do
      extractDigits "" `shouldBe` ""
    it "aaa1" $ do
      extractDigits "aaa1" `shouldBe` "1"
    it "14wrfer" $ do
      extractDigits "14wrfer" `shouldBe` "14"
  describe "convert" $ do
    it "000reffu000" $ do
      convert "000reffu000" `shouldBe` 0
    it "000reffu010" $ do
      convert "000reffu010" `shouldBe` 10
    it "010reffu010" $ do
      convert "010reffu010" `shouldBe` 10010
    it "ji1ds1j1oo" $ do
      convert "ji1ds1j1oo" `shouldBe` 111
