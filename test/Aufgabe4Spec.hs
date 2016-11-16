module Aufgabe4Spec (spec) where

import Test.Hspec

import Aufgabe4

tree1 :: Tree Integer
tree1 = Node 42 Nil Nil

tree2 :: Tree Integer
tree2 = Node 42 tree1 tree1

tree3 :: Tree Integer
tree3 = Node 42 (Node 44 Nil Nil) Nil

tree4 :: Tree Integer
tree4 = Node 42 Nil (Node 40 Nil Nil)

leaf21 :: Tree Integer
leaf21 = Node 21 Nil Nil

leaf84 :: Tree Integer
leaf84 = Node 84 Nil Nil

ntree1 :: Tree Integer
ntree1 = Node 42 leaf84 leaf21
ntree2 :: Tree Integer
ntree2 = Node 42 (Node 40 leaf84 Nil) Nil
ntree3 :: Tree Integer
ntree3 = Node 42 (Node 40 Nil leaf84) Nil
ntree4 :: Tree Integer
ntree4 = Node 42 (Node 40 leaf84 leaf21) Nil
ntree5 :: Tree Integer
ntree5 = Node 42 (Node 40 leaf21 leaf21) Nil
ntree6 :: Tree Integer
ntree6 = Node 42 Nil (Node 44 leaf84 leaf21)
ntree7 :: Tree Integer
ntree7 = Node 42 Nil (Node 44 Nil leaf21)
ntree8 :: Tree Integer
ntree8 = Node 42 leaf21 (Node 44 Nil leaf21)

gtree1 :: Tree Integer
gtree1 = Node 42 leaf21 leaf84
gtree2 :: Tree Integer
gtree2 = Node 42 (Node 40 leaf21 Nil) Nil
gtree3 :: Tree Integer
gtree3 = Node 42 (Node 40 Nil Nil) Nil
gtree4 :: Tree Integer
gtree4 = Node 42 (Node 40 leaf21 Nil) leaf84
gtree5 :: Tree Integer
gtree5 = nil
gtree6 :: Tree Integer
gtree6 = Node 42 (Node 32 (Node 16 Nil Nil) (Node 37 Nil Nil)) (Node 100 (Node 60 Nil Nil) (Node 128 Nil Nil))
gtree7 :: Tree Integer
gtree7 = Node 42 Nil (Node 44 Nil leaf84)
gtree8 :: Tree Integer
gtree8 = Node 42 leaf21 (Node 44 Nil Nil)

spec :: Spec
spec = do
  describe "isNilTree" $ do
    it "nil is nil" $ do
      isNilTree nil `shouldBe` True
    it "not is not Nill" $ do
      isNodeTree nil `shouldBe` False
  describe "left/rightSubTree" $ do
    it "tree1" $ do
      leftSubTree tree1 `shouldBe` Nil
      rightSubTree tree1 `shouldBe` Nil
    it "tree2" $ do
      leftSubTree tree2 `shouldBe` tree1
      rightSubTree tree2 `shouldBe` tree1
  describe "treeValue/isValueOf" $ do
    it "tree1" $ do
      treeValue tree1 `shouldBe` 42
      isValueOf 42 tree1 `shouldBe` True
      isValueOf 41 tree1 `shouldBe` False
      isValueOf 43 tree1 `shouldBe` False
    it "tree2" $ do
      treeValue tree2 `shouldBe` 42
      isValueOf 42 tree2 `shouldBe` True
      isValueOf 41 tree2 `shouldBe` False
      isValueOf 43 tree2 `shouldBe` False
    it "tree3" $ do
      isValueOf 42 tree3 `shouldBe` True
      isValueOf 44 tree3 `shouldBe` True
      isValueOf 40 tree3 `shouldBe` False
      isValueOf 41 tree3 `shouldBe` False
      isValueOf 43 tree3 `shouldBe` False
      isValueOf 45 tree3 `shouldBe` False
    it "tree4" $ do
      isValueOf 40 tree4 `shouldBe` True
      isValueOf 42 tree4 `shouldBe` True
      isValueOf 44 tree4 `shouldBe` False
      isValueOf 35 tree4 `shouldBe` False
  describe "isOrderedTree" $ do
    it "not ordered" $ do
      isOrderedTree ntree1 `shouldBe` False
      isOrderedTree ntree2 `shouldBe` False
      isOrderedTree ntree3 `shouldBe` False
      isOrderedTree ntree4 `shouldBe` False
      isOrderedTree ntree5 `shouldBe` False
      isOrderedTree ntree6 `shouldBe` False
      isOrderedTree ntree7 `shouldBe` False
      isOrderedTree ntree8 `shouldBe` False
    it "ordered" $ do
      isOrderedTree gtree1 `shouldBe` True
      isOrderedTree gtree2 `shouldBe` True
      isOrderedTree gtree3 `shouldBe` True
      isOrderedTree gtree4 `shouldBe` True
      isOrderedTree gtree5 `shouldBe` True
      isOrderedTree gtree6 `shouldBe` True
      isOrderedTree gtree7 `shouldBe` True
      isOrderedTree gtree8 `shouldBe` True
  describe "insert" $ do
    it "bigtree0" $ do
      isOrderedTree bigtree0 `shouldBe` True

      isValueOf 41 bigtree0 `shouldBe` False
      isValueOf 41 (insert 41 bigtree0) `shouldBe` True
      isOrderedTree (insert 41 bigtree0) `shouldBe` True
      isValueOf 16 (insert 41 bigtree0) `shouldBe` True
      isValueOf 32 (insert 41 bigtree0) `shouldBe` True
      isValueOf 64 (insert 41 bigtree0) `shouldBe` True
      isValueOf 128 (insert 41 bigtree0) `shouldBe` True
      isValueOf 256 (insert 41 bigtree0) `shouldBe` True
      isValueOf 512 (insert 41 bigtree0) `shouldBe` True
      isValueOf 1024 (insert 41 bigtree0) `shouldBe` True

      isValueOf 43 bigtree0 `shouldBe` False
      isValueOf 43 (insert 43 bigtree0) `shouldBe` True
      isOrderedTree (insert 43 bigtree0) `shouldBe` True
      isValueOf 16 (insert 43 bigtree0) `shouldBe` True
      isValueOf 32 (insert 43 bigtree0) `shouldBe` True
      isValueOf 64 (insert 43 bigtree0) `shouldBe` True
      isValueOf 128 (insert 43 bigtree0) `shouldBe` True
      isValueOf 256 (insert 43 bigtree0) `shouldBe` True
      isValueOf 512 (insert 43 bigtree0) `shouldBe` True
      isValueOf 1024 (insert 43 bigtree0) `shouldBe` True

      isValueOf 14 bigtree0 `shouldBe` False
      isValueOf 14 (insert 14 bigtree0) `shouldBe` True
      isOrderedTree (insert 14 bigtree0) `shouldBe` True
      isValueOf 16 (insert 14 bigtree0) `shouldBe` True
      isValueOf 32 (insert 14 bigtree0) `shouldBe` True
      isValueOf 64 (insert 14 bigtree0) `shouldBe` True
      isValueOf 128 (insert 14 bigtree0) `shouldBe` True
      isValueOf 256 (insert 14 bigtree0) `shouldBe` True
      isValueOf 512 (insert 14 bigtree0) `shouldBe` True
      isValueOf 1024 (insert 14 bigtree0) `shouldBe` True

      isValueOf 18 bigtree0 `shouldBe` False
      isValueOf 18 (insert 18 bigtree0) `shouldBe` True
      isOrderedTree (insert 18 bigtree0) `shouldBe` True
      isValueOf 16 (insert 18 bigtree0) `shouldBe` True
      isValueOf 32 (insert 18 bigtree0) `shouldBe` True
      isValueOf 64 (insert 18 bigtree0) `shouldBe` True
      isValueOf 128 (insert 18 bigtree0) `shouldBe` True
      isValueOf 256 (insert 18 bigtree0) `shouldBe` True
      isValueOf 512 (insert 18 bigtree0) `shouldBe` True
      isValueOf 1024 (insert 18 bigtree0) `shouldBe` True

      isValueOf 30 bigtree0 `shouldBe` False
      isValueOf 30 (insert 30 bigtree0) `shouldBe` True
      isOrderedTree (insert 30 bigtree0) `shouldBe` True
      isValueOf 16 (insert 30 bigtree0) `shouldBe` True
      isValueOf 32 (insert 30 bigtree0) `shouldBe` True
      isValueOf 64 (insert 30 bigtree0) `shouldBe` True
      isValueOf 128 (insert 30 bigtree0) `shouldBe` True
      isValueOf 256 (insert 30 bigtree0) `shouldBe` True
      isValueOf 512 (insert 30 bigtree0) `shouldBe` True
      isValueOf 1024 (insert 30 bigtree0) `shouldBe` True

      isValueOf 60 bigtree0 `shouldBe` False
      isValueOf 60 (insert 60 bigtree0) `shouldBe` True
      isOrderedTree (insert 60 bigtree0) `shouldBe` True
      isValueOf 16 (insert 60 bigtree0) `shouldBe` True
      isValueOf 32 (insert 60 bigtree0) `shouldBe` True
      isValueOf 64 (insert 60 bigtree0) `shouldBe` True
      isValueOf 128 (insert 60 bigtree0) `shouldBe` True
      isValueOf 256 (insert 60 bigtree0) `shouldBe` True
      isValueOf 512 (insert 60 bigtree0) `shouldBe` True
      isValueOf 1024 (insert 60 bigtree0) `shouldBe` True

  describe "delete" $ do
    it "bigtree0" $ do
      isOrderedTree (delete 128 bigtree0) `shouldBe` True
      isValueOf 16 (delete 128 bigtree0) `shouldBe` True
      isValueOf 32 (delete 128 bigtree0) `shouldBe` True
      isValueOf 64 (delete 128 bigtree0) `shouldBe` True
      isValueOf 128 (delete 128 bigtree0) `shouldBe` False
      isValueOf 256 (delete 128 bigtree0) `shouldBe` True
      isValueOf 512 (delete 128 bigtree0) `shouldBe` True
      isValueOf 1024 (delete 128 bigtree0) `shouldBe` True
    it "bigtree1" $ do
      isOrderedTree (delete 32 bigtree1) `shouldBe` True
      isValueOf 16 (delete 32 bigtree1) `shouldBe` True
      isValueOf 8 (delete 32 bigtree1) `shouldBe` True
      isValueOf 18 (delete 32 bigtree1) `shouldBe` True
      isValueOf 20 (delete 32 bigtree1) `shouldBe` True
      isValueOf 32 (delete 32 bigtree1) `shouldBe` False
      isValueOf 64 (delete 32 bigtree1) `shouldBe` True
      isValueOf 128 (delete 32 bigtree1) `shouldBe` True
      isValueOf 256 (delete 32 bigtree1) `shouldBe` True
      isValueOf 512 (delete 32 bigtree1) `shouldBe` True
      isValueOf 1024 (delete 32 bigtree1) `shouldBe` True


bigtree0 :: Tree Integer
bigtree0 = (
  Node 128 (
      Node 32 (Node 16 Nil Nil) (Node 64 Nil Nil)
      ) (
      Node 512 (Node 256 Nil Nil) (Node 1024 Nil Nil)
      )
  )

bigtree1 :: Tree Integer
bigtree1 = (
  Node 128 (
      Node 32 (
          Node 16 (Node 8 Nil Nil) (Node 20 (Node 18 Nil Nil) Nil)
          ) (Node 64 Nil Nil)
      ) (
      Node 512 (Node 256 Nil Nil) (Node 1024 Nil Nil)
      )
  )
