module InterpreterTest where

import qualified Data.Map as Map
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Monad.IO.Class

import Utils
import SExpr
import Interpreter

testEvalAtoms :: IO ()
testEvalAtoms = hspec $ do
  describe "Test for evaluating atomic s-expressions" $ do

    it "test for Nil" $ do
      let x = Nil
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` Nil

    it "test for Integer" $ do
      let x = Integer 4
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` (Integer 4)

    it "test for Double" $ do
      let x = Double 4.0
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` (Double 4.0)

    it "test for String" $ do
      let x = String "Hello, World!"
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` (String "Hello, World!")

    it "test for #t" $ do
      let x = T
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` T

    it "test for #f" $ do
      let x = F
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` F

testEvalBuiltIn :: IO ()
testEvalBuiltIn = hspec $ do
  describe "Test for evaluating built-in functions" $ do

    it "test for cons #1" $ do
      let x = CONS (Integer 4) (Integer 5)
          y = Pair (Integer 4) (Integer 5)
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` y

    it "test for cons #2" $ do
      let x = CONS (Integer 4) (String "5")
          y = Pair (Integer 4) (String "5")
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` y

    it "test for car #1" $ do
      let x = CONS (Integer 4) (Integer 5)
          y = CAR x
      let (Effect result) = eval Map.empty [] y
      Right (env', [], x') <- result
      x' `shouldBe` (Integer 4)

    it "test for car #2" $ do
      let x = CONS (CONS (Integer 4) (Integer 7)) (Integer 5)
          y = CAR x
      let (Effect result) = eval Map.empty [] y
      Right (env', [], x') <- result
      x' `shouldBe` (Pair (Integer 4) (Integer 7))

    it "test for cdr #1" $ do
      let x = CONS (Integer 4) (Integer 5)
          y = CDR x
      let (Effect result) = eval Map.empty [] y
      Right (env', [], x') <- result
      x' `shouldBe` (Integer 5)

    it "test for cdr #2" $ do
      let x = CONS (Integer 5) (CONS (Integer 4) (Integer 7))
          y = CDR x
      let (Effect result) = eval Map.empty [] y
      Right (env', [], x') <- result
      x' `shouldBe` (Pair (Integer 4) (Integer 7))

    it "test for eq #1" $ do
      let x = EQQ (Integer 4) (Integer 5)
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` F

    it "test for eq #2" $ do
      let x = EQQ (Integer 4) (Integer 4)
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` T

    it "test for atom #1" $ do
      let x = ATOM (Integer 4)
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` T

    it "test for atom #2" $ do
      let x = ATOM (CONS (Integer 4) (Integer 5))
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` F

testEvalMacros :: IO ()
testEvalMacros = hspec $ do
  describe "Test for evaluating built-in macros" $ do

    it "test for if #1" $ do
      let x = IF T (Integer 4) (Integer 5)
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` (Integer 4)

    it "test for if #2" $ do
      let x = IF F (Integer 4) (Integer 5)
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` (Integer 5)

    it "test for if #3" $ do
      let x = IF (ATOM (Double 4.0)) (Integer 4) (Integer 5)
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` (Integer 4)

    it "test for cond #1" $ do
      let x = COND [(ATOM (Double 4.0), Integer 4), (ATOM (Double 3.0), Integer 5)]
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` (Integer 4)

    it "test for cond #2" $ do
      let x = COND [(EQQ (Double 4.0) (Integer 4), Integer 4), (ATOM (Double 3.0), Integer 5)]
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` (Integer 5)

    it "test for cond #3" $ do
      let x = COND [(F, Integer 4), (F, Double 7.0), (T, Integer 5)]
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` (Integer 5)

    it "test for quote #1" $ do
      let x = QUOTE (CONS (Integer 4) (Double 6.0))
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` (CONS (Integer 4) (Double 6.0))

testEvalMath :: IO ()
testEvalMath = hspec $ do
  describe "Test for evaluating math functions" $ do

    it "test for + #1" $ do
      let x = PLUS [Integer 1, Integer 2, Integer 3]
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` (Integer 6)

    it "test for + #2" $ do
      let x = PLUS [Integer (-1), Integer 2, Integer 3]
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` (Integer 4)

    it "test for + #3" $ do
      let x = PLUS [Integer 1, Integer 2]
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` (Integer 3)

    it "test for + #4" $ do
      let x = PLUS [Integer 1, Double 2.0]
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` (Double 3.0)

    it "test for + #5" $ do
      let x = PLUS [Double 1.0, Integer 2]
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` (Double 3.0)

    it "test for + #5" $ do
      let x = PLUS [Double 1.0, Double 2.0]
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` (Double 3.0)

    it "test for + #6" $ do
      let x = PLUS []
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` (Integer 0)

    it "test for - #1" $ do
      let x = MINUS [Integer 1]
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` (Integer (-1))

    it "test for - #2" $ do
      let x = MINUS [Integer 1, Integer 2]
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` (Integer (-1))

    it "test for - #3" $ do
      let x = MINUS []
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` (Integer 0)

    it "test for - #4" $ do
      let x = MINUS [Integer 4, Double 1.0]
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` (Double 3.0)

    it "test for - #5" $ do
      let x = MINUS [Integer 4, Double 1.0, Integer 7]
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` (Double (-4.0))

    it "test for * #1" $ do
      let x = MULT [Integer 1, Integer 2, Integer 3]
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` (Integer 6)

    it "test for * #2" $ do
      let x = MULT [Integer 1, Integer 2]
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` (Integer 2)

    it "test for / #1" $ do
      let x = DIV [Double 1.0, Double 2.0]
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` (Double 0.5)

    it "test for / #2" $ do
      let x = DIV [Integer 1, Integer 2]
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` (Integer 0)

    it "test for % #1" $ do
      let x = MOD (Integer 1) (Integer 2)
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` (Integer 1)

    it "test for < #1" $ do
      let x = LESS [Integer 1, Integer 2]
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` T

    it "test for < #2" $ do
      let x = LESS [Double 1, Double 2]
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` T

    it "test for < #1" $ do
      let x = LESS [Integer 3, Integer 2]
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` F

    it "test for > #1" $ do
      let x = GREATER [Integer 3, Integer 2]
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` T

    it "test for > #2" $ do
      let x = GREATER [Double 3.0, Double 2.0]
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` T

    it "test for > #3" $ do
      let x = GREATER [Integer 1, Integer 2]
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` F

testEvalLogic :: IO ()
testEvalLogic = hspec $ do
  describe "Test for evaluating logic functions" $ do

    it "test for and #1" $ do
      let x = AND [T, F]
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` F

    it "test for and #2" $ do
      let x = AND [T, T]
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` T

    it "test for and #3" $ do
      let x = AND [F, T]
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` F

    it "test for and #4" $ do
      let x = AND [F, F]
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` F

    it "test for or #1" $ do
      let x = OR [T, F]
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` T

    it "test for or #2" $ do
      let x = OR [T, T]
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` T

    it "test for or #3" $ do
      let x = OR [F, T]
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` T

    it "test for or #4" $ do
      let x = OR [F, F]
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` F
