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

    it "test for list #1" $ do
      let x = LIST [Integer 4, Double 6.0]
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` (Pair (Integer 4) (Pair (Double 6.0) Nil))

    it "test for list #2" $ do
      let x = LIST []
      let (Effect result) = eval Map.empty [] x
      Right (env', [], x') <- result
      x' `shouldBe` Nil

    it "test for let #1" $ do
      let x = LET (Symb "x") (Integer 4) (Symb "x")
      let (Effect result) = eval Map.empty [] x
      Right (env', ctx', x') <- result
      x' `shouldBe` (Integer 4)
      ctx' `shouldBe` [Map.insert "x" (Integer 4) Map.empty]
      env' `shouldBe` Map.empty

    it "test for let #2" $ do
      let x = LET (Integer 5) (Integer 4) (Integer 5)
      let (Effect def) = eval Map.empty [] x
      Left err <- def
      return ()

    it "test for define #1" $ do
      let x = DEFINE (Symb "x") (Integer 4)
      let y = Symb "x"
      let (Effect def) = eval Map.empty [] x
      Right (env, [], x') <- def
      let (Effect res) = eval env [] y
      Right (env', [], y') <- res
      x' `shouldBe` (Symb "x")
      y' `shouldBe` (Integer 4)
      env' `shouldBe` (Map.insert "x" (Integer 4) Map.empty)

    it "test for define #2" $ do
      let x = DEFINE (Integer 5) (Integer 4)
      let (Effect def) = eval Map.empty [] x
      Left err <- def
      return ()

    -- (lambda (x) (+ x 1))
    it "test for lambda #1" $ do
      let foo = Lambda [Symb "x"] (PLUS [Symb "x", Integer 1])
      let (Effect res) = eval Map.empty [] (Pair foo (Pair (Integer 0) Nil))
      Right (env, ctx, x') <- res
      x' `shouldBe` (Integer 1)
      env `shouldBe` Map.empty
      ctx `shouldBe` [Map.insert "x" (Integer 0) Map.empty]

    -- (lambda (n) (if (eq n 0) 1 (* n (lambda (- n 1)))))
    it "test for lambda #2" $ do
      let lam = (Lambda [Symb "n"] (IF (EQQ (Symb "n") (Integer 0)) (Integer 1) (MULT [Symb "n", Pair (Symb "fact") (Pair (MINUS [Symb "n", Integer 1]) Nil)])))
      let def = DEFINE (Symb "fact") lam
      let (Effect def') = eval Map.empty [] def
      Right (env, [], x') <- def'
      let (Effect res) = eval env [] (Pair (Symb "fact") (Pair (Integer 5) Nil))
      Right (env', ctx', x') <- res
      x' `shouldBe` (Integer 120)
      env `shouldBe` (Map.insert "fact" lam Map.empty)
      ctx' `shouldBe` [Map.insert "n" (Integer 5) Map.empty]

    -- (lambda (n) (lambda (m) (lambda () (+ n m))))
    it "test for lambda #3" $ do
      let lam = Lambda [Symb "n"] (Lambda [Symb "m"] (Lambda [] (PLUS [Symb "n", Symb "m"])))
      let (Effect res) = eval Map.empty [] (Pair lam (Pair (Integer 4) Nil))
      Right (env, ctx, x) <- res
      env `shouldBe` Map.empty
      ctx `shouldBe` [Map.insert "n" (Integer 4) Map.empty]
      let (Effect res') = eval env ctx (Pair x (Pair (Integer 5) Nil))
      Right (env', ctx', x') <- res'
      env' `shouldBe` Map.empty
      ctx' `shouldBe` ([Map.insert "m" (Integer 5) Map.empty] ++ ctx)
      let (Effect res'') = eval env' ctx' (Pair x' Nil)
      Right (env'', ctx'', x'') <- res''
      env'' `shouldBe` Map.empty
      ctx' `shouldBe` ctx'
      x'' `shouldBe` (Integer 9)

    -- ((lambda (a) ((lambda (f) ((lambda (a) (f)) #f)) (lambda () a))) #t)
    it "test for lambda #4" $ do
      let lam1 = Lambda [] (Symb "a")
      let lam2 = Lambda [Symb "a"] (Pair (Symb "f") Nil)
      let lam3 = Lambda [Symb "f"] (Pair lam2 (Pair F Nil))
      let lam4 = Lambda [Symb "a"] (Pair lam3 (Pair lam1 Nil))
      let (Effect res) = eval Map.empty [] (Pair lam4 (Pair T Nil))
      Right (env, ctx, x) <- res
      x `shouldBe` T

    it "test for label #1" $ do
      let foo = LABEL (Symb "foo") (Lambda [Symb "x"] (PLUS [Symb "x", Integer 1]))
      let (Effect res) = eval Map.empty [] foo
      Right (env, ctx, x') <- res
      ctx `shouldBe` [Map.insert "foo" (Lambda [Symb "x"] (PLUS [Symb "x", Integer 1])) Map.empty]
      env `shouldBe` Map.empty
      let (Effect res') = eval Map.empty [] (Pair foo (Pair (Integer 0) Nil))
      Right (env', ctx', x'') <- res'
      x'' `shouldBe` (Integer 1)
      ctx' `shouldBe` ([Map.insert "x" (Integer 0) Map.empty] ++ ctx)
      env' `shouldBe` Map.empty

    it "test for label #2" $ do
      let x = LABEL (Integer 5) (Integer 4)
      let (Effect def) = eval Map.empty [] x
      Left err <- def
      return ()

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
