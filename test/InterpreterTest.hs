module InterpreterTest where

import qualified Data.Map as Map
import Test.Hspec
import Control.Monad.IO.Class

import Utils
import Parser
import Interpreter

testEvalSExpr :: IO ()
testEvalSExpr = hspec $ do
  describe "test for evaluating S-Expressions" $ do

    it "test for Nil" $ do
      let x = Nil
      let env = [Map.empty]
      let (Effect result) = eval env x
      Right (env', x') <- result
      x' `shouldBe` Nil
