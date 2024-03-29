module CompilerTest where

import Test.Hspec
import Control.Monad.IO.Class

import Data.Token
import Data.ParseTree
import Data.SExpr
import Data.Compiler

testReadTokens :: IO ()
testReadTokens = hspec $ do
  describe "test for reading tokens" $ do

    it "test for blanks" $ do
      let x = reads " \n\r\t" :: [(Token, String)]
      x `shouldBe` []

    it "test for integers #1" $ do
      let x = reads "45asd" :: [(Token, String)]
      x `shouldBe` [(IntegerType 45, "asd")]

    it "test for doubles #1" $ do
      let x = reads "45.0asd" :: [(Token, String)]
      x `shouldBe` [(DoubleType 45.0, "asd")]

    it "test for strings #1" $ do
      let x = reads "\"a 45 asav\"asd" :: [(Token, String)]
      x `shouldBe` [(StringType "a 45 asav", "asd")]

    it "test for strings #2" $ do
      let x = reads "\"a 45 asavasd" :: [(Token, String)]
      x `shouldBe` []

    it "test for symbols #1" $ do
      let x = reads "ciao asd" :: [(Token, String)]
      x `shouldBe` [(SymbType "ciao", " asd")]

    it "test for error #1" $ do
      let x = reads "( asd" :: [(Token, String)]
      x `shouldBe` []

    it "test for error #2" $ do
      let x = reads ") asd" :: [(Token, String)]
      x `shouldBe` []

    it "test for error #3" $ do
      let x = reads ". asd" :: [(Token, String)]
      x `shouldBe` []

    it "test for quote #1" $ do
      let x = reads "'asd" :: [(Token, String)]
      x `shouldBe` [(SymbType "'asd", "")]

testReadParseTrees :: IO ()
testReadParseTrees = hspec $ do
  describe "test for reading ParseTrees" $ do

    it "test for blanks" $ do
      let x = reads " \r\t\n" :: [(ParseTree, String)]
      x `shouldBe` []

    it "test for Empty #1" $ do
      let x = reads " ()asd" :: [(ParseTree, String)]
      x `shouldBe` [(Empty, "asd")]

    it "test for Leaf #1" $ do
      let x = reads " 45asd" :: [(ParseTree, String)]
      x `shouldBe` [(Leaf (IntegerType 45), "asd")]

    it "test for Node #1" $ do
      let x = reads "( 45 . 56.0) asd" :: [(ParseTree, String)]
      x `shouldBe` [(Node (Leaf (IntegerType 45)) (Leaf (DoubleType 56.0)), " asd")]

    it "test for quote #1" $ do
      let x = reads "'() rest" :: [(ParseTree, String)]
      x `shouldBe` [(Node (Leaf (SymbType "\'")) (Node Empty Empty), " rest")]

    it "test for quote #2" $ do
      let x = reads "'(cons a b) rest" :: [(ParseTree, String)]
      x `shouldBe` [(Node (Leaf (SymbType "\'")) (Node (Node (Leaf (SymbType "cons")) (Node (Leaf (SymbType "a")) (Node (Leaf (SymbType "b")) Empty))) Empty), " rest")]

    it "test for quote #3" $ do
      let x = reads "'ciao rest" :: [(ParseTree, String)]
      x `shouldBe` [(Node (Leaf (SymbType "\'")) (Node (Leaf (SymbType "ciao")) Empty), " rest")]

testCompile :: IO ()
testCompile = hspec $ do
  describe "test for internal compiling" $ do

    it "test for quote #1" $ do
      let x = Leaf (SymbType "\'ciao")
      case compile x of
        Just x' -> do
          x' `shouldBe` (QUOTE (Symb "ciao"))
        _ -> 1 `shouldBe` 2

    it "test for label #1" $ do
      let a = Leaf $ IntegerType 4
      let x = (Node (Leaf (SymbType "label")) (Node a (Node a Empty)))
      case compile x of
        Just _ -> do
          1 `shouldBe` 2
        _ -> 1 `shouldBe` 1

    it "test for let #1" $ do
      let a = Leaf $ IntegerType 4
      let x = (Node (Leaf (SymbType "let")) (Node a (Node a (Node a Empty))))
      case compile x of
        Just _ -> do
          1 `shouldBe` 2
        _ -> 1 `shouldBe` 1

    it "test for define #1" $ do
      let a = Leaf $ IntegerType 4
      let x = (Node (Leaf (SymbType "define")) (Node a (Node a Empty)))
      case compile x of
        Just _ -> do
          1 `shouldBe` 2
        _ -> 1 `shouldBe` 1

    it "test for lambda #1" $ do
      let a = Leaf $ IntegerType 4
      let x = (Node (Leaf (SymbType "lambda")) (Node a (Node a Empty)))
      case compile x of
        Just _ -> do
          1 `shouldBe` 2
        _ -> 1 `shouldBe` 1

{--

testEvalSExpr :: IO ()
testEvalSExpr = hspec $ do
  describe "test for evaluating S-expressions" $ do

    it "test for lambda #1" $ do
      let [(lambda, _)] = reads "((lambda (x y) (cons x (cons y ()))) #t #f)" :: [(SExpr, String)]
          [(val, _)] = reads "(#t #f)" :: [(SExpr, String)]
      let Effect lambda' = eval ([], lambda)
      Right (env, x) <- lambda'
      (env, x) `shouldBe` ([], val)

    it "test for define #1" $ do
      let [(def, _)] = reads "(define x #t)" :: [(SExpr, String)]
          [(val, _)] = reads "x" :: [(SExpr, String)]
      let Effect def' = eval ([], def)
      Right (env, x) <- def'
      let Effect val' = eval (env, val)
      Right (env', x') <- val'
      (env, x) `shouldBe` ([("x", Val $ Bool True)], Sym "x")
      (env', x')  `shouldBe` ([("x", Val $ Bool True)], Val $ Bool True)

    it "test for define #2" $ do
      let [(def1, _)] = reads "(define x #t)" :: [(SExpr, String)]
          [(lam, _)] = reads "(define foo (lambda (a) (cons a x)))" :: [(SExpr, String)]
          [(ans1, _)] = reads "(foo #t)" :: [(SExpr, String)]
          [(def2, _)] = reads "(define x #f)" :: [(SExpr, String)]
          [(ans2, _)] = reads "(foo #t)" :: [(SExpr, String)]
          [(ans3, _)] = reads "x" :: [(SExpr, String)]
      let Effect def1' = eval ([], def1)
      Right (env, x) <- def1'
      let Effect lam' = eval (env, lam)
      Right (env', l) <- lam'
      let Effect ans1' = eval (env', ans1)
      Right (env'', a1) <- ans1'
      let Effect def2' = eval (env'', def2)
      Right (env''', x') <- def2'
      let Effect ans2' = eval (env''', ans2)
      Right (env'''', a2) <- ans2'
      let Effect ans3' = eval (env'''', ans3)
      Right (env''''', a3) <- ans3'
      a3 `shouldBe` (Val $ Bool False)
      a1 `shouldBe` (Pair (Val $ Bool True) (Val $ Bool True))
      a2 `shouldBe` (Pair (Val $ Bool True) (Val $ Bool False))

    it "test for define #3" $ do
      let [(def1, _)] = reads "(define x #t)" :: [(SExpr, String)]
          [(ans1, _)] = reads "x" :: [(SExpr, String)]
          [(ans2, _)] = reads "x" :: [(SExpr, String)]
      let Effect def1' = eval ([], def1)
      Right (env, x) <- def1'
      let Effect ans1' = eval (env, ans1)
      Right (env', a1) <- ans1'
      let Effect ans2' = eval (env', ans2)
      Right (env'', a2) <- ans2'
      a1 `shouldBe` (Val $ Bool True)
      a2 `shouldBe` (Val $ Bool True)

    it "test for define #4" $ do
      let [(def1, _)] = reads "(define x #t)" :: [(SExpr, String)]
          [(ans1, _)] = reads "x" :: [(SExpr, String)]
          [(def2, _)] = reads "(define x #f)" :: [(SExpr, String)]
          [(ans2, _)] = reads "x" :: [(SExpr, String)]
      let Effect def1' = eval ([], def1)
      Right (env, x) <- def1'
      let Effect ans1' = eval (env, ans1)
      Right (env', a1) <- ans1'
      let Effect def2' = eval (env', def2)
      Right (env'', y) <- def2'
      let Effect ans2' = eval (env'', ans2)
      Right (env''', a2) <- ans2'
      a1 `shouldBe` (Val $ Bool True)
      a2 `shouldBe` (Val $ Bool False)

    it "test for define #5" $ do
      let [(def1, _)] = reads "(define x #t)" :: [(SExpr, String)]
          [(ans1, _)] = reads "x" :: [(SExpr, String)]
          [(def2, _)] = reads "(define y #f)" :: [(SExpr, String)]
          [(ans2, _)] = reads "x" :: [(SExpr, String)]
      let Effect def1' = eval ([], def1)
      Right (env, x) <- def1'
      let Effect ans1' = eval (env, ans1)
      Right (env', a1) <- ans1'
      let Effect def2' = eval (env', def2)
      Right (env'', y) <- def2'
      let Effect ans2' = eval (env'', ans2)
      Right (env''', a2) <- ans2'
      a1 `shouldBe` (Val $ Bool True)
      a2 `shouldBe` (Val $ Bool True)

    it "test for dynamic scoping #1" $ do
      let [(def, _)] = reads "(define x #t)" :: [(SExpr, String)]
          [(lam1, _)] = reads "(define g (lambda (y) (cons x y)))" :: [(SExpr, String)]
          [(lam2, _)] = reads "(define f (lambda (x) (g #t)))" :: [(SExpr, String)]
          [(ans, _)] = reads "(f #f)" :: [(SExpr, String)]
      let Effect def' = eval ([], def)
      Right (env, x) <- def'
      let Effect lam1' = eval (env, lam1)
      Right (env', l1) <- lam1'
      let Effect lam2' = eval (env', lam2)
      Right (env'', l2) <- lam2'
      let Effect ans' = eval (env'', ans)
      Right (env''', a) <- ans'
      a `shouldBe` (Pair (Val $ Bool False) (Val $ Bool True))

    it "test for dynamic scoping #2" $ do
      let [(run, _)] = reads "((lambda (a) ((lambda (f) ((lambda (a) (f)) #f)) (lambda () a))) #t)" :: [(SExpr, String)]
          [(res, _)] = reads "#f" :: [(SExpr, String)]
      let Effect run' = eval ([], run)
      Right (_, x) <- run'
      x `shouldBe` res

    it "test for recursion #1" $ do
      let [(def, _)] = reads "(define fib (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))" :: [(SExpr, String)]
          [(run, _)] = reads "(fib 28)" :: [(SExpr, String)]
      let Effect def' = eval ([], def)
      Right (env, fib) <- def'
      let Effect run' = eval (env, run)
      Right (env', res) <- run'
      res `shouldBe` (Val $ Int 317811)
--}

      {--
    it "test for define #2" $ do
      let [(def1, _)] = reads "(define x #t)" :: [(SExpr, String)]
          [(def2, _)] = reads "(define y #f)" :: [(SExpr, String)]
          [(def3, _)] = reads "(define z ())" :: [(SExpr, String)]
          [(expr, _)] = reads "(cons x (cons y z))" :: [(SExpr, String)]
          [(Value, _)] = reads "(#t #f)" :: [(SExpr, String)]
      (env, def1') <- eval ([], def1)
      --env `shouldBe` [("x", Val $ Bool True)]
      (env', def2') <- eval (env, def2)
      --env' `shouldBe` [("y", Val $ Bool False), ("x", Val $ Bool True)]
      (env'', def3') <- eval (env', def3)
      --env'' `shouldBe` [("z", Nil), ("y", Val $ Bool False), ("x", Val $ Bool True)]
      (env''', expr') <- eval (env'', expr)
      return $ expr' `shouldBe` val

    it "test for lambda #1" $ do
      let [(expr, _)] = reads "((lambda (x y) (cons x y)) #t #f)" :: [(SExpr, String)]
          [(res, _)] = reads "(#t . #f)" :: [(SExpr, String)]
      (env, expr') <- eval ([], expr)
      return $ (env, expr') `shouldBe` ([], res)

    it "test for static scoping #1" $ do
      let [(run, _)] = reads "((lambda (a) ((lambda (f) ((lambda (a) (f)) #f)) (lambda () a))) #t)" :: [(SExpr, String)]
          [(res, _)] = reads "#t" :: [(SExpr, String)]
      (_, x) <- eval ([], run)
      return $ x `shouldBe` res

    it "test for define #3" $ do
      let [(def, _)] = reads "(define foo (lambda (x) (if (eq? x ()) () (foo (cdr x)))))" :: [(SExpr, String)]
          [(run, _)] = reads "(foo (cons #t #t))" :: [(SExpr, String)]
          [(res, _)] = reads "()" :: [(SExpr, String)]
      (env', _) <- eval ([], def)
      (env'', y) <- eval (env', run)
      return $ y `shouldBe` res
--}
{--
testeval'SExpr :: IO ()
testeval'SExpr = hspec $ do
  describe "test for eval'uating S-expressions" $ do

    it "test for #nil #1" $ liftIO $ do
      (x, _) <- eval' (Atom SymNil) []
      x `shouldBe` (Atom SymNil)

    it "test for define #1" $ do
      let [(def, _)] = reads "(define foo (lambda (x y) (cons x (cons x y))))" :: [(SExpr, String)]
          [(run, _)] = reads "(foo 4 5)" :: [(SExpr, String)]
          [(res, _)] = reads "(4 . (4 . 5))" :: [(SExpr, String)]
      (_, env') <- eval' def []
      (y, env'') <- eval' run env'
      y `shouldBe` res

    it "test for define #2" $ do
      let [(def, _)] = reads "(define fact (lambda (n) (if (eq? n 0) 1 (* n (fact (- n 1))))))" :: [(SExpr, String)]
          [(run, _)] = reads "(fact 5)" :: [(SExpr, String)]
          [(res, _)] = reads "120" :: [(SExpr, String)]
      (_, env') <- eval' def []
      (y, env'') <- eval' run env'
      y `shouldBe` res

    it "test for define #3" $ do
      let [(def, _)] = reads "(define fib (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))" :: [(SExpr, String)]
          [(run, _)] = reads "(fib 28)" :: [(SExpr, String)]
          [(res, _)] = reads "317811" :: [(SExpr, String)]
      (_, env') <- eval' def []
      (y, env'') <- eval' run env'
      y `shouldBe` res

    it "test for function cons #1" $ do
      let [(run, _)] = reads "(cons #t (cons #f ()))" :: [(SExpr, String)]
          [(res, _)] = reads "(#t #f)" :: [(SExpr, String)]
      (x, _) <- eval' run []
      x `shouldBe` res

    it "test for function car #1" $ do
      let [(run, _)] = reads "(car (cons #t (cons #f ())))" :: [(SExpr, String)]
          [(res, _)] = reads "#t #f" :: [(SExpr, String)]
      (x, _) <- eval' run []
      x `shouldBe` res

    it "test for function cdr #1" $ do
      let [(run, _)] = reads "(cdr (cons #t (cons #f ())))" :: [(SExpr, String)]
          [(res, _)] = reads "(#f)" :: [(SExpr, String)]
      (x, _) <- eval' run []
      x `shouldBe` res

    it "test for function pair? #1" $ do
      let [(run, _)] = reads "(pair? (cons #t (cons #f ())))" :: [(SExpr, String)]
          [(res, _)] = reads "#t" :: [(SExpr, String)]
      (x, _) <- eval' run []
      x `shouldBe` res

    it "test for function eq? #1" $ do
      let [(run, _)] = reads "(eq? (car (cons #t (cons #f ()))) #t)" :: [(SExpr, String)]
          [(res, _)] = reads "#t" :: [(SExpr, String)]
      (x, _) <- eval' run []
      x `shouldBe` res

    it "test for function eq? #2" $ do
      let [(run, _)] = reads "(eq? (car (cons #t (cons #f ()))) #f)" :: [(SExpr, String)]
          [(res, _)] = reads "#f" :: [(SExpr, String)]
      (x, _) <- eval' run []
      x `shouldBe` res

    it "test for function + #1" $ do
      let [(run, _)] = reads "(+ 1)" :: [(SExpr, String)]
          [(res, _)] = reads "1" :: [(SExpr, String)]
      (x, _) <- eval' run []
      x `shouldBe` res

    it "test for function + #2" $ do
      let [(run, _)] = reads "(+ 1 2 3)" :: [(SExpr, String)]
          [(res, _)] = reads "6" :: [(SExpr, String)]
      (x, _) <- eval' run []
      x `shouldBe` res

    it "test for function * #1" $ do
      let [(run, _)] = reads "(* 1)" :: [(SExpr, String)]
          [(res, _)] = reads "1" :: [(SExpr, String)]
      (x, _) <- eval' run []
      x `shouldBe` res

    it "test for function * #2" $ do
      let [(run, _)] = reads "(* 1 2 3)" :: [(SExpr, String)]
          [(res, _)] = reads "6" :: [(SExpr, String)]
      (x, _) <- eval' run []
      x `shouldBe` res

    it "test for static scoping #1" $ do
      let [(run, _)] = reads "((lambda (a) ((lambda (f) ((lambda (a) (f)) #f)) (lambda () a))) #t)" :: [(SExpr, String)]
          [(res, _)] = reads "#t" :: [(SExpr, String)]
      (x, _) <- eval' run []
      x `shouldBe` res
--}
