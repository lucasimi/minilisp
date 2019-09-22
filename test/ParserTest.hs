module ParserTest where

import Test.Hspec
import Control.Monad.IO.Class

import Parser

testReadTokens :: IO ()
testReadTokens = hspec $ do
  describe "test for reading values" $ do

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

testReadTokenTrees :: IO ()
testReadTokenTrees = hspec $ do
  describe "test for reading ASTs" $ do

    it "test for blanks" $ do
      let x = reads " \r\t\n" :: [(TokenTree, String)]
      x `shouldBe` []

    it "test for Empty #1" $ do
      let x = reads " ()asd" :: [(TokenTree, String)]
      x `shouldBe` [(Empty, "asd")]

    it "test for Leaf #1" $ do
      let x = reads " 45asd" :: [(TokenTree, String)]
      x `shouldBe` [(Leaf (IntegerType 45), "asd")]

    it "test for Node #1" $ do
      let x = reads "( 45 . 56.0) asd" :: [(TokenTree, String)]
      x `shouldBe` [(Node (Leaf (IntegerType 45)) (Leaf (DoubleType 56.0)), " asd")]
{--
testReadVal :: IO ()
testReadVal = hspec $ do
  describe "test for reading values" $ do

    it "test for error #1" $ do
      let x = reads "(rest" :: [(Value, String)]
      x `shouldBe` []

    it "test for error #2" $ do
      let x = reads ".rest" :: [(Value, String)]
      x `shouldBe` []

    it "test for error #3" $ do
      let x = reads ")rest" :: [(Value, String)]
      x `shouldBe` []

    it "test for error #2" $ do
      let x = reads "rest" :: [(Value, String)]
      x `shouldBe` []

    it "test for #t #1" $ do
      let x = reads "   #t" :: [(Value, String)]
      x `shouldBe` [(Bool True, "")]

    it "test for #t #2" $ do
      let x = reads "#t(rest" :: [(Value, String)]
      x `shouldBe` [(Bool True, "(rest")]

    it "test for #t #3" $ do
      let x = reads "#t.rest" :: [(Value, String)]
      x `shouldBe` [(Bool True, ".rest")]

    it "test for #t #4" $ do
      let x = reads "#t rest" :: [(Value, String)]
      x `shouldBe` [(Bool True, " rest")]

    it "test for spaces #1" $ do
      let x = reads "   #t  rest" :: [(Value, String)]
      x `shouldBe` [(Bool True, "  rest")]

    it "test for #f" $ do
      let x = reads "#f" :: [(Value, String)]
      x `shouldBe` [(Bool False, "")]

    it "test for integer #1" $ do
      let x = reads "45rest" :: [(Value, String)]
      x `shouldBe` [(Int 45, "rest")]

    it "test for integer #2" $ do
      let x = reads "+45 rest" :: [(Value, String)]
      x `shouldBe` []

    it "test for integer #3" $ do
      let x = reads "-45rest" :: [(Value, String)]
      x `shouldBe` [(Int (-45), "rest")]

    it "test for double #1" $ do
      let x = reads "45.5786578rest" :: [(Value, String)]
      x `shouldBe` [(Num 45.5786578, "rest")]

    it "test for double #2" $ do
      let x = reads "-45.5786578rest" :: [(Value, String)]
      x `shouldBe` [(Num (-45.5786578), "rest")]

    it "test for double #3" $ do
      let x = reads "-45.578.6578rest" :: [(Value, String)]
      x `shouldBe` [(Num (-45.578), ".6578rest")]

    it "test for string #1" $ do
      let x = reads "\"Ciao Mondo\"cane" :: [(Value, String)]
      x `shouldBe` [(Str "Ciao Mondo", "cane")]

testReadSExpr :: IO ()
testReadSExpr = hspec $ do
  describe "test for reading S-expressions" $ do

    it "test for error #1" $ do
      let x = reads "." :: [(SExpr, String)]
      x `shouldBe` []

    it "test for error #2" $ do
      let x = reads ")" :: [(SExpr, String)]
      x `shouldBe` []

    it "test for error #3" $ do
      let x = reads " " :: [(SExpr, String)]
      x `shouldBe` []

    it "test for #nil #1" $ do
      let x = reads "()" :: [(SExpr, String)]
      x `shouldBe` [(Nil, "")]

    it "test for #nil #2" $ do
      let x = reads "()rest" :: [(SExpr, String)]
      x `shouldBe` [(Nil, "rest")]

    it "test for #t" $ do
      let x = reads "#t" :: [(SExpr, String)]
      x `shouldBe` [(Val (Bool True), "")]

    it "test for #f" $ do
      let x = reads "#f" :: [(SExpr, String)]
      x `shouldBe` [(Val (Bool False), "")]

    it "test for cons #1" $ do
      let x = reads "(asd)rest" :: [(SExpr, String)]
      x `shouldBe` [(Pair (Sym "asd") Nil, "rest")]

    it "test for cons #2" $ do
      let x = reads "( asd )rest" :: [(SExpr, String)]
      x `shouldBe` [(Pair (Sym "asd") Nil, "rest")]

    it "test for cons #3" $ do
      let x = reads "(asd . lol)rest" :: [(SExpr, String)]
      x `shouldBe` [(Pair (Sym "asd") (Sym "lol"), "rest")]

    it "test for cons #4" $ do
      let x = reads "(asd.lol)rest" :: [(SExpr, String)]
      x `shouldBe` [(Pair (Sym "asd") (Sym "lol"), "rest")]

    it "test for cons #5" $ do
      let x = reads "(asd lol)rest" :: [(SExpr, String)]
      x `shouldBe` [(Pair (Sym "asd") (Pair (Sym "lol") Nil), "rest")]

testEvalSExpr :: IO ()
testEvalSExpr = hspec $ do
  describe "test for evaluating S-expressions" $ do

    it "test for #nil #1" $ do
      let [(expr, "")] = reads "()" :: [(SExpr, String)]
          Effect expr' = eval ([], expr)
      Right (env, x) <- expr'
      x `shouldBe` Nil

    it "test for cons #1" $ do
      let [(expr, _)] = reads "(cons #t #f)" :: [(SExpr, String)]
          [(res, _)] = reads "(#t . #f)" :: [(SExpr, String)]
      let Effect expr' = eval ([], expr)
      Right (env, expr') <- expr'
      (env, expr') `shouldBe` ([], res)

    it "test for car #1" $ do
      let [(expr, _)] = reads "(car (cons #t #f))" :: [(SExpr, String)]
          [(res, _)] = reads "#t" :: [(SExpr, String)]
      let Effect expr' = eval ([], expr)
      Right (env, expr') <- expr'
      (env, expr') `shouldBe` ([], res)

    it "test for cdr #1" $ do
      let [(expr, _)] = reads "(cdr (cons #t #f))" :: [(SExpr, String)]
          [(res, _)] = reads "#f" :: [(SExpr, String)]
      let Effect expr' = eval ([], expr)
      Right (env, expr') <- expr'
      (env, expr') `shouldBe` ([], res)

    it "test for eq #1" $ do
      let [(expr, _)] = reads "(eq #t (car (cons #t #f)))" :: [(SExpr, String)]
          [(res, _)] = reads "#t" :: [(SExpr, String)]
      let Effect expr' = eval ([], expr)
      Right (env, expr') <- expr'
      (env, expr') `shouldBe` ([], res)

    it "test for atom #1" $ do
      let [(expr, _)] = reads "(atom (cons #t #f))" :: [(SExpr, String)]
          [(res, _)] = reads "#f" :: [(SExpr, String)]
      let Effect expr' = eval ([], expr)
      Right (env, expr') <- expr'
      (env, expr') `shouldBe` ([], res)

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
