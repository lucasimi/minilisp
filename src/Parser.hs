module Parser where

import qualified Data.Map as Map

import SExpr
import Utils

-- all admitted types for primitive type
data Token = IntegerType Integer
           | DoubleType Double
           | BoolType Bool
           | StringType String
           | SymbType String
           deriving (Show, Eq)

instance Read Token where
  readsPrec _ str = case dropLeadingBlanks str of
    "" -> []
    '(':_ -> []
    ')':_ -> []
    '.':_ -> []
    str' -> case reads str' :: [(String, String)] of
      [(x, s)] -> [(StringType x, s)]
      _ -> case reads str' :: [(Integer, String)] of
        [(x, s)] -> [(IntegerType x, s)]
        _ -> case reads str' :: [(Double, String)] of
          [(x, s)] -> [(DoubleType x, s)]
          _ -> case split str' of
            ("#t", s) -> [(BoolType True, s)]
            ("#f", s) -> [(BoolType False, s)]
            ('\"':_, _) -> [] -- a symbol cannot begin with a double quote
            (str'', s) -> [(SymbType str'', s)]

data TokenTree = Empty
               | Leaf Token
               | Node TokenTree TokenTree deriving (Show, Eq)

instance Read TokenTree where
  readsPrec _ str = case dropLeadingBlanks str of
    "" -> []
    ')':_ -> []
    '.':_ -> []
    '(':str' -> case dropLeadingBlanks str' of
      ')':str'' -> [(Empty, str'')]
      _ -> case reads str' :: [(TokenTree, String)] of
        [(x, str'')] -> case dropLeadingBlanks str'' of
          "" -> []
          ')':str''' -> [(Node x Empty, str''')]
          '.':str''' -> case reads ('(':str''') :: [(TokenTree, String)] of
            [(Node y Empty, str'''')] -> [(Node x y, str'''')]
            _ -> []
          _ -> case reads ('(':str'') :: [(TokenTree, String)] of
            [(y, str''')] -> [(Node x y, str''')]
            _ -> []
        _ -> []
    _ -> case reads str :: [(Token, String)] of
      [(x, str')] -> [(Leaf x, str')]
      _ -> []

compile :: TokenTree -> SExpr
compile Empty = Nil

compile (Leaf (BoolType True)) = T
compile (Leaf (BoolType False)) = F
compile (Leaf (IntegerType x)) = Integer x
compile (Leaf (DoubleType x)) = Double x
compile (Leaf (StringType x)) = String x
compile (Leaf (SymbType ('\'':x))) = QUOTE (Symb x)
compile (Leaf (SymbType x)) = Symb x

compile (Node (Leaf (SymbType "car")) (Node x Empty)) = CAR (compile x)
compile (Node (Leaf (SymbType "cdr")) (Node x Empty)) = CDR (compile x)
compile (Node (Leaf (SymbType "cons")) (Node x (Node y Empty))) = CONS (compile x) (compile y)
compile (Node (Leaf (SymbType "atom")) (Node x Empty)) = ATOM (compile x)
compile (Node (Leaf (SymbType "eq")) (Node x (Node y Empty))) = EQQ (compile x) (compile y)
compile (Node (Leaf (SymbType "quote")) (Node x Empty)) = QUOTE (compile x)
compile (Node (Leaf (SymbType "\'")) (Node x Empty)) = QUOTE (compile x)
compile (Node (Leaf (SymbType "cond")) x) = COND (compileCoupleList x)
compile (Node (Leaf (SymbType "if")) (Node x (Node y (Node z Empty)))) = IF (compile x) (compile y) (compile z)
compile (Node (Leaf (SymbType "lambda")) (Node x (Node y Empty))) = Lambda (compileList x) (compile y)
compile (Node (Leaf (SymbType "label")) (Node x (Node y Empty))) = LABEL (compile x) (compile y)
compile (Node (Leaf (SymbType "let")) (Node x (Node y (Node z Empty)))) = LET (compile x) (compile y) (compile z)
compile (Node (Leaf (SymbType "define")) (Node x (Node y Empty))) = DEFINE (compile x) (compile y)
compile (Node (Leaf (SymbType "+")) x) = PLUS (compileList x)
compile (Node (Leaf (SymbType "*")) x) = MULT (compileList x)
compile (Node (Leaf (SymbType "-")) x) = MINUS (compileList x)
compile (Node (Leaf (SymbType "/")) x) = DIV (compileList x)
compile (Node (Leaf (SymbType "%")) (Node x (Node y Empty))) = MOD (compile x) (compile y)
compile (Node (Leaf (SymbType "<")) x) = LESS (compileList x)
compile (Node (Leaf (SymbType ">")) x) = GREATER (compileList x)
compile (Node (Leaf (SymbType "or")) x) = OR (compileList x)
compile (Node (Leaf (SymbType "and")) x) = AND (compileList x)
compile (Node (Leaf (SymbType "not")) (Node x Empty)) = NOT (compile x)
compile (Node (Leaf (SymbType "list")) x) = LIST (compileList x)

compile (Node x y) = Pair (compile x) (compile y)

compileList :: TokenTree -> [SExpr]
compileList Empty = []
compileList (Node x y) = (compile x):(compileList y)

compileCoupleList :: TokenTree -> [(SExpr, SExpr)]
compileCoupleList Empty = []
compileCoupleList (Node (Node x (Node y Empty)) z) = (compile x, compile y):(compileCoupleList z)

parse :: String -> Maybe SExpr
parse str = case reads str :: [(TokenTree, String)] of
  [(tree, str')] -> case isBlank str' of
    True -> Just $ compile tree
    False -> Nothing
  _ -> Nothing
