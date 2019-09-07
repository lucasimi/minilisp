module Parser where

import qualified Data.Map as Map

import Utils
import Tokenizer

-- environment data type, where variables are stored
type LocalEnv = Map.Map String SExpr
type Env = [LocalEnv]

-- S-Expression data type
data SExpr = Nil
           | T
           | F
           | Integer Integer
           | Double Double
           | String String
           | Symb String
           | Pair SExpr SExpr
           | Func (Env -> [SExpr] -> Effect (Env, SExpr))
           | CAR SExpr
           | CDR SExpr
           | CONS SExpr SExpr
           | ATOM SExpr
           | EQQ SExpr SExpr
           | QUOTE SExpr
           | COND [(SExpr, SExpr)]
           | IF SExpr SExpr SExpr
           | LAMBDA [SExpr] SExpr
           | LABEL SExpr SExpr
           | DEFINE SExpr SExpr
           | PLUS [SExpr]
           | MINUS [SExpr]
           | MULT [SExpr]
           | DIV SExpr SExpr
           | MOD SExpr SExpr
           | LESS SExpr SExpr
           | GREATER SExpr SExpr

instance Show SExpr where
  show Nil = "()"
  show T = "#t"
  show F = "#f"
  show (Integer x) = show x
  show (Double x) = show x
  show (String x) = show x
  show (Symb x) = x
  show (Pair x Nil) = "(" ++ show x ++ ")"
  show (Pair x y) = case isList (Pair x y) of
    True -> "(" ++ show x ++ " " ++ (tail $ show y)
    False -> "(" ++ show x ++ " . " ++ show y ++ ")"
    where isList Nil = True
          isList (Pair x y) = isList y
          isList _ = False
  show (Func _) = "<function>"
  show _ = "<unevaluated>"

compile :: AST -> SExpr
compile Empty = Nil

compile (Leaf (BoolType True)) = T
compile (Leaf (BoolType False)) = F
compile (Leaf (IntegerType x)) = Integer x
compile (Leaf (DoubleType x)) = Double x
compile (Leaf (StringType x)) = String x
compile (Leaf (SymbType x)) = Symb x

compile (Node (Leaf (SymbType "car")) (Node x Empty)) = CAR (compile x)
compile (Node (Leaf (SymbType "cdr")) (Node x Empty)) = CDR (compile x)
compile (Node (Leaf (SymbType "cons")) (Node x (Node y Empty))) = CONS (compile x) (compile y)
compile (Node (Leaf (SymbType "atom")) (Node x Empty)) = ATOM (compile x)
compile (Node (Leaf (SymbType "eq")) (Node x (Node y Empty))) = EQQ (compile x) (compile y)
compile (Node (Leaf (SymbType "quote")) (Node x Empty)) = QUOTE (compile x)
compile (Node (Leaf (SymbType "cond")) x) = COND (compileCoupleList x)
compile (Node (Leaf (SymbType "if")) (Node x (Node y (Node z Empty)))) = IF (compile x) (compile y) (compile z)
compile (Node (Leaf (SymbType "lambda")) (Node x (Node y Empty))) = LAMBDA (compileList x) (compile y)
compile (Node (Leaf (SymbType "label")) (Node x (Node y Empty))) = LABEL (compile x) (compile y)
compile (Node (Leaf (SymbType "define")) (Node x (Node y Empty))) = DEFINE (compile x) (compile y)
compile (Node (Leaf (SymbType "+")) x) = PLUS (compileList x)
compile (Node (Leaf (SymbType "*")) x) = MULT (compileList x)
compile (Node (Leaf (SymbType "-")) x) = MINUS (compileList x)
compile (Node (Leaf (SymbType "/")) (Node x (Node y Empty))) = DIV (compile x) (compile y)
compile (Node (Leaf (SymbType "%")) (Node x (Node y Empty))) = MOD (compile x) (compile y)
compile (Node (Leaf (SymbType "<")) (Node x (Node y Empty))) = LESS (compile x) (compile y)
compile (Node (Leaf (SymbType ">")) (Node x (Node y Empty))) = GREATER (compile x) (compile y)

compile (Node x y) = Pair (compile x) (compile y)

compileList :: AST -> [SExpr]
compileList Empty = []
compileList (Node x y) = (compile x):(compileList y)

compileCoupleList :: AST -> [(SExpr, SExpr)]
compileCoupleList Empty = []
compileCoupleList (Node (Node x (Node y Empty)) z) = (compile x, compile y):(compileCoupleList z)
