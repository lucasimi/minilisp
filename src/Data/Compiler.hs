module Data.Compiler where

import qualified Data.Map as Map

import Data.SExpr
import Data.Token
import Data.ParseTree
import Utils

compile :: ParseTree -> Maybe SExpr
compile Empty = Just Nil

compile (Leaf (BoolType True)) = Just T
compile (Leaf (BoolType False)) = Just F
compile (Leaf (IntegerType x)) = Just $ Integer x
compile (Leaf (DoubleType x)) = Just $ Double x
compile (Leaf (StringType x)) = Just $ String x
compile (Leaf (SymbType ('\'':x))) = Just $ QUOTE (Symb x)
compile (Leaf (SymbType x)) = Just $ Symb x

compile (Node (Leaf (SymbType "car")) (Node x Empty)) = do
  x' <- compile x
  return $ CAR x'

compile (Node (Leaf (SymbType "cdr")) (Node x Empty)) = do
  x' <- compile x
  return $ CDR x'

compile (Node (Leaf (SymbType "cons")) (Node x (Node y Empty))) = do
  x' <- compile x
  y' <- compile y
  return $ CONS x' y'

compile (Node (Leaf (SymbType "atom")) (Node x Empty)) = do
  x' <- compile x
  return $ ATOM x'

compile (Node (Leaf (SymbType "eq")) (Node x (Node y Empty))) = do
  x' <- compile x
  y' <- compile y
  return $ EQQ x' y'

compile (Node (Leaf (SymbType "quote")) (Node x Empty)) = do
  x' <- compile x
  return $ QUOTE x'

compile (Node (Leaf (SymbType "\'")) (Node x Empty)) = do
  x' <- compile x
  return $ QUOTE x'

compile (Node (Leaf (SymbType "cond")) x) = do
  x' <- compileCoupleList x
  return $ COND x'

compile (Node (Leaf (SymbType "if")) (Node x (Node y (Node z Empty)))) = do
  x' <- compile x
  y' <- compile y
  z' <- compile z
  return $ IF x' y' z'

compile (Node (Leaf (SymbType "lambda")) (Node x (Node y Empty))) = do
  x' <- compileList x
  case isSymbList x' of
    True -> do
      y' <- compile y
      return $ Lambda x' y'
    False -> Nothing
  where isSymbList [] = True
        isSymbList (Symb _:_) = True
        isSymbList _ = False

compile (Node (Leaf (SymbType "label")) (Node x (Node y Empty))) = do
  x' <- compile x
  case x' of
    Symb _ -> do
      y' <- compile y
      return $ LABEL x' y'
    _ -> Nothing

compile (Node (Leaf (SymbType "let")) (Node x (Node y (Node z Empty)))) = do
  x' <- compile x
  case x' of
    Symb _ -> do
      y' <- compile y
      z' <- compile z
      return $ LET x' y' z'
    _ -> Nothing

compile (Node (Leaf (SymbType "define")) (Node x (Node y Empty))) = do
  x' <- compile x
  case x' of
    Symb _ -> do
      y' <- compile y
      return $ DEFINE x' y'
    _ -> Nothing

compile (Node (Leaf (SymbType "+")) x) = do
  x' <- compileList x
  return $ PLUS x'

compile (Node (Leaf (SymbType "*")) x) = do
  x' <- compileList x
  return $ MULT x'

compile (Node (Leaf (SymbType "-")) x) = do
  x' <- compileList x
  return $ MINUS x'

compile (Node (Leaf (SymbType "/")) x) = do
  x' <- compileList x
  return $ DIV x'

compile (Node (Leaf (SymbType "%")) (Node x (Node y Empty))) = do
  x' <- compile x
  y' <- compile y
  return $ MOD x' y'

compile (Node (Leaf (SymbType "<")) x) = do
  x' <- compileList x
  return $ LESS x'

compile (Node (Leaf (SymbType ">")) x) = do
  x' <- compileList x
  return $ GREATER x'

compile (Node (Leaf (SymbType "or")) x) = do
  x' <- compileList x
  return $ OR x'

compile (Node (Leaf (SymbType "and")) x) = do
  x' <- compileList x
  return $ AND x'

compile (Node (Leaf (SymbType "not")) (Node x Empty)) = do
  x' <- compile x
  return $ NOT x'

compile (Node (Leaf (SymbType "list")) x) = do
  x' <- compileList x
  return $ LIST x'

compile (Node x y) = do
  x' <- compile x
  y' <- compile y
  return $ Pair x' y'

compileList :: ParseTree -> Maybe [SExpr]
compileList Empty = Just []
compileList (Node x y) = do
  x' <- compile x
  y' <- compileList y
  return (x':y')
compileList _ = Nothing

compileCoupleList :: ParseTree -> Maybe [(SExpr, SExpr)]
compileCoupleList Empty = Just []
compileCoupleList (Node (Node x (Node y Empty)) z) = do
  x' <- compile x
  y' <- compile y
  z' <- compileCoupleList z
  return ((x', y'):z')
compileCoupleList _ = Nothing
