module Tokenizer where

import Utils

-- all admitted TypeTypes for Primic type
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
            (str'', s) -> [(SymbType str'', s)]

data AST = Empty | Leaf Token | Node AST AST deriving (Show, Eq)

instance Read AST where
  readsPrec _ str = case dropLeadingBlanks str of
    "" -> []
    ')':_ -> []
    '.':_ -> []
    '(':str' -> case dropLeadingBlanks str' of
      ')':str'' -> [(Empty, str'')]
      _ -> case reads str' :: [(AST, String)] of
        [(x, str'')] -> case dropLeadingBlanks str'' of
          "" -> []
          ')':str''' -> [(Node x Empty, str''')]
          '.':str''' -> case reads ('(':str''') :: [(AST, String)] of
            [(Node y Empty, str'''')] -> [(Node x y, str'''')]
            _ -> []
          _ -> case reads ('(':str'') :: [(AST, String)] of
            [(y, str''')] -> [(Node x y, str''')]
            _ -> []
        _ -> []
    _ -> case reads str :: [(Token, String)] of
      [(x, str')] -> [(Leaf x, str')]
      _ -> []
