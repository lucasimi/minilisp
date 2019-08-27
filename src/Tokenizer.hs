module Tokenizer where

import Utils

-- all admitted TypeTypes for Primic type
data Token = IntegerType Integer
           | DoubleType Double
           | BoolType Bool
           | StringType String
           | SymbType String
           deriving Show

instance Read Token where
  readsPrec _ str =
    case reads str :: [(String, String)] of
      [(x, s)] -> [(StringType x, s)]
      _ -> case split str of
        ("", '(':_) -> []
        ("", ')':_) -> []
        ("", '.':_) -> []
        (str', s) -> case reads str' :: [(Integer, String)] of
          [(x, s')] -> [(IntegerType x, s' ++ s)]
          _ -> case reads str' :: [(Double, String)] of
            [(x, s')] -> [(DoubleType x, s' ++ s)]
            _ -> case split str' of
              ("#t", s') -> [(BoolType True, s' ++ s)]
              ("#f", s') -> [(BoolType False, s' ++ s)]
              (str'', s') -> [(SymbType str'', s' ++ s)]

data AST = Empty | Leaf Token | Node AST AST deriving Show

instance Read AST where
  readsPrec _ str = case reads str :: [(Token, String)] of
    [(x, s)] -> [(Leaf x, s)]
    _ -> case split str of
      ("", '(':str') -> case reads str' :: [(AST, String)] of
        [(x, str'')] -> case split str'' of
          ("", ')':str''') -> [(Node x Empty, str''')]
          ("", '.':str''') -> case reads ('(':str''') :: [(AST, String)] of
              [(Node y Empty, str'''')] -> [(Node x y, str'''')]
              _ -> []
          _ -> case reads ('(':str'') :: [(AST, String)] of
              [(y, s)] -> [(Node x y, s)]
              _ -> []
        _ -> case split str' of
          ("", ')':str'') -> [(Empty, str'')]
          _ -> []
