module Data.ParseTree where

import Data.Token
import Utils

data ParseTree = Empty
               | Leaf Token
               | Node ParseTree ParseTree deriving (Show, Eq)

instance Read ParseTree where
  readsPrec _ str = case dropLeadingBlanks str of
    "" -> []
    ')':_ -> []
    '.':_ -> []
    '\'':str' -> case reads str' :: [(ParseTree, String)] of
      [(x, str'')] -> [(Node (Leaf (SymbType "\'")) (Node x Empty), str'')]
      _ -> [(Empty, '\'':str')]
    '(':str' -> case dropLeadingBlanks str' of
      ')':str'' -> [(Empty, str'')]
      _ -> case reads str' :: [(ParseTree, String)] of
        [(x, str'')] -> case dropLeadingBlanks str'' of
          "" -> []
          ')':str''' -> [(Node x Empty, str''')]
          '.':str''' -> case reads ('(':str''') :: [(ParseTree, String)] of
            [(Node y Empty, str'''')] -> [(Node x y, str'''')]
            _ -> []
          _ -> case reads ('(':str'') :: [(ParseTree, String)] of
            [(y, str''')] -> [(Node x y, str''')]
            _ -> []
        _ -> []
    _ -> case reads str :: [(Token, String)] of
      [(x, str')] -> [(Leaf x, str')]
      _ -> []
