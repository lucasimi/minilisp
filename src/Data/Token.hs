module Data.Token where

import Utils

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
