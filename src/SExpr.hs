module SExpr where

-- S-Expression data type
data SExpr = Nil
           | T
           | F
           | Integer Integer
           | Double Double
           | String String
           | Symb String
           | Lambda [SExpr] SExpr
           | Pair SExpr SExpr
           | CAR SExpr
           | CDR SExpr
           | CONS SExpr SExpr
           | ATOM SExpr
           | EQQ SExpr SExpr
           | QUOTE SExpr
           | COND [(SExpr, SExpr)]
           | IF SExpr SExpr SExpr
           | LABEL SExpr SExpr
           | LET SExpr SExpr SExpr
           | DEFINE SExpr SExpr
           | PLUS [SExpr]
           | MINUS [SExpr]
           | MULT [SExpr]
           | DIV [SExpr]
           | MOD SExpr SExpr
           | LESS [SExpr]
           | GREATER [SExpr]
           | OR [SExpr]
           | AND [SExpr]
           | NOT SExpr
           | LIST [SExpr]
           deriving Eq

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
  show (Lambda _ _) = "<function>"
  show _ = "<unevaluated>"
