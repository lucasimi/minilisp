module SExpr where

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

showSExprList :: [SExpr] -> String
showSExprList [] = ""
showSExprList (x:xs) = show x ++ " " ++ showSExprList xs

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

  show (CAR x) = "(car " ++ show x ++ ")"
  show (CDR x) = "(cdr " ++ show x ++ ")"
  show (CONS x y) = "(cons " ++ show x ++ " " ++ show y ++ ")"
  show (ATOM x) = "(atom " ++ show x ++ ")"
  show (EQQ x y) = "(eq " ++ show x ++ " " ++ show y ++ ")"
  show (QUOTE x) = "\'" ++ show x
  show (PLUS x) = "(+ " ++ showSExprList x ++ ")"
  show (MINUS x) = "(- " ++ showSExprList x ++ ")"
  show (MULT x) = "(* " ++ showSExprList x ++ ")"
  show (DIV x) = "(/ " ++ showSExprList x ++ ")"
  show (LESS x) = "(< " ++ showSExprList x ++ ")"
  show (GREATER x) = "(> " ++ showSExprList x ++ ")"
  show (OR x) = "(or " ++ showSExprList x ++ ")"
  show (AND x) = "(and " ++ showSExprList x ++ ")"
  show (LIST x) = "(list " ++ showSExprList x ++ ")"

  show (IF x y z) = "(if " ++ show x ++ " " ++ show y ++ " " ++ show z ++ ")"
  show (LABEL x y) = "(label " ++ show x ++ " " ++ show y ++ ")"
  show (LET x y z) = "(let " ++ show x ++ " " ++ show y ++ " " ++ show z ++ ")"
  show (DEFINE x y) = "(define " ++ show x ++ " " ++ show y ++ ")"

  show (MOD x y) = "(% " ++ show x ++ " " ++ show y ++ ")"
  show (NOT x) = "(not " ++ show x ++ ")"
  show _ = "<unevaluated>"
