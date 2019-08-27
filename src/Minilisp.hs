module Minilisp where

import Eval
import qualified Data.Map as Map

-- drop all blank leading characters from a string
dropLeadingBlanks :: String -> String
dropLeadingBlanks = dropWhile (`elem` " \t\n")

-- splitOnDelimiter a string where a delimiter is found
splitOnDelimiter :: String -> (String, String)
splitOnDelimiter = (break (`elem` " \t\n()."))

split :: String -> (String, String)
split = splitOnDelimiter . dropLeadingBlanks

-- all admitted TypeTypes for Primic type
data TokenType = IntegerType Integer
               | DoubleType Double
               | BoolType Bool
               | StringType String
               | SymbType String
               deriving Show

instance Read TokenType where
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

data TokenTree = Empty | Leaf TokenType | Node TokenTree TokenTree deriving Show

instance Read TokenTree where
  readsPrec _ str = case reads str :: [(TokenType, String)] of
    [(x, s)] -> [(Leaf x, s)]
    _ -> case split str of
      ("", '(':str') -> case reads str' :: [(TokenTree, String)] of
        [(x, str'')] -> case split str'' of
          ("", ')':str''') -> [(Node x Empty, str''')]
          ("", '.':str''') -> case reads ('(':str''') :: [(TokenTree, String)] of
              [(Node y Empty, str'''')] -> [(Node x y, str'''')]
              _ -> []
          _ -> case reads ('(':str'') :: [(TokenTree, String)] of
              [(y, s)] -> [(Node x y, s)]
              _ -> []
        _ -> case split str' of
          ("", ')':str'') -> [(Empty, str'')]
          _ -> []

-- S-Expression data type
data SExpr = Nil
           | T
           | F
           | Integer Integer
           | Double Double
           | String String
           | Symb String
           | Pair SExpr SExpr
           | Func (Env -> SExpr -> Effect (Env, SExpr))
           | CAR SExpr
           | CDR SExpr
           | CONS SExpr SExpr
           | ATOM SExpr
           | EQ SExpr SExpr
           | QUOTE SExpr
           | COND [(SExpr, SExpr)]
           | IF SExpr SExpr SExpr
           | LAMBDA SExpr SExpr
           | DEFINE SExpr SExpr
           | PLUS [SExpr]
           | MINUS SExpr SExpr
           | MULT [SExpr]
           | DIV SExpr SExpr
           | MOD SExpr SExpr
           | LESS SExpr SExpr
           | GREATER SExpr SExpr

compile :: TokenTree -> SExpr
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
compile (Node (Leaf (SymbType "eq")) (Node x (Node y Empty))) = Minilisp.EQ (compile x) (compile y)
compile (Node (Leaf (SymbType "quote")) (Node x Empty)) = QUOTE (compile x)
compile (Node (Leaf (SymbType "cond")) x) = COND (compileCoupleList x)
compile (Node (Leaf (SymbType "if")) (Node x (Node y (Node z Empty)))) = IF (compile x) (compile y) (compile z)
compile (Node (Leaf (SymbType "lambda")) (Node x (Node y Empty))) = LAMBDA (compile x) (compile y)
compile (Node (Leaf (SymbType "define")) (Node x (Node y Empty))) = DEFINE (compile x) (compile y)
compile (Node (Leaf (SymbType "+")) x) = PLUS (compileList x)
compile (Node (Leaf (SymbType "*")) x) = MULT (compileList x)
compile (Node (Leaf (SymbType "-")) (Node x (Node y Empty))) = MINUS (compile x) (compile y)
compile (Node (Leaf (SymbType "/")) (Node x (Node y Empty))) = DIV (compile x) (compile y)
compile (Node (Leaf (SymbType "%")) (Node x (Node y Empty))) = MOD (compile x) (compile y)
compile (Node (Leaf (SymbType "<")) (Node x (Node y Empty))) = LESS (compile x) (compile y)
compile (Node (Leaf (SymbType ">")) (Node x (Node y Empty))) = GREATER (compile x) (compile y)
compile (Node x y) = Pair (compile x) (compile y)

compileList :: TokenTree -> [SExpr]
compileList Empty = []
compileList (Node x y) = (compile x):(compileList y)

compileCoupleList :: TokenTree -> [(SExpr, SExpr)]
compileCoupleList Empty = []
compileCoupleList (Node (Node x (Node y Empty)) z) = (compile x, compile y):(compileCoupleList z)

instance Show SExpr where
  show Nil = "()"
  show T = "#t"
  show F = "#f"
  show (Integer x) = show x
  show (Double x) = show x
  show (String x) = show x
  show (Symb x) = x
  show (Pair x Nil) = "(" ++ show x ++ ")"
  show (Pair x y) = if isList (Pair x y)
                    then "(" ++ show x ++ " " ++ (tail $ show y)
                    else "(" ++ show x ++ " . " ++ show y ++ ")"
    where isList Nil = True
          isList (Pair x y) = isList y
          isList _ = False
  show (Func _) = "<function>"
{--
instance Read SExpr where
  readsPrec _ str = case dropLeadingBlanks str of
    "" -> []
    '.':_ -> []
    ')':_ -> []
    '(':str' -> case dropLeadingBlanks str' of
      ')':str'' -> [(Nil, str'')]
      str'' -> case reads str'' :: [(SExpr, String)] of
        [(x, str''')] -> case dropLeadingBlanks str''' of
          '.':str'''' -> case reads ('(':str'''') :: [(SExpr, String)] of
            [(Pair y Nil, str''''')] -> [(Pair x y, str''''')]
            _ -> []
          str'''' -> case reads ('(':str'''') :: [(SExpr, String)] of
            [(y, str''''')] -> [(Pair x y, str''''')]
            _ -> []
        _ -> []
    str' -> case reads str' :: [(Type, String)] of
      [(x, s)] -> [(Prim x, s)]
      _ -> case splitOnDelimiter str' of
        (x, str'') -> [(Symb x, str'')]
--}
-- monad which wraps effectful computations (which may fail)
data Effect a = Effect (IO (Either Error a))

instance Functor Effect where
  fmap f (Effect x) = Effect $ do
    x' <- x
    case x' of
      Left err -> do
        --putStrLn $ "[ERROR] " ++ show err
        return $ Left err
      Right a -> return $ Right $ f a

instance Applicative Effect where
  pure a = Effect $ return $ Right a

  (Effect f) <*> (Effect x) = Effect $ do
    x' <- x
    case x' of
      Left errx -> do
        --putStrLn $ "[ERROR] " ++ show errx
        return $ Left errx
      Right a -> do
        f' <- f
        case f' of
          Left errf -> do
            --putStrLn $ "[ERROR] " ++ show errf
            return $ Left errf
          Right g -> return $ Right $ g a

instance Monad Effect where
  (Effect x) >>= f = Effect $ do
    x' <- x
    case x' of
      Left err -> do
        --putStrLn $ "[ERROR] " ++ show err
        return $ Left err
      Right a -> do
        let Effect y = f a
        y' <- y
        return y'

-- environment data type, where variables are stored
type Env = Map.Map String SExpr

-- bind symbols to values
bind :: SExpr -> SExpr -> Env -> Env
bind Nil Nil env = env
bind (Pair (Symb x) xs) (Pair val vals) env = Map.insert x val (bind xs vals env)

-- evaluation wrapper data type
type Ev = Eval Effect Env

-- error data type
data Error = RuntimeErr String
           deriving Show

-- returns a runtime error with specified message
runtimeError :: String -> Effect (Env, SExpr)
runtimeError x = Effect $ return $ Left $ RuntimeErr x

-- evaluation of "car"
evalCar :: Env -> SExpr -> Effect (Env, SExpr)
evalCar env x = do
  (_, x') <- eval env x
  case x' of
    Pair a _ -> return (env, a)

-- evaluation of "cdr"
evalCdr :: Env -> SExpr -> Effect (Env, SExpr)
evalCdr env x = do
  (_, x') <- eval env x
  case x' of
    Pair _ b -> return (env, b)

-- evaluation of "cons"
evalCons :: Env -> SExpr -> SExpr -> Effect (Env, SExpr)
evalCons env x y = do
  (_, x') <- eval env x
  (_, y') <- eval env y
  return (env, Pair x' y')

-- evaluation of "eq"
evalEq :: Env -> SExpr -> SExpr -> Effect(Env, SExpr)
evalEq env x y = do
  (_, x') <- eval env x
  (_, y') <- eval env y
  case (x', y') of
    (Integer a, Integer b) -> case a == b of
      True -> return (env, T)
      False -> return (env, F)
    (Double a, Double b) -> case a == b of
      True -> return (env, T)
      False -> return (env, F)
    (String a, String b) -> case a == b of
      True -> return (env, T)
      False -> return (env, F)
    _ -> return (env, F)

-- evaluation of "atom"
evalAtom :: Env -> SExpr -> Effect (Env, SExpr)
evalAtom env x = do
  (_, x') <- eval env x
  case x' of
    Nil -> return (env, T)
    Integer _ -> return (env, T)
    Double _ -> return (env, T)
    String _ -> return (env, T)
    _ -> return (env, F)

-- evaluation of "cond" special form
evalCond :: Env -> [(SExpr, SExpr)] -> Effect (Env, SExpr)
evalCond env ((x, y):xs) = do
  (_, x') <- eval env x
  case x' of
    T -> eval env y
    F -> evalCond env xs
    _ -> runtimeError "Non boolean value as conditional"

evalIf :: Env -> SExpr -> SExpr -> SExpr -> Effect (Env, SExpr)
evalIf env x y z = do
  (_, x') <- eval env x
  case x' of
    T -> eval env y
    F -> eval env z
    _ -> runtimeError "Non boolean value as conditional"

-- evaluation of "quote" special form
evalQuote :: Env -> SExpr -> Effect (Env, SExpr)
evalQuote env x = do
  return (env, x)

-- evaluation of "lambda" special form
evalLambda :: Env -> SExpr -> SExpr -> Effect (Env, SExpr)
evalLambda env x y = do
  return (env, Func (\e a -> eval (bind x a e) y))

-- evaluation of "label" special form
evalLabel :: (Env, SExpr) -> Effect (Env, SExpr)
evalLabel (env, Pair (Symb x) (Pair y Nil)) = do
  (_, y') <- eval env y
  case  y' of
    Func f -> return (env, Func g)
      where g = \e a -> eval (bind (Pair (Symb x) Nil) (Pair (Func g) Nil) e) y

-- evaluation of "define" special form
evalDefine :: Env -> SExpr -> SExpr -> Effect (Env, SExpr)
evalDefine env (Symb x) y = do
  (_, y') <- eval env y
  return (Map.insert x y' env, Symb x)

-- evaluation of "+"
evalPlus :: Env -> [SExpr] -> Effect (Env, SExpr)
evalPlus env [x] = do
  (_, x') <- eval env x
  case x' of
    Integer a -> return (env, x')
    Double a -> return (env, x')
    _ -> runtimeError $ "Non numerical value " ++ show x'
evalPlus env (x:xs) = do
  (_, xs') <- evalPlus env xs
  (_, x') <- eval env x
  case (x', xs') of
    (Integer a, Integer b) -> return (env, Integer (a + b))
    (Double a, Double b) -> return (env, Double (a + b))
    _ -> runtimeError "Numeric type mismatch"

-- evaluation of "-"
evalMinus :: Env -> SExpr -> SExpr -> Effect (Env, SExpr)
evalMinus env x y = do
  (_, x') <- eval env x
  (_, y') <- eval env y
  case (x', y') of
    (Integer a, Integer b) -> return (env, Integer (a - b))
    (Double a, Double b) -> return (env, Double (a - b))
    _ -> runtimeError "Type mismatch"

-- evaluation of "*"
evalMult :: Env -> [SExpr] -> Effect (Env, SExpr)
evalMult env [x] = do
  (_, x') <- eval env x
  case x' of
    Integer a -> return (env, x')
    Double a -> return (env, x')
    _ -> runtimeError $ "Non numerical value " ++ show x'
evalMult env (x:xs) = do
  (_, xs') <- evalMult env xs
  (_, x') <- eval env x
  case (x', xs') of
    (Integer a, Integer b) -> return (env, Integer (a * b))
    (Double a, Double b) -> return (env, Double (a * b))
    _ -> runtimeError "Type mismatch"

-- evaluation of "/"
evalDiv :: Env -> SExpr -> SExpr -> Effect (Env, SExpr)
evalDiv env x y = do
  (_, y') <- eval env y
  case y' of
    Integer 0 -> runtimeError "Division by 0"
    Double 0.0 -> runtimeError "Division by 0.0"
    _ -> do
      (_, x') <- eval env x
      case (x', y') of
        (Integer a,Integer b) -> return (env, Integer (a `div` b))
        (Double a, Double b) -> return (env, Double (a / b))
        _ -> runtimeError $ "Type mismatch"

-- evaluation of "%"
evalMod :: Env -> SExpr -> SExpr -> Effect (Env, SExpr)
evalMod env x y = do
  (_, y') <- eval env y
  case y' of
    Integer 0 -> runtimeError "Division by 0"
    _ -> do
      (_, x') <- eval env x
      case (x', y') of
        (Integer a,Integer b) -> return (env, Integer (a `mod` b))
        _ -> runtimeError $ "Type mismatch"

-- evaluation of "<"
evalLess :: Env -> SExpr -> SExpr -> Effect (Env, SExpr)
evalLess env x y = do
  (_, x') <- eval env x
  (_, y') <- eval env y
  case (x', y') of
    (Integer a, Integer b) -> case a < b of
      True -> return (env,T)
      False -> return (env, F)
    (Double a, Double b) -> case a < b of
      True -> return (env, T)
      False -> return (env, F)
    _ -> runtimeError "Type mismatch"

-- evaluation of ">"
evalGreater :: Env -> SExpr -> SExpr -> Effect (Env, SExpr)
evalGreater env x y = do
  (_, x') <- eval env x
  (_, y') <- eval env y
  case (x', y') of
    (Integer a, Integer b) -> case a > b of
      True -> return (env,T)
      False -> return (env, F)
    (Double a, Double b) -> case a > b of
      True -> return (env, T)
      False -> return (env, F)
    _ -> runtimeError "Type mismatch"

-- evaluation function, the core of the lisp interpreter
eval :: Env -> SExpr -> Effect (Env, SExpr)
-- evaluation of atomic values (self-evaluation)
eval env Nil = return (env, Nil)
eval env T = return (env, T)
eval env F = return (env, F)
eval env (Integer x) = return (env, Integer x)
eval env (Double x) = return (env, Double x)
eval env (String x) = return (env, String x)
-- evaluation of symbols (lookup)
eval env (Symb x) = case Map.lookup x env of
  Just x' -> return (env, x')
  Nothing -> runtimeError $ "Unbound symbol " ++ x
-- evaluation of built-in functions (function application) and special forms (custom evaluation)
eval env (CAR x) = evalCar env x
eval env (CDR x) = evalCdr env x
eval env (CONS x y) = evalCons env x y
eval env (Minilisp.EQ x y) = evalEq env x y
eval env (ATOM x) = evalAtom env x
eval env (COND x) = evalCond env x
eval env (IF x y z) = evalIf env x y z
eval env (QUOTE x) = evalQuote env x
eval env (DEFINE x y) = evalDefine env x y
eval env (LAMBDA x y) = evalLambda env x y
--eval (env, LABEL x y) = evalLabel (env, x)
eval env (PLUS x) = evalPlus env x
eval env (MINUS x y) = evalMinus env x y
eval env (MULT x) = evalMult env x
eval env (DIV x y) = evalDiv env x y
eval env (MOD x y) = evalMod env x y
eval env (LESS x y) = evalLess env x y
eval env (GREATER x y) = evalGreater env x y

-- evaluation of non built-in functions
eval env (Pair f x) = do
  (_, f') <- eval env f
  case f' of
    Func g -> do
      (_, x') <- evalList env x
      (_, g') <- g env x'
      return (env, g')
    _ -> runtimeError $ "Unknown expression"
-- evaluation of other cases
eval _ _ = runtimeError $ "Unknown expression"

-- evaluate all elements in a list
evalList :: Env -> SExpr -> Effect (Env, SExpr)
evalList env Nil = return (env, Nil)
evalList env (Pair x xs) = do
  (_, x') <- eval env x
  (_, xs') <- evalList env xs
  return (env, Pair x' xs')

-- evaluate a function on an arguments list
evalFunc :: (Env -> SExpr -> Effect (Env, SExpr)) -> Env -> SExpr -> Effect (Env, SExpr)
evalFunc func env args = do
  (_, args') <- evalList env args
  (_, res) <- func env args'
  return (env, res)
