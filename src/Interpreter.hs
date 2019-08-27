module Interpreter where

import qualified Data.Map as Map

import Utils
import Parser

toList :: SExpr -> [SExpr]
toList Nil = []
toList (Pair x y) = x:(toList y)

toCoupleList :: SExpr -> [(SExpr, SExpr)]
toCoupleList Nil = []
toCoupleList (Pair (Pair x (Pair y Nil)) z) = (x, y):(toCoupleList z)

-- bind symbols to values
bind :: SExpr -> SExpr -> Env -> Env
bind Nil Nil env = env
bind (Pair (Symb x) xs) (Pair val vals) env = Map.insert x val (bind xs vals env)

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
evalEq :: Env -> SExpr -> SExpr -> Effect (Env, SExpr)
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
        (Integer a, Integer b) -> return (env, Integer (a `mod` b))
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

eval env Car = return (env, Spec $ \e (Pair a Nil) -> evalCar e a)
eval env Cdr = return (env, Spec $ \e (Pair a Nil) -> evalCdr e a)
eval env Cons = return (env, Spec $ \e (Pair a (Pair b Nil)) -> evalCons e a b)
eval env Eqq = return (env, Spec $ \e (Pair a (Pair b Nil)) -> evalEq e a b)
eval env Atom = return (env, Spec $ \e (Pair a Nil) -> evalAtom e a)
eval env Cond = return (env, Spec $ \e x -> evalCond e (toCoupleList x))
eval env If = return (env, Spec $ \e (Pair a (Pair b (Pair c Nil))) -> evalIf e a b c)
eval env Quote = return (env, Spec $ \e (Pair a Nil) -> evalQuote e a)
eval env Define = return (env, Spec $ \e (Pair a (Pair b Nil)) -> evalDefine e a b)
eval env Lambda = return (env, Spec $ \e (Pair a (Pair b Nil)) -> evalLambda e a b)
eval env Plus = return (env, Spec $ \e x -> evalPlus e (toList x))
eval env Mult = return (env, Spec $ \e x -> evalMult e (toList x))
eval env Minus = return (env, Spec $ \e (Pair a (Pair b Nil)) -> evalMinus e a b)
eval env Div = return (env, Spec $ \e (Pair a (Pair b Nil)) -> evalDiv e a b)
eval env Mod = return (env, Spec $ \e (Pair a (Pair b Nil)) -> evalMod e a b)
eval env Less = return (env, Spec $ \e (Pair a (Pair b Nil)) -> evalLess e a b)
eval env Greater = return (env, Spec $ \e (Pair a (Pair b Nil)) -> evalGreater e a b)
-- evaluation of symbols (lookup)
eval env (Symb x) = case Map.lookup x env of
  Just x' -> return (env, x')
  Nothing -> runtimeError $ "Unbound symbol " ++ x
-- evaluation of built-in functions (function application) and special forms (custom evaluation)
eval env (CAR x) = evalCar env x
eval env (CDR x) = evalCdr env x
eval env (CONS x y) = evalCons env x y
eval env (EQQ x y) = evalEq env x y
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
    Spec g -> g env x
    _ -> runtimeError $ "Unknown expression " ++ show f'
-- evaluation of other cases
eval _ _ = runtimeError $ "Unknown expression(???)"

-- evaluate all elements in a list
evalList :: Env -> SExpr -> Effect (Env, SExpr)
evalList env Nil = return (env, Nil)
evalList env (Pair x xs) = do
  (_, x') <- eval env x
  (_, xs') <- evalList env xs
  return (env, Pair x' xs')
