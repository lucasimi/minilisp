module Interpreter where

import qualified Data.Map as Map

import Utils
import Parser

printEnv :: Env -> Effect ()
printEnv env = Effect $ do
  putStrLn $ show env
  return $ Right ()

-- bind symbols to values
bindLocal :: [SExpr] -> [SExpr] -> LocalEnv -> LocalEnv
bindLocal [] [] env = env
bindLocal ((Symb x):xs) (val:vals) env = Map.insert x val (bindLocal xs vals env)
bindLocal _ _ env = env

bind :: [SExpr] -> [SExpr] -> Env -> Env
bind x y (env:envs) = (bindLocal x y env):envs

seek :: SExpr -> Env -> Maybe (Env, SExpr)
seek (Symb x) [] = Nothing
seek (Symb x) (env:envs) = case Map.lookup x env of
  Just y -> Just (env:envs, y)
  Nothing -> seek (Symb x) envs

-- returns a runtime error with specified message
runtimeError :: String -> Effect (LocalEnv, Env, SExpr)
runtimeError x = Effect $ return $ Left $ RuntimeErr x

-- evaluation of "car"
evalCar :: LocalEnv -> Env -> SExpr -> Effect (LocalEnv, Env, SExpr)
evalCar root branch x = do
  (_, _, x') <- eval root branch x
  case x' of
    Pair a _ -> return (root, branch, a)

-- evaluation of "cdr"
evalCdr :: LocalEnv -> Env -> SExpr -> Effect (LocalEnv, Env, SExpr)
evalCdr root branch x = do
  (_, _, x') <- eval root branch x
  case x' of
    Pair _ b -> return (root, branch, b)

-- evaluation of "cons"
evalCons :: LocalEnv -> Env -> SExpr -> SExpr -> Effect (LocalEnv, Env, SExpr)
evalCons root branch x y = do
  (_, _, x') <- eval root branch x
  (_, _, y') <- eval root branch y
  return (root, branch, Pair x' y')

-- evaluation of "eq"
evalEq :: LocalEnv -> Env -> SExpr -> SExpr -> Effect (LocalEnv, Env, SExpr)
evalEq root branch x y = do
  (_, _, x') <- eval root branch x
  (_, _, y') <- eval root branch y
  case (x', y') of
    (Integer a, Integer b) -> case a == b of
      True -> return (root, branch, T)
      False -> return (root, branch, F)
    (Double a, Double b) -> case a == b of
      True -> return (root, branch, T)
      False -> return (root, branch, F)
    (String a, String b) -> case a == b of
      True -> return (root, branch, T)
      False -> return (root, branch, F)
    _ -> return (root, branch, F)

-- evaluation of "atom"
evalAtom :: LocalEnv -> Env -> SExpr -> Effect (LocalEnv, Env, SExpr)
evalAtom root branch x = do
  (_, _, x') <- eval root branch x
  case x' of
    Nil -> return (root, branch, T)
    Integer _ -> return (root, branch, T)
    Double _ -> return (root, branch, T)
    String _ -> return (root, branch, T)
    _ -> return (root, branch, F)

-- evaluation of "cond" special form
evalCond :: LocalEnv -> Env -> [(SExpr, SExpr)] -> Effect (LocalEnv, Env, SExpr)
evalCond root branch ((x, y):xs) = do
  (_, _, x') <- eval root branch x
  case x' of
    T -> eval root branch y
    F -> evalCond root branch xs
    _ -> runtimeError "Non boolean value as conditional"

evalIf :: LocalEnv -> Env -> SExpr -> SExpr -> SExpr -> Effect (LocalEnv, Env, SExpr)
evalIf root branch x y z = do
  (_, _, x') <- eval root branch x
  case x' of
    T -> eval root branch y
    F -> eval root branch z
    _ -> runtimeError "Non boolean value as conditional"

-- evaluation of "quote" special form
evalQuote :: LocalEnv -> Env -> SExpr -> Effect (LocalEnv, Env, SExpr)
evalQuote root branch x = do
  return (root, branch, x)

-- evaluation of "lambda" special form
evalLambda :: LocalEnv -> Env -> [SExpr] -> SExpr -> Effect (LocalEnv, Env, SExpr)
evalLambda root branch x y = return (root, branch, Func x y)

-- evaluation of "define" special form
evalDefine :: LocalEnv -> Env -> SExpr -> SExpr -> Effect (LocalEnv, Env, SExpr)
evalDefine root branch (Symb x) y = do
  (_, _, y') <- eval root branch y
  return (head (bind [Symb x] [y'] [root]), branch, Symb x)

-- evaluation of "+"
evalPlus :: LocalEnv -> Env -> [SExpr] -> Effect (LocalEnv, Env, SExpr)
evalPlus root branch [x] = do
  (_, _, x') <- eval root branch x
  case x' of
    Integer a -> return (root, branch, x')
    Double a -> return (root, branch, x')
    _ -> runtimeError $ "Non numerical value " ++ show x'
evalPlus root branch (x:xs) = do
  (_, _, xs') <- evalPlus root branch xs
  (_, _, x') <- eval root branch x
  case (x', xs') of
    (Integer a, Integer b) -> return (root, branch, Integer (a + b))
    (Double a, Double b) -> return (root, branch, Double (a + b))
    _ -> runtimeError "Numeric type mismatch"

-- evaluation of "-"
evalMinus :: LocalEnv -> Env -> [SExpr] -> Effect (LocalEnv, Env, SExpr)
evalMinus root branch [x] = do
  (_, _, x') <- eval root branch x
  case x' of
    Integer a -> return (root, branch, Integer (-a))
    Double a -> return (root, branch, Double (-a))
    _ -> runtimeError "Type mismatch"
evalMinus root branch [x, y] = do
  (_, _, x') <- eval root branch x
  (_, _, y') <- eval root branch y
  case (x', y') of
    (Integer a, Integer b) -> return (root, branch, Integer (a - b))
    (Double a, Double b) -> return (root, branch, Double (a - b))
    _ -> runtimeError "Type mismatch"
evalMinus _ _ _ = runtimeError "Wrong number of arguments"

-- evaluation of "*"
evalMult :: LocalEnv -> Env -> [SExpr] -> Effect (LocalEnv, Env, SExpr)
evalMult root branch [x] = do
  (_, _, x') <- eval root branch x
  case x' of
    Integer a -> return (root, branch, x')
    Double a -> return (root, branch, x')
    _ -> runtimeError $ "Non numerical value " ++ show x'
evalMult root branch (x:xs) = do
  (_, _, xs') <- evalMult root branch xs
  (_, _, x') <- eval root branch x
  case (x', xs') of
    (Integer a, Integer b) -> return (root, branch, Integer (a * b))
    (Double a, Double b) -> return (root, branch, Double (a * b))
    _ -> runtimeError "Type mismatch"

-- evaluation of "/"
evalDiv :: LocalEnv -> Env -> SExpr -> SExpr -> Effect (LocalEnv, Env, SExpr)
evalDiv root branch x y = do
  (_, _, y') <- eval root branch y
  case y' of
    Integer 0 -> runtimeError "Division by 0"
    Double 0.0 -> runtimeError "Division by 0.0"
    _ -> do
      (_, _, x') <- eval root branch x
      case (x', y') of
        (Integer a,Integer b) -> return (root, branch, Integer (a `div` b))
        (Double a, Double b) -> return (root, branch, Double (a / b))
        _ -> runtimeError $ "Type mismatch"

-- evaluation of "%"
evalMod :: LocalEnv -> Env -> SExpr -> SExpr -> Effect (LocalEnv, Env, SExpr)
evalMod root branch x y = do
  (_, _, y') <- eval root branch y
  case y' of
    Integer 0 -> runtimeError "Division by 0"
    _ -> do
      (_, _, x') <- eval root branch x
      case (x', y') of
        (Integer a, Integer b) -> return (root, branch, Integer (a `mod` b))
        _ -> runtimeError $ "Type mismatch"

-- evaluation of "<"
evalLess :: LocalEnv -> Env -> SExpr -> SExpr -> Effect (LocalEnv, Env, SExpr)
evalLess root branch x y = do
  (_, _, x') <- eval root branch x
  (_, _, y') <- eval root branch y
  case (x', y') of
    (Integer a, Integer b) -> case a < b of
      True -> return (root, branch, T)
      False -> return (root, branch, F)
    (Double a, Double b) -> case a < b of
      True -> return (root, branch, T)
      False -> return (root, branch, F)
    _ -> runtimeError "Type mismatch"

-- evaluation of ">"
evalGreater :: LocalEnv -> Env -> SExpr -> SExpr -> Effect (LocalEnv, Env, SExpr)
evalGreater root branch x y = do
  (_, _, x') <- eval root branch x
  (_, _, y') <- eval root branch y
  case (x', y') of
    (Integer a, Integer b) -> case a > b of
      True -> return (root, branch, T)
      False -> return (root, branch, F)
    (Double a, Double b) -> case a > b of
      True -> return (root, branch, T)
      False -> return (root, branch, F)
    _ -> runtimeError "Type mismatch"

-- evaluation function, the core of the lisp interpreter
eval :: LocalEnv -> Env -> SExpr -> Effect (LocalEnv, Env, SExpr)
-- evaluation of atomic values (self-evaluation)
eval root branch Nil = return (root, branch, Nil)
eval root branch T = return (root, branch, T)
eval root branch F = return (root, branch, F)
eval root branch (Integer x) = return (root, branch, Integer x)
eval root branch (Double x) = return (root, branch, Double x)
eval root branch (String x) = return (root, branch, String x)
-- evaluation of symbols (lookup)
eval root branch (Symb x) = case seek (Symb x) branch of
  Just (branch', y) -> return (root, branch', y)
  Nothing -> case seek (Symb x) [root] of
    Just (branch', y) -> return (root, branch', y)
    Nothing -> runtimeError $ "Unbound symbol " ++ show x
-- evaluation of built-in functions (function application) and special forms (custom evaluation)
eval root branch (CAR x) = evalCar root branch x
eval root branch (CDR x) = evalCdr root branch x
eval root branch (CONS x y) = evalCons root branch x y
eval root branch (EQQ x y) = evalEq root branch x y
eval root branch (ATOM x) = evalAtom root branch x
eval root branch (COND x) = evalCond root branch x
eval root branch (IF x y z) = evalIf root branch x y z
eval root branch (QUOTE x) = evalQuote root branch x
eval root branch (DEFINE x y) = evalDefine root branch x y
eval root branch (LAMBDA x y) = evalLambda root branch x y
eval root branch (PLUS x) = evalPlus root branch x
eval root branch (MINUS x) = evalMinus root branch x
eval root branch (MULT x) = evalMult root branch x
eval root branch (DIV x y) = evalDiv root branch x y
eval root branch (MOD x y) = evalMod root branch x y
eval root branch (LESS x y) = evalLess root branch x y
eval root branch (GREATER x y) = evalGreater root branch x y
-- evaluation of non built-in functions
eval root branch (Pair f x) = do
  (root', branch', f') <- eval root branch f
  case f' of
    Func args body -> do
      (_, _, x') <- evalList root branch x
      (_, b, y) <- eval root' (bind args x' (Map.empty:branch')) body
      return (root, b, y)
    _ -> runtimeError $ "Unknown expression"
-- evaluation of other cases
eval _ _ _ = runtimeError $ "Unknown expression"

-- evaluate all elements in a list
evalList :: LocalEnv -> Env -> SExpr -> Effect (LocalEnv, Env, [SExpr])
evalList root branch Nil = return (root, branch, [])
evalList root branch (Pair x xs) = do
  (_, _, x') <- eval root branch x
  (_, _, xs') <- evalList root branch xs
  return (root, branch, x':xs')
