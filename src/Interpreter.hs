module Interpreter where

import qualified Data.Map as Map

import Utils
import Parser

printEnv :: Env -> Effect ()
printEnv env = Effect $ do
  putStrLn $ show env
  return $ Right ()

-- bind symbols to values
bindEnv :: [SExpr] -> [SExpr] -> Env -> Env
bindEnv [] [] env = env
bindEnv ((Symb x):xs) (val:vals) env = Map.insert x val (bindEnv xs vals env)
bindEnv _ _ env = env

bindCtx :: [SExpr] -> [SExpr] -> Ctx -> Ctx
bindCtx x y (env:envs) = (bindEnv x y env):envs

seekEnv :: SExpr -> Env -> Maybe SExpr
seekEnv (Symb x) env = Map.lookup x env

seekCtx :: SExpr -> Ctx -> Maybe (Ctx, SExpr)
seekCtx (Symb x) [] = Nothing
seekCtx (Symb x) (env:envs) = case seekEnv (Symb x) env of
  Just y -> Just (env:envs, y)
  Nothing -> seekCtx (Symb x) envs

-- returns a runtime error with specified message
runtimeError :: String -> Effect (Env, Ctx, SExpr)
runtimeError x = Effect $ return $ Left $ RuntimeErr x

-- evaluation of "car"
evalCar :: Env -> Ctx -> SExpr -> Effect (Env, Ctx, SExpr)
evalCar env ctx x = do
  (_, _, x') <- eval env ctx x
  case x' of
    Pair a _ -> return (env, ctx, a)

-- evaluation of "cdr"
evalCdr :: Env -> Ctx -> SExpr -> Effect (Env, Ctx, SExpr)
evalCdr env ctx x = do
  (_, _, x') <- eval env ctx x
  case x' of
    Pair _ b -> return (env, ctx, b)

-- evaluation of "cons"
evalCons :: Env -> Ctx -> SExpr -> SExpr -> Effect (Env, Ctx, SExpr)
evalCons env ctx x y = do
  (_, _, x') <- eval env ctx x
  (_, _, y') <- eval env ctx y
  return (env, ctx, Pair x' y')

-- evaluation of "eq"
evalEq :: Env -> Ctx -> SExpr -> SExpr -> Effect (Env, Ctx, SExpr)
evalEq env ctx x y = do
  (_, _, x') <- eval env ctx x
  (_, _, y') <- eval env ctx y
  case (x', y') of
    (Integer a, Integer b) -> case a == b of
      True -> return (env, ctx, T)
      False -> return (env, ctx, F)
    (Double a, Double b) -> case a == b of
      True -> return (env, ctx, T)
      False -> return (env, ctx, F)
    (String a, String b) -> case a == b of
      True -> return (env, ctx, T)
      False -> return (env, ctx, F)
    _ -> return (env, ctx, F)

-- evaluation of "atom"
evalAtom :: Env -> Ctx -> SExpr -> Effect (Env, Ctx, SExpr)
evalAtom env ctx x = do
  (_, _, x') <- eval env ctx x
  case x' of
    Nil -> return (env, ctx, T)
    Integer _ -> return (env, ctx, T)
    Double _ -> return (env, ctx, T)
    String _ -> return (env, ctx, T)
    _ -> return (env, ctx, F)

-- evaluation of "cond" special form
evalCond :: Env -> Ctx -> [(SExpr, SExpr)] -> Effect (Env, Ctx, SExpr)
evalCond env ctx ((x, y):xs) = do
  (_, _, x') <- eval env ctx x
  case x' of
    T -> eval env ctx y
    F -> evalCond env ctx xs
    _ -> runtimeError "Non boolean value as conditional"

evalIf :: Env -> Ctx -> SExpr -> SExpr -> SExpr -> Effect (Env, Ctx, SExpr)
evalIf env ctx x y z = do
  (_, _, x') <- eval env ctx x
  case x' of
    T -> eval env ctx y
    F -> eval env ctx z
    _ -> runtimeError "Non boolean value as conditional"

-- evaluation of "quote" special form
evalQuote :: Env -> Ctx -> SExpr -> Effect (Env, Ctx, SExpr)
evalQuote env ctx x = do
  return (env, ctx, x)

-- evaluation of "define" special form
evalDefine :: Env -> Ctx -> SExpr -> SExpr -> Effect (Env, Ctx, SExpr)
evalDefine env ctx (Symb x) y = do
  (_, _, y') <- eval env ctx y
  return (bindEnv [Symb x] [y'] env, ctx, Symb x)

-- evaluation of "+"
evalPlus :: Env -> Ctx -> [SExpr] -> Effect (Env, Ctx, SExpr)
evalPlus env ctx [x] = do
  (_, _, x') <- eval env ctx x
  case x' of
    Integer a -> return (env, ctx, x')
    Double a -> return (env, ctx, x')
    _ -> runtimeError $ "Non numerical value " ++ show x'
evalPlus env ctx (x:xs) = do
  (_, _, xs') <- evalPlus env ctx xs
  (_, _, x') <- eval env ctx x
  case (x', xs') of
    (Integer a, Integer b) -> return (env, ctx, Integer (a + b))
    (Double a, Double b) -> return (env, ctx, Double (a + b))
    _ -> runtimeError "Numeric type mismatch"

-- evaluation of "-"
evalMinus :: Env -> Ctx -> [SExpr] -> Effect (Env, Ctx, SExpr)
evalMinus env ctx [x] = do
  (_, _, x') <- eval env ctx x
  case x' of
    Integer a -> return (env, ctx, Integer (-a))
    Double a -> return (env, ctx, Double (-a))
    _ -> runtimeError "Type mismatch"
evalMinus env ctx [x, y] = do
  (_, _, x') <- eval env ctx x
  (_, _, y') <- eval env ctx y
  case (x', y') of
    (Integer a, Integer b) -> return (env, ctx, Integer (a - b))
    (Double a, Double b) -> return (env, ctx, Double (a - b))
    _ -> runtimeError "Type mismatch"
evalMinus _ _ _ = runtimeError "Wrong number of arguments"

-- evaluation of "*"
evalMult :: Env -> Ctx -> [SExpr] -> Effect (Env, Ctx, SExpr)
evalMult env ctx [x] = do
  (_, _, x') <- eval env ctx x
  case x' of
    Integer a -> return (env, ctx, x')
    Double a -> return (env, ctx, x')
    _ -> runtimeError $ "Non numerical value " ++ show x'
evalMult env ctx (x:xs) = do
  (_, _, xs') <- evalMult env ctx xs
  (_, _, x') <- eval env ctx x
  case (x', xs') of
    (Integer a, Integer b) -> return (env, ctx, Integer (a * b))
    (Double a, Double b) -> return (env, ctx, Double (a * b))
    _ -> runtimeError "Type mismatch"

-- evaluation of "/"
evalDiv :: Env -> Ctx -> SExpr -> SExpr -> Effect (Env, Ctx, SExpr)
evalDiv env ctx x y = do
  (_, _, y') <- eval env ctx y
  case y' of
    Integer 0 -> runtimeError "Division by 0"
    Double 0.0 -> runtimeError "Division by 0.0"
    _ -> do
      (_, _, x') <- eval env ctx x
      case (x', y') of
        (Integer a,Integer b) -> return (env, ctx, Integer (a `div` b))
        (Double a, Double b) -> return (env, ctx, Double (a / b))
        _ -> runtimeError $ "Type mismatch"

-- evaluation of "%"
evalMod :: Env -> Ctx -> SExpr -> SExpr -> Effect (Env, Ctx, SExpr)
evalMod env ctx x y = do
  (_, _, y') <- eval env ctx y
  case y' of
    Integer 0 -> runtimeError "Division by 0"
    _ -> do
      (_, _, x') <- eval env ctx x
      case (x', y') of
        (Integer a, Integer b) -> return (env, ctx, Integer (a `mod` b))
        _ -> runtimeError $ "Type mismatch"

-- evaluation of "<"
evalLess :: Env -> Ctx -> SExpr -> SExpr -> Effect (Env, Ctx, SExpr)
evalLess env ctx x y = do
  (_, _, x') <- eval env ctx x
  (_, _, y') <- eval env ctx y
  case (x', y') of
    (Integer a, Integer b) -> case a < b of
      True -> return (env, ctx, T)
      False -> return (env, ctx, F)
    (Double a, Double b) -> case a < b of
      True -> return (env, ctx, T)
      False -> return (env, ctx, F)
    _ -> runtimeError "Type mismatch"

-- evaluation of ">"
evalGreater :: Env -> Ctx -> SExpr -> SExpr -> Effect (Env, Ctx, SExpr)
evalGreater env ctx x y = do
  (_, _, x') <- eval env ctx x
  (_, _, y') <- eval env ctx y
  case (x', y') of
    (Integer a, Integer b) -> case a > b of
      True -> return (env, ctx, T)
      False -> return (env, ctx, F)
    (Double a, Double b) -> case a > b of
      True -> return (env, ctx, T)
      False -> return (env, ctx, F)
    _ -> runtimeError "Type mismatch"

-- evaluation function, the core of the lisp interpreter
eval :: Env -> Ctx -> SExpr -> Effect (Env, Ctx, SExpr)
-- evaluation of atomic values (self-evaluation)
eval env ctx Nil = return (env, ctx, Nil)
eval env ctx T = return (env, ctx, T)
eval env ctx F = return (env, ctx, F)
eval env ctx (Integer x) = return (env, ctx, Integer x)
eval env ctx (Double x) = return (env, ctx, Double x)
eval env ctx (String x) = return (env, ctx, String x)
eval env ctx (Lambda x y) = return (env, ctx, Lambda x y)
-- evaluation of symbols (lookup)
eval env ctx (Symb x) = case seekCtx (Symb x) ctx of
  Just (ctx', y) -> return (env, ctx', y)
  Nothing -> case seekEnv (Symb x) env of
    Just y -> return (env, [], y)
    Nothing -> runtimeError $ "Unbound symbol " ++ show x
-- evaluation of built-in functions (function application) and special forms (custom evaluation)
eval env ctx (CAR x) = evalCar env ctx x
eval env ctx (CDR x) = evalCdr env ctx x
eval env ctx (CONS x y) = evalCons env ctx x y
eval env ctx (EQQ x y) = evalEq env ctx x y
eval env ctx (ATOM x) = evalAtom env ctx x
eval env ctx (COND x) = evalCond env ctx x
eval env ctx (IF x y z) = evalIf env ctx x y z
eval env ctx (QUOTE x) = evalQuote env ctx x
eval env ctx (DEFINE x y) = evalDefine env ctx x y
eval env ctx (PLUS x) = evalPlus env ctx x
eval env ctx (MINUS x) = evalMinus env ctx x
eval env ctx (MULT x) = evalMult env ctx x
eval env ctx (DIV x y) = evalDiv env ctx x y
eval env ctx (MOD x y) = evalMod env ctx x y
eval env ctx (LESS x y) = evalLess env ctx x y
eval env ctx (GREATER x y) = evalGreater env ctx x y
-- evaluation of non built-in functions
eval env ctx (Pair f x) = do
  (env', ctx', f') <- eval env ctx f
  case f' of
    Lambda args body -> do
      (_, _, x') <- evalList env ctx x
      (_, ctx'', y) <- eval env' (bindCtx args x' (Map.empty:ctx')) body
      return (env, ctx'', y)
    _ -> runtimeError $ "Unknown expression"
-- evaluation of other cases
eval _ _ _ = runtimeError $ "Unknown expression"

-- evaluate all elements in a list
evalList :: Env -> Ctx -> SExpr -> Effect (Env, Ctx, [SExpr])
evalList env ctx Nil = return (env, ctx, [])
evalList env ctx (Pair x xs) = do
  (_, _, x') <- eval env ctx x
  (_, _, xs') <- evalList env ctx xs
  return (env, ctx, x':xs')
