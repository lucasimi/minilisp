module Minilisp where

import Eval
import qualified Data.Map as Map

-- drop all blank leading characters from a string
dropLeadingBlanks :: String -> String
dropLeadingBlanks = dropWhile (`elem` " \t\n")

-- splitOnDelimiter a string where a delimiter is found
splitOnDelimiter :: String -> (String, String)
splitOnDelimiter = (break (`elem` " \t\n()."))

-- all admitted TypeTypes for Primic type
data Type = Integer Integer
          | Decimal Double
          | Boolean Bool
          | String String

instance Show Type where
  show (Integer x) = show x
  show (Decimal x) = show x
  show (Boolean True) = "#t"
  show (Boolean False) = "#f"
  show (String x) = show x

instance Eq Type where
  Integer x == Integer y = x == y
  Decimal x == Decimal y = x == y
  Boolean x == Boolean y = x == y
  String x == String y = x == y
  _ == _ = False

instance Read Type where
  readsPrec _ str =
    case reads str :: [(Integer, String)] of
      [(x, s)] -> [(Integer x, s)]
      _ -> case reads str :: [(Double, String)] of
        [(x, s)] -> [(Decimal x, s)]
        _ -> case reads str :: [(String, String)] of
          [(x, s)] -> [(String x, s)]
          _ -> case splitOnDelimiter (dropLeadingBlanks str) of
            ("#t", s) -> [(Boolean True, s)]
            ("#f", s) -> [(Boolean False, s)]
            _ -> []

-- S-Expression data type
data SExpr = Nil
           | Prim Type
           | Symb String
           | Pair SExpr SExpr
           | Func ((Env, SExpr) -> Effect (Env, SExpr))
           | Spec ((Env, SExpr) -> Effect (Env, SExpr))

instance Show SExpr where
  show Nil = "()"
  show (Prim x) = show x
  show (Symb x) = x
  show (Pair x Nil) = "(" ++ show x ++ ")"
  show (Pair x y) = if isList (Pair x y)
                    then "(" ++ show x ++ " " ++ (tail $ show y)
                    else "(" ++ show x ++ " . " ++ show y ++ ")"
    where isList Nil = True
          isList (Pair x y) = isList y
          isList _ = False
  show (Func _) = "<function>"
  show (Spec _) = "<special>"

instance Eq SExpr where
  Nil == Nil = True
  Prim x == Prim y = x == y
  Symb x == Symb y = x == y
  Pair a b == Pair x y = (a == x) && (b == y)
  _ == _ = False

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

--
evalCar :: (Env, SExpr) -> Effect (Env, SExpr)
evalCar (env, Pair (Pair x _) Nil) = return (env, x)

evalCdr :: (Env, SExpr) -> Effect (Env, SExpr)
evalCdr (env, Pair (Pair _ y) Nil) = return (env, y)

evalCons :: (Env, SExpr) -> Effect (Env, SExpr)
evalCons (env, Pair x (Pair y Nil)) = return (env, Pair x y)

evalEq :: (Env, SExpr) -> Effect(Env, SExpr)
evalEq (env, Pair (Prim x) (Pair (Prim y) Nil)) = case x == y of
  True -> return (env, Prim $ Boolean True)
  False -> return (env, Prim $ Boolean False)

evalAtom :: (Env, SExpr) -> Effect (Env, SExpr)
evalAtom (env, Nil) = return (env, Prim $ Boolean True)
evalAtom (env, Prim _) = return (env, Prim $ Boolean True)
evalAtom (env, _) = return (env, Prim $ Boolean False)

-- evaluate conditional form
evalCond :: (Env, SExpr) -> Effect (Env, SExpr)
evalCond (env, Pair (Pair cond (Pair action Nil)) x) = do
  (_, cond') <- eval (env, cond)
  case cond' of
    Prim (Boolean True) -> eval (env, action)
    Prim (Boolean False) -> eval (env, Pair (Symb "cond") x)
    _ -> runtimeError "Non boolean value as conditional"

evalIf :: (Env, SExpr) -> Effect (Env, SExpr)
evalIf (env, Pair cond (Pair action (Pair alt Nil))) = do
  (_, cond') <- eval (env, cond)
  case cond' of
    Prim (Boolean True) -> eval (env, action)
    Prim (Boolean False) -> eval (env, alt)
    _ -> runtimeError "Non boolean value as conditional"
-- evaluate quotation form
evalQuote :: (Env, SExpr) -> Effect (Env, SExpr)
evalQuote (env, Pair x Nil) = do
  return (env, x)

-- evaluation of lambda form
evalLambda :: (Env, SExpr) -> Effect (Env, SExpr)
evalLambda (env, Pair x (Pair y Nil)) = do
  return (env, Func (\(e, a) -> eval (bind x a e, y)))
-- evaluation of label form

-- evaluation of define form
evalDefine :: (Env, SExpr) -> Effect (Env, SExpr)
evalDefine (env, Pair (Symb x) (Pair y Nil)) = do
  (_, y') <- eval (env, y)
  return (Map.insert x y' env, Symb x)

-- evaluation of +
evalPlus :: (Env, SExpr) -> Effect (Env, SExpr)
evalPlus (env, Pair x Nil) = do
  (_, x') <- eval (env, x)
  case x' of
    Prim (Integer a) -> return (env, x')
    Prim (Decimal a) -> return (env, x')
    _ -> runtimeError $ "Non numerical value " ++ show x'
evalPlus (env, Pair x y) = do
  (_, y') <- eval (env, Pair (Symb "+") y)
  (_, x') <- eval (env, x)
  case (x', y') of
    (Prim (Integer a), Prim (Integer b)) -> return (env, Prim (Integer (a + b)))
    (Prim (Decimal a), Prim (Decimal b)) -> return (env, Prim (Decimal (a + b)))
    _ -> runtimeError "Numeric type mismatch"
evalPlus _ = runtimeError "Arguments mismatch"

evalMinus :: (Env, SExpr) -> Effect (Env, SExpr)
evalMinus (env, Pair x Nil) = do
  (_, x') <- eval (env, x)
  case x' of
    Prim (Integer a) -> return (env, Prim $ Integer (-a))
    Prim (Decimal a) -> return (env, Prim $ Decimal (-a))
    _ -> runtimeError "Type mismatch"
evalMinus (env, Pair x (Pair y Nil)) = do
  (_, x') <- eval (env, x)
  case x' of
    Prim (Integer a) -> do
      (_, y') <- eval (env, y)
      case y' of
        Prim (Integer b) -> return (env, Prim $ Integer $ a - b)
        _ -> runtimeError "Type mismatch"
    Prim (Decimal a) -> do
      (_, y') <- eval (env, y)
      case y' of
        Prim (Decimal b) -> return (env, Prim $ Decimal $ a - b)
        _ -> runtimeError "Type mismatch"
    _ -> runtimeError "Type mismatch"
evalMinus _ = runtimeError "Arguments mismatch"

-- evaluation of *
evalMult :: (Env, SExpr) -> Effect (Env, SExpr)
evalMult (env, Pair x Nil) = do
  (_, x') <- eval (env, x)
  case x' of
    Prim (Integer a) -> return (env, x')
    Prim (Decimal a) -> return (env, x')
    _ -> runtimeError $ "Non numerical value " ++ show x'
evalMult (env, Pair x y) = do
  (_, y') <- eval (env, Pair (Symb "*") y)
  (_, x') <- eval (env, x)
  case (x', y') of
    (Prim (Integer a), Prim (Integer b)) -> return (env, Prim (Integer (a * b)))
    (Prim (Decimal a), Prim (Decimal b)) -> return (env, Prim (Decimal (a * b)))
    _ -> runtimeError "Type mismatch"

-- evaluation of /
evalDiv :: (Env, SExpr) -> Effect (Env, SExpr)
evalDiv (env, Pair x (Pair y Nil)) = do
  (_, y') <- eval (env, y)
  case y' of
    Prim (Integer 0) -> runtimeError "Division by 0"
    Prim (Decimal 0.0) -> runtimeError "Division by 0.0"
    _ -> do
      (_, x') <- eval (env, x)
      case (x', y') of
        (Prim (Integer a), Prim (Integer b)) -> return (env, Prim (Integer (a `div` b)))
        (Prim (Decimal a), Prim (Decimal b)) -> return (env, Prim (Decimal (a / b)))
        _ -> runtimeError $ "Type mismatch"

-- evaluation of %
evalMod :: (Env, SExpr) -> Effect (Env, SExpr)
evalMod (env, Pair x (Pair y Nil)) = do
  (_, y') <- eval (env, y)
  case y' of
    Prim (Integer 0) -> runtimeError "Division by 0"
    _ -> do
      (_, x') <- eval (env, x)
      case (x', y') of
        (Prim (Integer a), Prim (Integer b)) -> return (env, Prim (Integer (a `mod` b)))
        _ -> runtimeError $ "Type mismatch"

evalLess :: (Env, SExpr) -> Effect (Env, SExpr)
evalLess (env, Pair x (Pair y Nil)) = do
  (_, x') <- eval (env, x)
  case x' of
    Prim (Integer a) -> do
      (_, y') <- eval (env, y)
      case y' of
        Prim (Integer b) -> case a < b of
          True -> return (env, Prim (Boolean True))
          False -> return (env, Prim (Boolean False))
        _ -> runtimeError "Type mismatch"
    Prim (Decimal a) -> do
      (_, y') <- eval (env, y)
      case y' of
        Prim (Decimal b) -> case a < b of
          True -> return (env, Prim (Boolean True))
          False -> return (env, Prim (Boolean False))
        _ -> runtimeError "Type mismatch"
    _ -> runtimeError "Type mismatch"

evalGreater :: (Env, SExpr) -> Effect (Env, SExpr)
evalGreater (env, Pair x (Pair y Nil)) = do
  (_, x') <- eval (env, x)
  case x' of
    Prim (Integer a) -> do
      (_, y') <- eval (env, y)
      case y' of
        Prim (Integer b) -> case a > b of
          True -> return (env, Prim (Boolean True))
          False -> return (env, Prim (Boolean False))
        _ -> runtimeError "Type mismatch"
    Prim (Decimal a) -> do
      (_, y') <- eval (env, y)
      case y' of
        Prim (Decimal b) -> case a > b of
          True -> return (env, Prim (Boolean True))
          False -> return (env, Prim (Boolean False))
        _ -> runtimeError "Type mismatch"
    _ -> runtimeError "Type mismatch"

-- evaluation function, the core of the lisp interpreter
eval :: (Env, SExpr) -> Effect (Env, SExpr)
-- self evaluation
eval (env, Nil) = return (env, Nil)
eval (env, Prim x) = return (env, Prim x)
-- evaluation of symbols which refer to built-in functions
eval (env, Symb "car") = return (env, Func evalCar)
eval (env, Symb "cdr") = return (env, Func evalCdr)
eval (env, Symb "cons") = return (env, Func evalCons)
eval (env, Symb "eq") = return (env, Func evalEq)
eval (env, Symb "atom") = return (env, Spec evalAtom)
eval (env, Symb "cond") = return (env, Spec evalCond)
eval (env, Symb "if") = return (env, Spec evalIf)
eval (env, Symb "quote") = return (env, Spec evalQuote)
eval (env, Symb "define") = return (env, Spec evalDefine)
eval (env, Symb "lambda") = return (env, Spec evalLambda)
eval (env, Symb "+") = return (env, Spec evalPlus)
eval (env, Symb "-") = return (env, Spec evalMinus)
eval (env, Symb "*") = return (env, Spec evalMult)
eval (env, Symb "/") = return (env, Spec evalDiv)
eval (env, Symb "%") = return (env, Spec evalMod)
eval (env, Symb "<") = return (env, Spec evalLess)
eval (env, Symb ">") = return (env, Spec evalGreater)
-- evaluation of custom symbols by lookup inside environment
eval (env, Symb x) = case Map.lookup x env of
  Just x' -> return (env, x')
  Nothing -> runtimeError $ "Unbound symbol " ++ x
-- evaluation of pairs
eval (env, (Pair f x)) = do
  (_, f') <- eval (env, f)
  case f' of
    Func g -> do
      (_, x') <- evlis (env, x)
      (_, g') <- g (env, x')
      return (env, g')
    Spec g -> g (env, x)
    _ -> runtimeError $ "Unknown expression " ++ show f'
-- other cases return an error
eval (_, x) = runtimeError $ "Unknown function " ++ show x

evlis :: (Env, SExpr) -> Effect (Env, SExpr)
evlis (env, Nil) = return (env, Nil)
evlis (env, Pair x xs) = do
  (_, x') <- eval (env, x)
  (_, xs') <- evlis (env, xs)
  return (env, Pair x' xs')
