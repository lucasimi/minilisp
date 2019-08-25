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

-- evaluation function, the core of the lisp interpreter
eval :: (Env, SExpr) -> Effect (Env, SExpr)
-- self evaluation of Prims
eval (env, Nil) = return (env, Nil)
eval (env, Prim x) = return (env, Prim x)
-- lookup evaluation of Symbs
eval (env, Symb x) = case Map.lookup x env of
  Just x' -> return (env, x')
  Nothing -> runtimeError $ "Unbound symbol " ++ x
-- evaluate conditional form
eval (env, Pair (Symb "cond") (Pair (Pair cond (Pair action Nil)) x)) = do
  (_, cond') <- eval (env, cond)
  case cond' of
    Prim (Boolean True) -> eval (env, action)
    Prim (Boolean False) -> eval (env, Pair (Symb "cond") x)
    _ -> runtimeError "Non boolean value as conditional"
-- evaluate quotation form
eval (env, Pair (Symb "quote") (Pair x Nil)) = do
  return (env, x)
-- evaluation of lambda form
eval (env, Pair (Symb "lambda") (Pair x (Pair y Nil))) = do
  return (env, Func (\(e, a) -> eval (bind x a e, y)))
-- evaluation of label form

-- evaluation of define form
eval (env, Pair (Symb "define") (Pair (Symb x) (Pair y Nil))) = do
  (_, y') <- eval (env, y)
  return (Map.insert x y' env, Symb x)
-- evaluation of car function
eval (env, Pair (Symb "car") (Pair x Nil)) = do
  (_, x') <- eval (env, x)
  case x' of
    Pair a _ -> return (env, a)
    _ -> runtimeError "Non pair value"
-- evaluation of cdr function
eval (env, Pair (Symb "cdr") (Pair x Nil)) = do
  (_, x') <- eval (env, x)
  case x' of
    Pair _ b -> return (env, b)
    _ -> runtimeError "Non pair value"
-- evaluation of Prim function
eval (env, Pair (Symb "atom") (Pair x Nil)) = do
  (_, x') <- eval (env, x)
  case x' of
    Nil -> return (env, Prim (Boolean True))
    Prim _ -> return (env, Prim (Boolean True))
    _ -> return (env, Prim (Boolean False))
-- evaluation of eq function
eval (env, Pair (Symb "eq") (Pair x (Pair y Nil))) = do
  (_, x') <- eval (env, x)
  (_, y') <- eval (env, y)
  case (x', y') of
    (Prim x'', Prim y'') -> case x'' == y'' of
      True -> return (env, Prim (Boolean True))
      False -> return (env, Prim (Boolean False))
    _ -> return (env, Prim (Boolean False))
-- evaluation of cons function
eval (env, Pair (Symb "cons") (Pair x (Pair y Nil))) = do
  (_, x') <- eval (env, x)
  (_, y') <- eval (env, y)
  return (env, Pair x' y')
-- evaluation of +
eval (env, Pair (Symb "+") (Pair x Nil)) = do
  (_, x') <- eval (env, x)
  case x' of
    Prim (Integer a) -> return (env, x')
    Prim (Decimal a) -> return (env, x')
    _ -> runtimeError $ "Non numerical value " ++ show x'
eval (env, Pair (Symb "+") (Pair x y)) = do
  (_, y') <- eval (env, Pair (Symb "+") y)
  (_, x') <- eval (env, x)
  case (x', y') of
    (Prim (Integer a), Prim (Integer b)) -> return (env, Prim (Integer (a + b)))
    (Prim (Decimal a), Prim (Decimal b)) -> return (env, Prim (Decimal (a + b)))
    _ -> runtimeError "Numeric type mismatch"
-- evaluation of -
eval (env, Pair (Symb "-") (Pair x Nil)) = do
  (_, x') <- eval (env, x)
  case x' of
    Prim (Integer a) -> return (env, Prim $ Integer (-a))
    Prim (Decimal a) -> return (env, Prim $ Decimal (-a))
    _ -> runtimeError $ "Non numerical value " ++ show x'
eval (env, Pair (Symb "-") (Pair x (Pair y Nil))) = do
  (_, y') <- eval (env, y)
  (_, x') <- eval (env, x)
  case (x', y') of
    (Prim (Integer a), Prim (Integer b)) -> return (env, Prim (Integer (a - b)))
    (Prim (Decimal a), Prim (Decimal b)) -> return (env, Prim (Decimal (a - b)))
    _ -> runtimeError "Numeric type mismatch"
-- evaluation of *
eval (env, Pair (Symb "*") (Pair x Nil)) = do
  (_, x') <- eval (env, x)
  case x' of
    Prim (Integer a) -> return (env, x')
    Prim (Decimal a) -> return (env, x')
    _ -> runtimeError $ "Non numerical value " ++ show x'
eval (env, Pair (Symb "*") (Pair x y)) = do
  (_, y') <- eval (env, Pair (Symb "*") y)
  (_, x') <- eval (env, x)
  case (x', y') of
    (Prim (Integer a), Prim (Integer b)) -> return (env, Prim (Integer (a * b)))
    (Prim (Decimal a), Prim (Decimal b)) -> return (env, Prim (Decimal (a * b)))
    _ -> runtimeError "Type mismatch"
-- evaluation of /
eval (env, Pair (Symb "/") (Pair x (Pair y Nil))) = do
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
eval (env, Pair (Symb "%") (Pair x (Pair y Nil))) = do
  (_, y') <- eval (env, y)
  case y' of
    Prim (Integer 0) -> runtimeError "Division by 0"
    _ -> do
      (_, x') <- eval (env, x)
      case (x', y') of
        (Prim (Integer a), Prim (Integer b)) -> return (env, Prim (Integer (a `mod` b)))
        _ -> runtimeError $ "Type mismatch"
-- evaluation of <
eval (env, Pair (Symb "<") (Pair x (Pair y Nil))) = do
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
-- evaluation of >
eval (env, Pair (Symb ">") (Pair x (Pair y Nil))) = do
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
-- evaluation of unknown cases
eval (env, (Pair f x)) = do
  (_, f') <- eval (env, f)
  case f' of
    Func g -> do
      (_, x') <- evlis (env, x)
      (_, g') <- g (env, x')
      return (env, g')
    _ -> runtimeError $ "Unknown expression " ++ show f'

eval (_, x) = runtimeError $ "Unknown function " ++ show x

evlis :: (Env, SExpr) -> Effect (Env, SExpr)
evlis (env, Nil) = return (env, Nil)
evlis (env, Pair x xs) = do
  (_, x') <- eval (env, x)
  (_, xs') <- evlis (env, xs)
  return (env, Pair x' xs')
