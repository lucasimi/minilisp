module Utils where

-- drop all blank leading characters from a string
dropLeadingBlanks :: String -> String
dropLeadingBlanks = dropWhile (`elem` " \t\n")

-- splitOnDelimiter a string where a delimiter is found
splitOnDelimiter :: String -> (String, String)
splitOnDelimiter = (break (`elem` " \t\n()."))

split :: String -> (String, String)
split = splitOnDelimiter . dropLeadingBlanks

-- error data type
data Error = RuntimeErr String deriving Show

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

{--
-- evaluation monad
data Eval m st a = Eval { runEval :: st -> m (st, a) }

instance Monad m => Functor (Eval m st) where
  fmap f x = Eval $ \s -> do
    (s', res) <- runEval x s
    return (s', f res)

instance Monad m => Applicative (Eval m st) where
  pure x = Eval $ \s -> return (s, x)
  f <*> x = Eval $ \s -> do
    (s', x') <- runEval x s
    (s'', f') <- runEval f s'
    return (s'', f' x')

instance Monad m => Monad (Eval m st) where
  x >>= f = Eval $ \s -> do
    (s', x') <- runEval x s
    (s'', y) <- runEval (f x') s'
    return (s'', y)
--}
