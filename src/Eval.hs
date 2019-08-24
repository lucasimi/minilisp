module Eval where

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
