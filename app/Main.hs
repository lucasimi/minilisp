module Main where

import qualified Data.Map as Map
import System.Environment
import System.Console.Haskeline
import Control.Monad.IO.Class

import Utils
import Tokenizer
import Parser
import Interpreter

-- Read input from keyboard
getInput :: String -> Int -> InputT IO (String)
getInput str lines = do
  input <- getInputLine $ "[" ++ show lines ++ "]> "
           ++ (concat $ replicate open "\t")
  case input of
    Nothing -> getInput "" 0
    Just str' -> case isBlank str' of
      True -> getInput str lines
      False -> case reads (str ++ " " ++ str') :: [(AST, String)] of
        [(s, s')] -> case dropLeadingBlanks s' == "" of
          True -> liftIO $ return $ str ++ " " ++ str'
          False -> getInput (str ++ " " ++ str') (lines + 1)
  where
    open = length $ filter (\x -> x == '(') str

-- read-eval-print-loop for terminal
repl :: Env -> InputT IO ()
repl env = do
  input <- getInput "" 0
  case input of
    str -> case reads str :: [(AST, String)] of
      [(s, s')] -> case dropLeadingBlanks s' == "" of
        True -> let Effect x = eval env (compile s) in do
          x' <- liftIO x
          case x' of
            Left e -> do
              liftIO $ putStrLn $ "[ERROR] " ++ show e
              repl env
            Right (env', expr) -> do
              liftIO $ putStrLn $ "=> " ++ show expr
              repl env'
      _ -> repl env

-- read-eval-print-loop for files
repl' :: (Env, String) -> IO ()
repl' (env, s) = case dropWhile (`elem` " \t\n") s of
  [] -> return ()
  _ -> case reads s :: [(AST, String)] of
    [] -> putStrLn $ "syntax error: " ++ show s
    [(e, s')] -> case dropLeadingBlanks s' == "" of
      True -> let Effect x = eval env (compile e) in do
        x' <- liftIO x
        case x' of
          Left e -> do
            liftIO $ putStrLn ("[ERROR] " ++ show e)
            return ()
          Right (env', res) -> do
            liftIO $ putStrLn $ show res
            case s' of
              "" -> return ()
              _ -> repl' (env', s')
      False -> putStrLn $ "syntax error"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Minilisp interactive REPL environment [CTRL+c to quit]"
      runInputT defaultSettings (repl [Map.empty])
    [path] -> do
      s <- readFile path
      repl' ([Map.empty], s)
