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
readInputFragment :: String -> Int -> InputT IO (String)
readInputFragment str lines = do
  input <- getInputLine $ "[" ++ show lines ++ "]> "
           ++ (concat $ replicate open "\t")
  case input of
    Nothing -> do
      liftIO $ putStrLn $ "[INFO] User input discarded"
      readInputFragment "" 0
    Just str' -> case isBlank str' of
      True -> readInputFragment str lines
      False -> case reads (str ++ " " ++ str') :: [(AST, String)] of
          [(s, s')] -> case dropLeadingBlanks s' == "" of
            True -> liftIO $ return $ str ++ " " ++ str'
            False -> readInputFragment (str ++ " " ++ str') (lines + 1)
          _ -> readInputFragment (str ++ " " ++ str') (lines + 1)
  where
    open = length $ filter (\x -> x == '(') str

readInput :: InputT IO (String)
readInput = readInputFragment "" 0

-- read-eval-print-loop for terminal
readEvalPrintLoop :: Env -> InputT IO ()
readEvalPrintLoop env = do
  input <- readInput
  case reads input :: [(AST, String)] of
    [] -> readEvalPrintLoop env
    [(expr, rest)] -> case isBlank rest of
      False -> do
        liftIO $ putStrLn $ "[ERROR] Unable to parse expression"
        readEvalPrintLoop env
      True -> let Effect effect = evalSExpr env (compile expr) in do
        effectValue <- liftIO effect
        case effectValue of
          Left err -> do
            liftIO $ putStrLn $ "[ERROR] " ++ show err
            readEvalPrintLoop env
          Right (env', result) -> do
            liftIO $ putStrLn $ "=> " ++ show result
            readEvalPrintLoop env'

-- read-eval-print-loop for files
readEvalLoop :: (Env, String) -> IO ()
readEvalLoop (env, str) = case dropWhile (`elem` " \t\n") str of
  [] -> return ()
  _ -> case reads str :: [(AST, String)] of
    [] -> putStrLn $ "[ERROR] Syntax error: " ++ show str
    [(expr, rest)] -> let Effect effect = evalSExpr env (compile expr) in do
      effectValue <- liftIO effect
      case effectValue of
        Left err -> do
          liftIO $ putStrLn ("[ERROR] " ++ show err)
          return ()
        Right (env', result) -> do
          liftIO $ putStrLn $ show result
          case isBlank rest of
            True -> return ()
            False -> readEvalLoop (env', rest)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Minilisp interactive REPL environment [CTRL+c to quit]"
      runInputT defaultSettings (readEvalPrintLoop Map.empty)
    [path] -> do
      file <- readFile path
      readEvalLoop (Map.empty, file)
