module Main where

import qualified Data.Map as Map
import System.Environment
import System.Console.Haskeline
import Control.Monad.IO.Class

import Interpreter
import Parser
import SExpr

import Utils

-- Read input from keyboard
readInputFragment :: String -> Int -> InputT IO (SExpr)
readInputFragment str lines = do
  input <- getInputLine $ "[" ++ show lines ++ "]> "
           ++ (concat $ replicate open "\t")
  case input of
    Nothing -> do
      liftIO $ putStrLn $ "[INFO] User input discarded"
      readInputFragment "" 0
    Just str' -> case isBlank str' of
      True -> readInputFragment str lines
      False -> case parse (str ++ " " ++ str') of
        Nothing -> readInputFragment (str ++ " " ++ str') (lines + 1)
        Just expr -> liftIO $ return expr
  where
    open = numberOfOpenBrackets False str

readInput :: InputT IO (SExpr)
readInput = readInputFragment "" 0

-- read-eval-print-loop for terminal
readEvalPrintLoop :: Env -> InputT IO ()
readEvalPrintLoop env = do
  input <- readInput
  let Effect effect = evalSExpr env input in do
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
  _ -> case parse str of
    Nothing -> putStrLn $ "[ERROR] Syntax error: " ++ show str
    Just expr -> let Effect effect = evalSExpr env expr in do
      effectValue <- liftIO effect
      case effectValue of
        Left err -> do
          liftIO $ putStrLn ("[ERROR] " ++ show err)
          return ()
        Right (env', result) -> do
          liftIO $ putStrLn $ show result

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
