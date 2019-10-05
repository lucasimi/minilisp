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
readInputFragment :: String -> Int -> InputT IO (Maybe TokenTree)
readInputFragment str lines = do
  input <- getInputLine $ "[" ++ show lines ++ "]> "
           ++ (concat $ replicate open "\t")
  case input of
    Nothing -> do
      liftIO $ putStrLn $ "[INFO] User input discarded"
      readInputFragment "" 0
    Just str' -> case isBlank str' of
      True -> readInputFragment str lines
      False -> case numberOfOpenBrackets False (str ++ " " ++ str') >= 0 of
        True -> case reads (str ++ " " ++ str') :: [(TokenTree, String)] of
          [] -> readInputFragment (str ++ " " ++ str') (lines + 1)
          [(tree, str'')] -> case isBlank str'' of
            True -> liftIO $ return $ Just tree
            False -> liftIO $ return $ Nothing
        False -> liftIO $ return $ Nothing
  where
    open = numberOfOpenBrackets False str

readInput :: InputT IO (Maybe TokenTree)
readInput = readInputFragment "" 0

-- read-eval-print-loop for terminal
readEvalPrintLoop :: Env -> InputT IO ()
readEvalPrintLoop env = do
  input <- readInput
  case input of
    Nothing -> do
      liftIO $ putStrLn $ "[ERROR] Tokenizer error"
      readEvalPrintLoop env
    Just tree -> case compile tree of
      Nothing -> liftIO $ putStrLn $ "[ERROR] Parser error: " ++ show tree
      Just expr -> let Effect effect = evalSExpr env expr in do
          effectValue <- liftIO effect
          case effectValue of
            Left err -> do
              liftIO $ putStrLn $ "[ERROR] Eval error: " ++ show err
              readEvalPrintLoop env
            Right (env', result) -> do
              liftIO $ putStrLn $ "=> " ++ show result
              readEvalPrintLoop env'

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Minilisp interactive REPL environment [CTRL+c to quit]"
      runInputT defaultSettings (readEvalPrintLoop Map.empty)
