import ParserTest
import InterpreterTest

main :: IO ()
main = do
  testReadTokens
  testReadTokenTrees
  testEvalAtoms
  testEvalBuiltIn
  testEvalMacros
  testEvalLogic
  testEvalMath
