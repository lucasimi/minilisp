import ParserTest
import InterpreterTest

main :: IO ()
main = do
  testReadTokens
  testReadTokenTrees
  testCompile
  testEvalAtoms
  testEvalBuiltIn
  testEvalMacros
  testEvalLogic
  testEvalMath
