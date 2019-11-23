import CompilerTest
import EvalTest

main :: IO ()
main = do
  testReadTokens
  testReadParseTrees
  testCompile
  testEvalAtoms
  testEvalBuiltIn
  testEvalMacros
  testEvalLogic
  testEvalMath
