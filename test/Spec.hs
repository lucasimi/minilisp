import ParserTest
import InterpreterTest

main :: IO ()
main = do
  testReadTokens
  testReadTokenTrees
  --testCompileAST
  testEvalSExpr
