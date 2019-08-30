import TokenizerTest

main :: IO ()
main = do
  testReadTokens
  testReadAST
  testCompileAST
