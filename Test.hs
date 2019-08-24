import System.Environment (getArgs)

import Test.Parser
import Test.Stage.Expander
import Test.Stage.IndentLexer
import Test.Stage.Lexer
import Test.Stage.Linearizer
import Test.Stage.Renamer
import Test.Stage.Typechecker

main :: IO ()
main =
  do generateTestExpectations <- ("--generate_test_expectations" `elem`) <$> getArgs
     testLexer
     testIndentLexer
     testParser generateTestExpectations
     testExpander generateTestExpectations
     testRenamer generateTestExpectations
     testTypechecker generateTestExpectations
     testLinearizer generateTestExpectations
