import Test.Parser
import Test.Stage.Lexer
import Test.Stage.IndentLexer
import Test.Stage.Expander
import Test.Stage.Renamer
import Test.Typechecker.Typechecker

generateTestExpectations :: Bool
generateTestExpectations = False

main :: IO ()
main =
  do testLexer
     testIndentLexer
     testParser generateTestExpectations
     testExpander
     testRenamer
     testTypechecker
