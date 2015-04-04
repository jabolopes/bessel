import Test.Parser
import Test.Stage.Lexer
import Test.Stage.IndentLexer
import Test.Stage.Expander
import Test.Stage.Renamer

main :: IO ()
main =
  do testLexer
     testIndentLexer
     testParser
     testExpander
     testRenamer
