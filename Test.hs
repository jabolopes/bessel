import Test.Parser
import Test.Stage.Lexer
import Test.Stage.Expander
import Test.Stage.Renamer

main :: IO ()
main =
  do testLexer
     testParser
     testExpander
     testRenamer
