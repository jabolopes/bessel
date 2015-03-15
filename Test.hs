import Test.Parser
import Test.Stage.Expander
-- import Test.Stage.Renamer

main :: IO ()
main =
  do testParser
     testExpander
     -- testRenamer
