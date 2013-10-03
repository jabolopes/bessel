module Doc.Doc (Doc, empty, nest, renderDoc, text) where

import Text.PrettyPrint hiding (nest)
import qualified Text.PrettyPrint as PrettyPrint

nesting :: Int
nesting = 2

nest :: Doc -> Doc
nest = PrettyPrint.nest nesting

renderDoc :: Doc -> String
renderDoc = renderStyle style
