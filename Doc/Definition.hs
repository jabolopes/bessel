module Doc.Definition where

import Data.List (intercalate)
import Text.PrettyPrint ((<+>), ($+$), equals, empty, isEmpty, text)

import Doc.Doc (Doc)
import qualified Doc.Doc as Doc

definitionOk :: String -> String -> [String] -> Doc -> Doc -> Doc -> String
definitionOk name val freeNames src exp ren
  | null freeNames =
    Doc.renderDoc $ hdDoc $+$ srcDoc $+$ expDoc $+$ renDoc
  | otherwise =
    Doc.renderDoc $ hdDoc $+$ freeDoc $+$ srcDoc $+$ expDoc $+$ renDoc
  where hdDoc = text name <+> equals <+> text val

        freeDoc =
          Doc.nest $ text "-- free" $+$ text (intercalate ", " freeNames)

        exprDoc desc expr
          | isEmpty expr = empty
          | otherwise = Doc.nest $ text desc $+$ expr $+$ text ""

        srcDoc = exprDoc "-- source" src
        expDoc = exprDoc "-- expanded" exp
        renDoc = exprDoc "-- renamed" ren

definitionError :: String -> String -> String
definitionError name err =
  Doc.renderDoc $ text name $+$ Doc.nest (text err)
