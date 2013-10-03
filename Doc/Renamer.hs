module Doc.Renamer where

import Data.List (intercalate)
import Text.PrettyPrint (($+$), text)

import Doc.Doc (Doc)
import qualified Doc.Doc as Doc

freeNamesFailedToRename :: [String] -> Doc
freeNamesFailedToRename freeNames =
  text "definition depends on free names that failed to rename"
  $+$
  Doc.nest (text (intercalate ", " freeNames))
