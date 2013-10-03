module Doc.Renamer where

import Data.List (intercalate)
import Text.PrettyPrint (($+$), text)

import qualified Doc.Doc as Doc

freeNamesFailedToRename :: [String] -> String
freeNamesFailedToRename freeNames =
  Doc.renderDoc $
  text "definition depends on free names that failed to rename"
  $+$
  Doc.nest (text (intercalate ", " freeNames))
