module Pretty.Renamer where

import Data.List (intercalate)

import Data.PrettyString (PrettyString)
import qualified Data.PrettyString as PrettyString

freeNamesFailedToRename :: [String] -> PrettyString
freeNamesFailedToRename freeNames =
  PrettyString.text "definition depends on free names that failed to rename"
  PrettyString.$+$
  PrettyString.nest (PrettyString.text (intercalate ", " freeNames))
