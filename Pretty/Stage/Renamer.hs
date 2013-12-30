module Pretty.Stage.Renamer where

import Data.List (intercalate)

import Data.PrettyString (PrettyString)
import qualified Data.PrettyString as PrettyString

freeNamesFailedToRename :: [String] -> PrettyString
freeNamesFailedToRename freeNames =
  PrettyString.text "definition depends on free names that failed to rename"
  PrettyString.$+$
  PrettyString.nest (PrettyString.text (intercalate ", " freeNames))

nameAlreadyDefined :: String -> PrettyString
nameAlreadyDefined name =
  PrettyString.text "name" PrettyString.<+>
  PrettyString.text name PrettyString.<+>
  PrettyString.text "is already defined"

nameNotDefined :: String -> PrettyString
nameNotDefined name =
  PrettyString.text "name" PrettyString.<+>
  PrettyString.text name PrettyString.<+>
  PrettyString.text "is not defined"

nameNotFunction :: String -> PrettyString
nameNotFunction name =
  PrettyString.text "name" PrettyString.<+>
  PrettyString.text name PrettyString.<+>
  PrettyString.text "is not a function"

nameMultiplyDefined :: String -> [String] -> PrettyString
nameMultiplyDefined name names =
  PrettyString.text "name" PrettyString.<+>
  PrettyString.text name PrettyString.<+>
  PrettyString.text "is multiply defined"
  PrettyString.$+$
  PrettyString.nest
  (PrettyString.sep . PrettyString.intercalate (PrettyString.text ",") . map PrettyString.text $ names)
