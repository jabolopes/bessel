module Pretty.Stage.Renamer where

import Data.PrettyString (PrettyString, (<+>))
import qualified Data.PrettyString as PrettyString

freeNamesFailedToRename :: String -> [(String, String)] -> PrettyString
freeNamesFailedToRename name names =
  PrettyString.text "definition" <+>
  PrettyString.text name <+>
  PrettyString.text "depends on names that failed to rename"
  PrettyString.$+$
  PrettyString.nest
  (PrettyString.vcat (map docName names))
  where
    docName (modName, defName) =
      PrettyString.text defName <+> PrettyString.text "in module" <+> PrettyString.text modName

nameAlreadyDefined :: String -> PrettyString
nameAlreadyDefined name =
  PrettyString.text "name" <+>
  PrettyString.text name <+>
  PrettyString.text "is already defined"

nameNotDefined :: String -> PrettyString
nameNotDefined name =
  PrettyString.text "name" <+>
  PrettyString.text name <+>
  PrettyString.text "is not defined"

nameNotFunction :: String -> PrettyString
nameNotFunction name =
  PrettyString.text "name" <+>
  PrettyString.text name <+>
  PrettyString.text "is not a function"

nameMultiplyDefined :: String -> [String] -> PrettyString
nameMultiplyDefined name names =
  PrettyString.text "name" <+>
  PrettyString.text name <+>
  PrettyString.text "is multiply defined"
  PrettyString.$+$
  PrettyString.nest
  (PrettyString.vcat (map PrettyString.text names))
