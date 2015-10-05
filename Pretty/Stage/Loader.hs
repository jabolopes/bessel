module Pretty.Stage.Loader where

import Data.Name (Name)
import qualified Data.Name as Name
import Data.PrettyString (PrettyString, (<+>), ($+$))
import qualified Data.PrettyString as PrettyString

moduleMeMismatch :: Name -> Name -> PrettyString
moduleMeMismatch me modName =
  (PrettyString.text "module" <+> PrettyString.text (show me) <+>
   PrettyString.text "and module name/filename mismatch")
  $+$
  PrettyString.nest (PrettyString.text "module me is" <+>
                     PrettyString.text (show me)
                     $+$
                     PrettyString.text "module name/filename is" <+>
                     PrettyString.text (show modName))

docUses :: [(Name, Name)] -> PrettyString
docUses [] = PrettyString.empty
docUses ((use, asName):uses)
  | Name.isEmptyName asName =
    PrettyString.text "use" <+> PrettyString.text (show use)
    $+$
    docUses uses
docUses ((use, prefix):uses) =
  PrettyString.text "use" <+> PrettyString.text (show use) <+>
  PrettyString.text "as" <+> PrettyString.text (show prefix)
  $+$
  docUses uses

-- EDIT: use colors to emphasize the problem
moduleContainsDuplicateUses :: Name -> [(Name, Name)] -> PrettyString
moduleContainsDuplicateUses me uses =
  (PrettyString.text "module" <+> PrettyString.text (show me) <+>
   PrettyString.text "contains duplicate uses")
  $+$
  PrettyString.nest (docUses uses)

-- EDIT: use colors to emphasize the problem
moduleContainsDuplicateQualifiers :: Name -> [(Name, Name)] -> PrettyString
moduleContainsDuplicateQualifiers me uses =
  (PrettyString.text "module" <+> PrettyString.text (show me) <+>
   PrettyString.text "contains duplicate qualifiers")
  $+$
  PrettyString.nest (docUses uses)

moduleCycle :: [Name] -> PrettyString
moduleCycle mes =
  PrettyString.text "module import cycle between" <+>
  (PrettyString.sep .
   PrettyString.intercalate (PrettyString.text ",") $
   map (PrettyString.text . show) mes)

moduleNotFound :: Name -> PrettyString
moduleNotFound me =
  PrettyString.text "module" <+> PrettyString.text (show me) <+>
  PrettyString.text "not found"

readFileFail :: Name -> String -> PrettyString
readFileFail me err =
  (PrettyString.text "module" <+> PrettyString.text (show me) <+>
   PrettyString.text "not found")
  $+$
  PrettyString.nest (PrettyString.text err)
