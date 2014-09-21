module Pretty.Stage.Loader where

import Data.PrettyString (PrettyString, (<+>), ($+$))
import qualified Data.PrettyString as PrettyString

moduleMeMismatch :: String -> String -> PrettyString
moduleMeMismatch me filename =
  (PrettyString.text "module" <+> PrettyString.text me <+>
   PrettyString.text "and filename mismatch")
  $+$
  PrettyString.nest (PrettyString.text "module me is" <+>
                     PrettyString.text me
                     $+$
                     PrettyString.text "module filename is" <+>
                     PrettyString.text filename)

docUses :: [(String, String)] -> PrettyString
docUses [] = PrettyString.empty
docUses ((use, ""):uses) =
  PrettyString.text "use" <+> PrettyString.text use
  $+$
  docUses uses
docUses ((use, prefix):uses) =
  PrettyString.text "use" <+> PrettyString.text use <+>
  PrettyString.text "as" <+> PrettyString.text prefix
  $+$
  docUses uses

-- EDIT: use colors to emphasize the problem
moduleContainsDuplicateUses :: String -> [(String, String)] -> PrettyString
moduleContainsDuplicateUses me uses =
  (PrettyString.text "module" <+> PrettyString.text me <+>
   PrettyString.text "contains duplicate uses")
  $+$
  PrettyString.nest (docUses uses)

-- EDIT: use colors to emphasize the problem
moduleContainsDuplicateQualifiers :: String -> [(String, String)] -> PrettyString
moduleContainsDuplicateQualifiers me uses =
  (PrettyString.text "module" <+> PrettyString.text me <+>
   PrettyString.text "contains duplicate qualifiers")
  $+$
  PrettyString.nest (docUses uses)

readFileFail :: String -> String -> PrettyString
readFileFail me err =
  (PrettyString.text "module" <+> PrettyString.text me <+>
   PrettyString.text "not found")
  $+$
  PrettyString.nest (PrettyString.text err)
