module Pretty.Stage.Expander where

import Data.PrettyString (PrettyString, (<+>), ($+$))
import qualified Data.PrettyString as PrettyString

devModuleNested :: String -> PrettyString
devModuleNested me =
  (PrettyString.text "module" <+>
   PrettyString.text me <+>
   PrettyString.text "occurs nested")
  $+$
  PrettyString.nest (PrettyString.text "This should be disallowed by the parser")

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
