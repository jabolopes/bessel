module Pretty.Stage.Expander where

import Data.PrettyString (PrettyString, (<+>), ($+$))
import qualified Data.PrettyString as PrettyString

condMatchPatternsMismatch :: PrettyString -> PrettyString
condMatchPatternsMismatch ms =
  PrettyString.text "Conditional contains different number of patterns"
  $+$
  PrettyString.nest ms

devModuleNested :: String -> PrettyString
devModuleNested me =
  (PrettyString.text "Module" <+>
   PrettyString.text me <+>
   PrettyString.text "occurs nested")
  $+$
  PrettyString.nest (PrettyString.text "This should be disallowed by the parser")

devPattern :: PrettyString -> PrettyString
devPattern pat =
  (PrettyString.text "Pattern" <+>
   pat <+>
   PrettyString.text "occurs in source to be expanded")
  $+$
  PrettyString.nest (PrettyString.text "This should be disallowed by the parser")

devTypePat :: PrettyString -> PrettyString
devTypePat src =
  PrettyString.text "Type pattern must contain identifiers or other patterns"
  $+$
  PrettyString.nest src

devSourceApp :: PrettyString -> PrettyString
devSourceApp src =
  PrettyString.text "Application pattern must be an expression"
  $+$
  PrettyString.nest src

devUnhandled :: String -> PrettyString -> PrettyString
devUnhandled fnName src =
  (PrettyString.text "Unhandled case in" <+> PrettyString.text fnName)
  $+$
  src
