module Pretty.Stage.Expander where

import Data.PrettyString (PrettyString, (<+>), ($+$))
import qualified Data.PrettyString as PrettyString

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

condMatchPatternsMismatch :: PrettyString -> PrettyString
condMatchPatternsMismatch ms =
  PrettyString.text "Conditional contains different number of patterns"
  $+$
  PrettyString.nest ms
