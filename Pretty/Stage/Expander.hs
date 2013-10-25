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

condMatchPatternsMismatch :: PrettyString -> PrettyString
condMatchPatternsMismatch ms =
  PrettyString.text "Cond contains different number of patterns"
  $+$
  PrettyString.nest ms
