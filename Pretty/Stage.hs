module Pretty.Stage where

import Data.PrettyString (PrettyString, ($+$))
import qualified Data.PrettyString as PrettyString (nest, text)

definitionContainsNoMacro :: PrettyString -> PrettyString
definitionContainsNoMacro err =
  PrettyString.text "Definition contains no macro to expand"
  $+$
  PrettyString.nest err
