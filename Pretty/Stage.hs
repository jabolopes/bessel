module Pretty.Stage where

import Data.PrettyString (PrettyString, ($+$))
import qualified Data.PrettyString as PrettyString (nest, text)

definitionContainsNoSource :: PrettyString -> PrettyString
definitionContainsNoSource err =
  PrettyString.text "Definition contains no source to expand"
  $+$
  PrettyString.nest err
