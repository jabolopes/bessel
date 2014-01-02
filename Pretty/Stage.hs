module Pretty.Stage where

import Prelude hiding (mod)

import Data.PrettyString (PrettyString, ($+$), (<+>))
import qualified Data.PrettyString as PrettyString

definitionContainsNoSource :: PrettyString -> PrettyString
definitionContainsNoSource err =
  PrettyString.text "Definition contains no source to expand"
  $+$
  PrettyString.nest err

duplicateDefinitions :: String -> String -> PrettyString
duplicateDefinitions mod name =
  PrettyString.text "Module" <+>
  PrettyString.quotes (PrettyString.text mod) <+>
  PrettyString.text "contains duplicate definitions"
  $+$
  PrettyString.nest (PrettyString.text name)
