module Pretty.Stage where

import Prelude hiding (mod)

import Data.Name (Name)
import Data.PrettyString (PrettyString, ($+$), (<+>))
import qualified Data.PrettyString as PrettyString

definitionContainsNoSource :: PrettyString -> PrettyString
definitionContainsNoSource err =
  PrettyString.text "Definition contains no source to expand"
  $+$
  PrettyString.nest err

definitionIsNotFunction :: PrettyString -> PrettyString
definitionIsNotFunction expr =
  PrettyString.text "Expanded expression must be a function definition"
  $+$
  PrettyString.nest expr

duplicateDefinitions :: Name -> String -> PrettyString
duplicateDefinitions mod name =
  PrettyString.text "Module" <+>
  PrettyString.quotes (PrettyString.text (show mod)) <+>
  PrettyString.text "contains duplicate definitions"
  $+$
  PrettyString.nest (PrettyString.text name)
