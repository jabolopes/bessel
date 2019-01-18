module Pretty.Stage.Typechecker where

import Data.PrettyString (PrettyString, (<+>), ($+$))
import qualified Data.PrettyString as PrettyString

devIncompleteCheck :: PrettyString -> String -> PrettyString
devIncompleteCheck expr typ =
  PrettyString.text "Failed to check term because of incomplete pattern matching"
  $+$
  PrettyString.nest
  (PrettyString.text "In expr " $+$
   PrettyString.nest expr $+$
   PrettyString.text "checking against type " $+$
   PrettyString.nest (PrettyString.text typ))

devIncompleteSynthesize :: PrettyString -> PrettyString
devIncompleteSynthesize expr =
  PrettyString.text "Failed to synthesize term because of incomplete pattern matching"
  $+$
  PrettyString.nest
    (PrettyString.text "In expr " $+$
     PrettyString.nest expr)

devIncompleteSynthesizeApply :: PrettyString -> String -> PrettyString
devIncompleteSynthesizeApply expr typ =
  PrettyString.text "Failed to synthesize apply because of incomplete pattern matching"
  $+$
  PrettyString.nest
  (PrettyString.text "In expr " $+$
   PrettyString.nest expr $+$
   PrettyString.text "synthesize apply with type " $+$
   PrettyString.nest (PrettyString.text typ))

typeMismatch :: String -> String -> String -> PrettyString
typeMismatch mod actual expected =
  PrettyString.text ("Type mismatch in " ++ mod)
  $+$
  PrettyString.nest
  (PrettyString.text "Expected " $+$
   PrettyString.nest (PrettyString.text expected) $+$
   PrettyString.text "actual " $+$
   PrettyString.nest (PrettyString.text actual))
