module Pretty.Stage.Typechecker where

import Data.PrettyString (PrettyString, ($+$))
import qualified Data.PrettyString as PrettyString

annotateIsSubtypeInstantiate :: PrettyString -> String -> String -> String -> PrettyString
annotateIsSubtypeInstantiate err context type1 type2 =
  err
  $+$
  PrettyString.nest
  (PrettyString.text "In context " $+$
   PrettyString.nest (PrettyString.text context) $+$
   PrettyString.text "with type " $+$
   PrettyString.nest (PrettyString.text type1) $+$
   PrettyString.text "and type " $+$
   PrettyString.nest (PrettyString.text type2))

annotateIsSubtype :: PrettyString -> String -> String -> String -> PrettyString
annotateIsSubtype err context type1 type2 =
  err
  $+$
  PrettyString.nest
  (PrettyString.text "In context " $+$
   PrettyString.nest (PrettyString.text context) $+$
   PrettyString.text "with type " $+$
   PrettyString.nest (PrettyString.text type1) $+$
   PrettyString.text "and type " $+$
   PrettyString.nest (PrettyString.text type2))

annotateCheck :: PrettyString -> String -> PrettyString -> String -> PrettyString
annotateCheck err context expr typ =
  err
  $+$
  PrettyString.nest
  (PrettyString.text "In context " $+$
   PrettyString.nest (PrettyString.text context) $+$
   PrettyString.text "with expr " $+$
   PrettyString.nest expr $+$
   PrettyString.text "and type " $+$
   PrettyString.nest (PrettyString.text typ))

annotateSynthesize :: PrettyString -> String -> PrettyString -> PrettyString
annotateSynthesize err context expr =
  err
  $+$
  PrettyString.nest
  (PrettyString.text "In context " $+$
   PrettyString.nest (PrettyString.text context) $+$
   PrettyString.text "with expr " $+$
   PrettyString.nest expr)

annotateSynthesizeApply :: PrettyString -> String -> String -> PrettyString -> PrettyString
annotateSynthesizeApply err context typ expr =
  err
  $+$
  PrettyString.nest
  (PrettyString.text "In context " $+$
   PrettyString.nest (PrettyString.text context) $+$
   PrettyString.text "with type " $+$
   PrettyString.nest (PrettyString.text typ) $+$
   PrettyString.text "and expr " $+$
   PrettyString.nest expr)

devIncompleteIsSubtypeInstantiate :: String -> String -> String -> PrettyString
devIncompleteIsSubtypeInstantiate =
  annotateIsSubtypeInstantiate $
    PrettyString.text "Failed to apply rule isSubtypeInstantiate because of incomplete pattern matching"

devIncompleteIsSubtype :: String -> String -> String -> PrettyString
devIncompleteIsSubtype =
  annotateIsSubtype $
    PrettyString.text "Failed to apply rule isSubtype because of incomplete pattern matching"

devIncompleteSynthesizeApply :: String -> String -> PrettyString -> PrettyString
devIncompleteSynthesizeApply =
  annotateSynthesizeApply $
    PrettyString.text "Failed to apply rule synthesizeApply because of incomplete pattern matching"

devIncompleteSynthesize :: String -> PrettyString -> PrettyString
devIncompleteSynthesize =
  annotateSynthesize $
    PrettyString.text "Failed to apply rule synthesize because of incomplete pattern matching"

devContextIsSubtypeInstantiate :: String -> String -> String -> PrettyString
devContextIsSubtypeInstantiate =
  annotateIsSubtypeInstantiate $
    PrettyString.text "Failed to apply rule isSubtypeInstantiate because context is ill-formed"

devContextIsSubtype :: String -> String -> String -> PrettyString
devContextIsSubtype =
  annotateIsSubtype $
    PrettyString.text "Failed to apply rule isSubtype because context is ill-formed"

devContextCheck :: String -> PrettyString -> String -> PrettyString
devContextCheck =
  annotateCheck $
    PrettyString.text "Failed to apply rule check because context is ill-formed"

devContextSynthesizeApply :: String -> String -> PrettyString -> PrettyString
devContextSynthesizeApply =
  annotateSynthesizeApply $
    PrettyString.text "Failed to apply rule synthesizeApply because context is ill-formed"

devContextSynthesize :: String -> PrettyString -> PrettyString
devContextSynthesize =
  annotateSynthesize $
    PrettyString.text "Failed to apply rule synthesize because context is ill-formed"

devContextTypecheck :: String -> PrettyString -> Maybe String -> PrettyString
devContextTypecheck context expr Nothing =
  PrettyString.text "Failed to typecheck term because context is ill-formed"
  $+$
  PrettyString.nest
  (PrettyString.text "In context " $+$
   PrettyString.nest (PrettyString.text context) $+$
   PrettyString.text "with expr " $+$
   PrettyString.nest expr)
devContextTypecheck context expr (Just checkType) =
  PrettyString.text "Failed to typecheck term because context is ill-formed"
  $+$
  PrettyString.nest
  (PrettyString.text "In context " $+$
   PrettyString.nest (PrettyString.text context) $+$
   PrettyString.text "with expr " $+$
   PrettyString.nest expr $+$
   PrettyString.text "and type " $+$
   PrettyString.nest (PrettyString.text checkType))

typeMismatch :: String -> String -> String -> PrettyString
typeMismatch moduleName actual expected =
  PrettyString.text ("Type mismatch in " ++ moduleName)
  $+$
  PrettyString.nest
  (PrettyString.text "Expected " $+$
   PrettyString.nest (PrettyString.text expected) $+$
   PrettyString.text "actual " $+$
   PrettyString.nest (PrettyString.text actual))
