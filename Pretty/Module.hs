module Pretty.Module where

import Prelude hiding (mod)

import Data.List (intercalate)
import qualified Data.Map as Map

import Data.Module (ModuleT, Module)
import qualified Data.Module as Module
import Data.PrettyString (PrettyString, (<+>), ($+$))
import qualified Data.PrettyString as PrettyString
import qualified Pretty.Definition as Pretty

docHeader :: String -> ModuleT -> PrettyString
docHeader name typ =
  PrettyString.text name <+> PrettyString.parens (PrettyString.text (show typ))

docDeps :: Bool -> [String] -> PrettyString
docDeps showBrief deps
  | showBrief || null deps = PrettyString.empty
  | otherwise = PrettyString.text "uses" <+> PrettyString.text (intercalate ", " deps)

docDefns :: Bool -> Bool -> Bool -> Bool -> Bool -> Module -> PrettyString
docDefns showOrd showFree showSrc showExp showRen mod
  | showOrd = defnDocs (Module.defsAsc mod)
  | otherwise = defnDocs $ Map.elems $ Module.modDefs mod
  where docDefnFn = Pretty.docDefn showFree showSrc showExp showRen

        defnDocs defns
          | null defns = PrettyString.text "no definitions"
          | otherwise = PrettyString.vcat (map docDefnFn defns)

docModule :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Module -> PrettyString
docModule showBrief showOrd showFree showSrc showExp showRen mod =
  docHeader (Module.modName mod) (Module.modType mod)
  $+$
  PrettyString.nest
    (docDeps showBrief (Module.modDeps mod)
     $+$
     docDefns showOrd showFree showSrc showExp showRen mod)

docModules :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> [Module] -> PrettyString
docModules showBrief showOrd showFree showSrc showExp showRen mods =
  PrettyString.vcat (map docModuleFn mods)
  where docModuleFn = docModule showBrief showOrd showFree showSrc showExp showRen