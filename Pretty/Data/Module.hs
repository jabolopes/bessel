module Pretty.Data.Module where

import Prelude hiding (mod)

import Data.List (intercalate)
import qualified Data.Map as Map

import Data.Module (ModuleT, Module)
import qualified Data.Module as Module
import Data.Name (Name)
import qualified Data.Name as Name
import Data.PrettyString (PrettyString, (<+>), ($+$))
import qualified Data.PrettyString as PrettyString
import qualified Pretty.Data.Definition as Pretty

docHeader :: Name -> ModuleT -> PrettyString
docHeader name typ =
  PrettyString.text (show name) <+> PrettyString.parens (PrettyString.text (show typ))

docUses :: [(Name, Name)] -> PrettyString
docUses [] = PrettyString.empty
docUses ((name1, asName):uses)
  | Name.isEmptyName asName =
    PrettyString.text "use" <+>
    PrettyString.text (show name1) $+$
    docUses uses
docUses ((name1, name2):uses) =
  PrettyString.text "use" <+>
  PrettyString.text (show name1) <+>
  PrettyString.text "as" <+>
  PrettyString.text (show name2) $+$
  docUses uses

docDeps :: [Name] -> PrettyString
docDeps deps
  | null deps = PrettyString.empty
  | otherwise = PrettyString.text "uses" <+> PrettyString.text (intercalate ", " $ map show deps)

docDefns :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Module -> PrettyString
docDefns showOrd showFree showSrc showExp showRen showJs mod
  | showOrd = defnDocs (Module.defsAsc mod)
  | otherwise = defnDocs $ Map.elems $ Module.modDefs mod
  where
    docDefnFn =
      Pretty.docDefn showFree showSrc showExp showRen showJs

    defnDocs defns
      | null defns = PrettyString.text "no definitions"
      | otherwise = PrettyString.vcat (map docDefnFn defns)

docModule :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Module -> PrettyString
docModule showBrief showOrd showFree showSrc showExp showRen showJs mod =
  docHeader (Module.modName mod) (Module.modType mod)
  $+$
  PrettyString.nest
    (if showBrief then
       docDeps (Module.dependencies mod)
     else
       docUses (Module.prefixedUses mod)
       $+$
       docDeps (Module.unprefixedUses mod))
  $+$
    docDefns showOrd showFree showSrc showExp showRen showJs mod

docModules :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> [Module] -> PrettyString
docModules showBrief showOrd showFree showSrc showExp showRen showJs mods =
  PrettyString.vcat (map docModuleFn mods)
  where docModuleFn = docModule showBrief showOrd showFree showSrc showExp showRen showJs
