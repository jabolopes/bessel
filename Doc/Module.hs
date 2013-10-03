module Doc.Module where

import Prelude hiding (mod)

import qualified Data.Map as Map
import Text.PrettyPrint

import Data.Module (ModuleT, Module)
import qualified Data.Module as Module
import qualified Doc.Definition as Doc
import qualified Doc.Doc as Doc

docHeader :: String -> ModuleT -> Doc
docHeader name typ =
  text name <+> parens (text (show typ))

docDeps :: Bool -> [String] -> Doc
docDeps showBrief deps
  | showBrief || null deps = empty
  | otherwise = text "uses" <+> sep (punctuate comma (map text deps))

docDefns :: Bool -> Bool -> Bool -> Bool -> Bool -> Module -> Doc
docDefns showOrd showFree showSrc showExp showRen mod
  | showOrd = defnDocs (Module.defsAsc mod)
  | otherwise = defnDocs $ Map.elems $ Module.modDefs mod
  where docDefnFn = Doc.docDefn showFree showSrc showExp showRen

        defnDocs defns
          | null defns = text "no definitions"
          | otherwise = vcat (map docDefnFn defns)

docModule :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Module -> Doc
docModule showBrief showOrd showFree showSrc showExp showRen mod =
  docHeader (Module.modName mod) (Module.modType mod)
  $+$
  Doc.nest
       (docDeps showBrief (Module.modDeps mod)
        $+$
        docDefns showOrd showFree showSrc showExp showRen mod)

docModules :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> [Module] -> Doc
docModules showBrief showOrd showFree showSrc showExp showRen mods =
  vcat (map docModuleFn mods)
  where docModuleFn = docModule showBrief showOrd showFree showSrc showExp showRen