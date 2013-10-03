module Doc.SrcFile where

import qualified Data.Map as Map
import Text.PrettyPrint

import Data.SrcFile (SrcFileT, SrcFile)
import qualified Data.SrcFile as SrcFile
import qualified Doc.Definition as Doc
import qualified Doc.Doc as Doc

docHeader :: String -> SrcFileT -> Doc
docHeader name typ =
  text name <+> parens (text (show typ))

docDeps :: Bool -> [String] -> Doc
docDeps showBrief deps
  | showBrief || null deps = empty
  | otherwise = text "uses" <+> sep (punctuate comma (map text deps))

docDefns :: Bool -> Bool -> Bool -> Bool -> Bool -> SrcFile -> Doc
docDefns showOrd showFree showSrc showExp showRen srcfile
  | showOrd = defnDocs (SrcFile.defsAsc srcfile)
  | otherwise = defnDocs $ Map.elems $ SrcFile.defs srcfile
  where docDefnFn = Doc.docDefn showFree showSrc showExp showRen
        
        defnDocs defns
          | null defns = text "no definitions"
          | otherwise = vcat (map docDefnFn defns)

docSrcFile :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> SrcFile -> Doc
docSrcFile showBrief showOrd showFree showSrc showExp showRen srcfile =
  docHeader (SrcFile.name srcfile) (SrcFile.t srcfile)
  $+$
  Doc.nest
       (docDeps showBrief (SrcFile.deps srcfile)
        $+$
        docDefns showOrd showFree showSrc showExp showRen srcfile)

docSrcFiles :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> [SrcFile] -> Doc
docSrcFiles showBrief showOrd showFree showSrc showExp showRen srcfiles =
  vcat (map docSrcFileFn srcfiles)
  where docSrcFileFn = docSrcFile showBrief showOrd showFree showSrc showExp showRen
