module Pretty.Data.Source where

import Data.PrettyString (PrettyString, (<>), (<+>), ($+$))
import qualified Data.PrettyString as PrettyString
import qualified Data.QualName as QualName (fromQualName)
import Data.Source

docPat :: String -> Maybe Source -> PrettyString
docPat binder guard =
  docBinder binder <> PrettyString.text "@" <> docGuard guard
  where docBinder "" = PrettyString.empty
        docBinder name = PrettyString.text name

        docGuard Nothing = PrettyString.empty
        docGuard (Just src) = docSource src

docMatch :: ([Source], Source) -> PrettyString
docMatch (args, body) =
  PrettyString.sep [PrettyString.sep (map docSourceParens args),
                    PrettyString.equals,
                    PrettyString.nest . docSource $ body]

docCond :: [([Source], Source)] -> PrettyString
docCond ms = PrettyString.vcat (map docMatch ms)

docSource :: Source -> PrettyString
docSource (AndS src1 src2) =
  docSource src1 <+> PrettyString.text "&&" <+> docSource src2
docSource (AppS src1 src2) =
  PrettyString.sep [docSourceParens src1,
                    PrettyString.nest (docSourceParens src2)]
docSource (BinOpS op src1 src2) =
  docSource src1 <+> PrettyString.text op <+> docSource src2
docSource (CharS c) =
  PrettyString.char '\'' <> PrettyString.char c <> PrettyString.char '\''
docSource (CondS srcs) =
  docCond srcs
docSource (FnDeclS name body) =
  PrettyString.sep [PrettyString.text "def" <+> PrettyString.text name, PrettyString.nest (docSource body)]
docSource (IdS name) =
  PrettyString.text (QualName.fromQualName name)
docSource (IntS i) =
  PrettyString.int i
docSource (LetS defns body) =
  PrettyString.sep
  [PrettyString.text "let",
   PrettyString.vcat (map docSource defns),
   PrettyString.text "in",
   docSource body]
docSource (ModuleS name uses defns) =
  PrettyString.text "me" <+> PrettyString.text name
  $+$
  PrettyString.empty
  $+$
  PrettyString.vcat (map docUse uses)
  $+$
  PrettyString.vcat
    (PrettyString.intercalate (PrettyString.text "\n") (map docSource defns))
  where docUse (use, "") =
          PrettyString.text "use" <+> PrettyString.text use
        docUse (use, qual) =
          docUse (use, "") <+> PrettyString.text "as" <+> PrettyString.text qual
docSource (OrS src1 src2) =
  docSource src1 <+> PrettyString.text "||" <+> docSource src2
docSource (PatS binder guard) =
  docPat binder guard
docSource (RealS r) =
  PrettyString.double r
docSource (SeqS ms) =
  PrettyString.char '[' <>
  PrettyString.sep (PrettyString.intercalate (PrettyString.text ",") (map docSource ms)) <>
  PrettyString.char ']'
docSource (StringS str) =
  PrettyString.char '"' <> PrettyString.text str <> PrettyString.char '"'
docSource (TypeDeclS typeName cons) =
  PrettyString.sep [PrettyString.text "type" <+> PrettyString.text (QualName.fromQualName typeName), PrettyString.nest consDoc]
  where docConstructor (consName, consPat) =
          PrettyString.text (QualName.fromQualName consName) <+> docSource consPat
        consDoc = PrettyString.sep . PrettyString.intercalate (PrettyString.text "|") . map docConstructor $ cons
docSource (WhereS m ms) =
  PrettyString.sep
  [docSource m,
   PrettyString.sep [PrettyString.text "where",
                     PrettyString.nest (PrettyString.vcat (map docSource ms))]]

isParens :: Bool -> Source -> Bool
isParens right AppS {}    = right
isParens _     CharS {}   = False
isParens _     IdS {}     = False
isParens _     IntS {}    = False
isParens _     LetS {}    = False
isParens _     PatS {}    = False
isParens _     RealS {}   = False
isParens _     SeqS {}    = False
isParens _     StringS {} = False
isParens _ _              = True

docSourceParens :: Source -> PrettyString
docSourceParens src
  | isParens True src = PrettyString.parens . docSource $ src
  | otherwise = docSource src

-- PrettyString for a list of 'Source's.
docSourceList :: [Source] -> PrettyString
docSourceList [] = PrettyString.text "[]"
docSourceList (src:srcs) =
  PrettyString.sep .
  (++ [PrettyString.text "]"]) .
  PrettyString.intercalate (PrettyString.text ",") $
  ((PrettyString.text "[" <+> docSource src):) $
  map (\x -> PrettyString.text "," <+> docSource x) srcs
