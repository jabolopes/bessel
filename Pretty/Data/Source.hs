module Pretty.Data.Source where

import Prelude hiding ((<>))

import Data.PrettyString (PrettyString, (<>), (<+>), ($+$))
import qualified Data.PrettyString as PrettyString
import Data.Name (Name)
import qualified Data.Name as Name
import Data.Source

docPat :: Name -> Maybe Source -> PrettyString
docPat binder guard =
  docBinder <> docAt guard <> docGuard guard
  where
    docBinder
      | Name.isEmptyName binder = PrettyString.empty
      | otherwise = PrettyString.text $ show binder

    docAt Just {} = PrettyString.text "@"
    docAt Nothing | Name.isEmptyName binder = PrettyString.text "@"
    docAt _ = PrettyString.empty

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
docSource (FnDefS pat Nothing body whereClause) =
  docFnDef whereClause
  where
    docBody CondS {} = docSource body
    docBody _ = PrettyString.equals <+> docSource body

    docWhereClause =
      PrettyString.vcat $
        [PrettyString.text "where",
         PrettyString.nest . PrettyString.vcat $ map docSource whereClause]

    docFnDef [] =
      PrettyString.sep [PrettyString.text "let" <+> docSource pat, PrettyString.nest (docBody body)]
    docFnDef _ =
      docFnDef []
      $+$
      docWhereClause

docSource (FnDefS pat (Just typ) body whereClause) =
  -- TODO: fix indentation
  -- TODO: use docType instead of show typ
  PrettyString.sep [PrettyString.text "let" <+>
                    docSource pat <+>
                    PrettyString.text " : " <+>
                    PrettyString.text (show typ),
                    PrettyString.nest (docSource body),
                    PrettyString.sep [PrettyString.text "where",
                                      PrettyString.nest (PrettyString.vcat (map docSource whereClause))]]
docSource (IdS name) =
  PrettyString.text $ show name
docSource (IntS i) =
  PrettyString.int i
docSource (LetS defns body) =
  PrettyString.sep
  [PrettyString.vcat (map docSource defns),
   PrettyString.text "in",
   docSource body]
docSource (ModuleS name uses defns) =
  PrettyString.text "me" <+> PrettyString.text (show name)
  $+$
  PrettyString.empty
  $+$
  PrettyString.vcat (map docUse uses)
  $+$
  PrettyString.vcat
    (PrettyString.intercalate (PrettyString.text "\n") (map docSource defns))
  where
    docUse (use, asName)
      | Name.isEmptyName asName =
        PrettyString.text "use" <+> PrettyString.text (show use)
    docUse (use, qual) =
      docUse (use, Name.empty) <+> PrettyString.text "as" <+> PrettyString.text (show qual)
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
  PrettyString.string str
docSource (TupleS [src]) =
  docSource src
docSource (TupleS srcs) =
  PrettyString.char '(' <>
  PrettyString.sep (PrettyString.intercalate (PrettyString.text ",") (map docSource srcs)) <>
  PrettyString.char ')'
docSource (TypeDeclS typeName cons) =
  PrettyString.sep [PrettyString.text "type" <+> PrettyString.text (show typeName), PrettyString.nest consDoc]
  where
    docConstructor (consName, consPat) =
      PrettyString.text (show consName) <+> docSource consPat
    consDoc =
      PrettyString.sep . PrettyString.intercalate (PrettyString.text "|") . map docConstructor $ cons

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
