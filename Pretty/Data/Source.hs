module Pretty.Data.Source where

import Prelude hiding ((<>))

import Data.PrettyString (PrettyString, (<>), (<+>), ($+$))
import qualified Data.PrettyString as PrettyString
import qualified Data.Name as Name
import Data.Source (Source(..))
import qualified Pretty.Data.Literal as Pretty

isParens :: Bool -> Source -> Bool
isParens right AppS {}     = right
isParens _     IdS {}      = False
isParens _     LetS {}     = False
isParens _     LiteralS {} = False
isParens _     PatS {}     = False
isParens _     SeqS {}     = False
isParens _     TupleS {}   = False
isParens _ _               = True

docParens :: Bool -> (Source -> PrettyString) -> Source -> PrettyString
docParens right fn src
  | isParens right src = PrettyString.parens $ fn src
  | otherwise = fn src

docSimplePattern :: Source -> PrettyString
docSimplePattern (AndS src1 src2) =
  docSimplePattern $ BinOpS "&&" src1 src2
docSimplePattern (AppS src1 src2) =
  docPattern src1 <+> docPattern src2
docSimplePattern (BinOpS op src1 src2) =
  docPattern src1 <+>
  PrettyString.text op <+>
  docPattern src2
docSimplePattern src@IdS {} =
  docSource src
docSimplePattern src@LiteralS {} =
  docSource src
docSimplePattern (OrS src1 src2) =
  docSimplePattern $ BinOpS "||" src1 src2
docSimplePattern (SeqS srcs) =
  PrettyString.text "[" <>
  PrettyString.sep (PrettyString.intercalate (PrettyString.text ",") (map docPattern srcs)) <>
  PrettyString.text "]"
docSimplePattern (TupleS []) =
  PrettyString.text "()"
docSimplePattern (TupleS [src]) =
  docPattern src
docSimplePattern (TupleS srcs) =
  PrettyString.text "(" <>
  PrettyString.sep (PrettyString.intercalate (PrettyString.text ",") (map docPattern srcs)) <>
  PrettyString.text ")"
docSimplePattern src =
  error . PrettyString.toString $
    PrettyString.text "docSimplePattern: incomplete pattern matching"
    $+$
    docSource src

docPattern :: Source -> PrettyString
docPattern (PatS binder (Just src@IdS {})) =
  PrettyString.text (show binder) <> PrettyString.text "@" <> docSource src
docPattern (PatS binder (Just guard)) =
  PrettyString.text (show binder) <> PrettyString.text "@" <> docParens True docSimplePattern guard
docPattern (PatS binder Nothing)
  | Name.isEmptyName binder = PrettyString.text "@"
  | otherwise = PrettyString.text (show binder)
docPattern src@(IdS name)
  | not $ Name.isTypeName name =
    PrettyString.text "@" <> docSimplePattern src
docPattern src =
  docParens True docSimplePattern src

docMatch :: ([Source], Source) -> PrettyString
docMatch (args, body) =
  PrettyString.sep [PrettyString.sep (map docPattern args),
                    PrettyString.equals,
                    PrettyString.nest . docSource $ body]

docCond :: [([Source], Source)] -> PrettyString
docCond ms = PrettyString.vcat (map docMatch ms)

docSource :: Source -> PrettyString
docSource (AndS src1 src2) =
  docSource src1 <+> PrettyString.text "&&" <+> docSource src2
docSource (AppS src1 src2) =
  PrettyString.sep [docParens False docSource src1,
                    PrettyString.nest (docParens True docSource src2)]
docSource (BinOpS op src1 src2) =
  docSource src1 <+> PrettyString.text op <+> docSource src2
docSource (CondS srcs) =
  docCond srcs
docSource (FnDefS pat typ body whereClause) =
  docFnDef whereClause
  where
    docBody CondS {} = docSource body
    docBody _ = PrettyString.equals <+> docSource body

    -- TODO: use proper (docType typ) instead of (show typ).
    docType =
      case typ of
        Nothing -> PrettyString.empty
        Just t -> PrettyString.text ":" <+> PrettyString.text (show t)

    docWhereClause =
      PrettyString.vcat $
        [PrettyString.text "where",
         PrettyString.nest . PrettyString.vcat $ map docSource whereClause]

    docFnDef [] =
      PrettyString.sep [PrettyString.text "let" <+> docSource pat, docType, PrettyString.nest (docBody body)]
    docFnDef _ =
      docFnDef []
      $+$
      docWhereClause
docSource (IdS name) =
  PrettyString.text $ show name
docSource (LetS defns body) =
  PrettyString.sep
  [PrettyString.vcat (map docSource defns),
   PrettyString.text "in",
   docSource body]
docSource (LiteralS literal) =
  Pretty.docLiteral literal
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
docSource src@PatS {} =
  docPattern src
docSource (SeqS ms) =
  PrettyString.char '[' <>
  PrettyString.sep (PrettyString.intercalate (PrettyString.text ",") (map docSource ms)) <>
  PrettyString.char ']'
docSource (TupleS [src]) =
  docSource src
docSource (TupleS srcs) =
  PrettyString.char '(' <>
  PrettyString.sep (PrettyString.intercalate (PrettyString.text ",") (map docSource srcs)) <>
  PrettyString.char ')'
docSource (TypeDeclS typeName tags) =
    PrettyString.text "type" <+> PrettyString.text (show typeName) <+> PrettyString.equals
    $+$
    PrettyString.nest docTags
  where
    docMaybePattern Nothing = PrettyString.empty
    docMaybePattern (Just pat) = docPattern pat

    docTag True (tagName, pat) =
      PrettyString.text (show tagName) <+> docMaybePattern pat
    docTag False (tagName, pat) =
      PrettyString.text "|" <+> PrettyString.text (show tagName) <+> docMaybePattern pat

    docTags =
      PrettyString.vcat $ (docTag True (head tags)):map (docTag False) (tail tags)

-- PrettyString for a list of 'Source's.
docSourceList :: [Source] -> PrettyString
docSourceList [] = PrettyString.text "[]"
docSourceList (src:srcs) =
  PrettyString.sep .
  (++ [PrettyString.text "]"]) .
  PrettyString.intercalate (PrettyString.text ",") $
  ((PrettyString.text "[" <+> docSource src):) $
  map (\x -> PrettyString.text "," <+> docSource x) srcs
