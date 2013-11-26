module Pretty.Data.Macro where

import Data.Macro
import Data.PrettyString (PrettyString, (<>), (<+>), ($+$))
import qualified Data.PrettyString as PrettyString
import qualified Data.TypeName as TypeName (fromTypeName)

docPat :: Pat -> PrettyString
docPat pat =
  docBinder (patBinder pat) <> PrettyString.text "@" <> docGuard (patGuard pat)
  where docBinder "" = PrettyString.empty
        docBinder name = PrettyString.text name 

        docGuard AllPG = PrettyString.empty
        docGuard (PredicatePG pred) = docMacro pred
        docGuard (ListPG hdPat tlPat) =
          PrettyString.sep
            [PrettyString.char '(' <> docPat hdPat,
             PrettyString.text "+>",
             docPat tlPat <> PrettyString.char ')']
        docGuard (TuplePG pats) =
          PrettyString.char '[' <>
          PrettyString.sep (PrettyString.intercalate (PrettyString.char ',') (map docPat pats)) <>
          PrettyString.char ']'
        docGuard (TypePG typeName pats) =
          PrettyString.sep
           (PrettyString.text (TypeName.fromTypeName typeName):map docPat pats)

docMatch :: ([Pat], Macro) -> PrettyString
docMatch (pats, body) =
  PrettyString.sep [docPats, PrettyString.equals, PrettyString.nest . docMacro $ body]
  where docPats = PrettyString.sep (map docPat pats)

docCond :: [([Pat], Macro)] -> PrettyString
docCond ms = PrettyString.vcat (map docMatch ms)

isParens :: Bool -> Macro -> Bool
isParens right AppM {} = right
isParens _ CharM {}    = False
isParens _ IdM {}      = False
isParens _ IntM {}     = False
isParens _ RealM {}    = False
isParens _ StringM {}  = False
isParens _ _           = True

docMacro :: Macro -> PrettyString
docMacro (AndM m1 m2) = docMacro m1 <+> PrettyString.text "&&" <+> docMacro m2
docMacro (AppM m1 m2) =
  let
    fn1 | isParens False m1 = PrettyString.parens . docMacro
        | otherwise = docMacro
    fn2 | isParens True m2 = PrettyString.parens . docMacro
        | otherwise = docMacro
  in
    PrettyString.sep [fn1 m1, PrettyString.nest (fn2 m2)]
docMacro (BinOpM op m1 m2) =
  docMacro m1 <+> PrettyString.text op <+> docMacro m2
docMacro (CharM c) = PrettyString.char '\'' <> PrettyString.char c <> PrettyString.char '\''
docMacro (CondM ms) = docCond ms
docMacro (FnDeclM name body) =
  PrettyString.sep [PrettyString.text "def" <+> PrettyString.text name, PrettyString.nest (docMacro body)]
docMacro (IdM name) = PrettyString.text (show name)
docMacro (IntM i) = PrettyString.int i
docMacro (ModuleM name uses defns) =
  PrettyString.text "me" <+> PrettyString.text name
  $+$
  PrettyString.empty
  $+$
  PrettyString.vcat (map docUse uses)
  $+$
  PrettyString.vcat
    (PrettyString.intercalate (PrettyString.text "\n") (map docMacro defns))
  where docUse (use, "") =
          PrettyString.text "use" <+> PrettyString.text use
        docUse (use, qual) =
          docUse (use, "") <+> PrettyString.text "as" <+> PrettyString.text qual
docMacro (OrM m1 m2) = docMacro m1 <+> PrettyString.text "||" <+> docMacro m2
docMacro (RealM r) = PrettyString.double r
docMacro (SeqM ms) =
  PrettyString.char '[' <>
  PrettyString.sep (PrettyString.intercalate (PrettyString.text ", ") (map docMacro ms)) <>
  PrettyString.char ']'
docMacro (StringM str) =
  PrettyString.char '"' <> PrettyString.text str <> PrettyString.char '"'
docMacro (TypeDeclM typeName cons) =
  PrettyString.sep [PrettyString.text "type" <+> PrettyString.text (TypeName.fromTypeName typeName), PrettyString.nest consDoc]
  where docConstructor (Constructor consName consPat) =
          PrettyString.text (TypeName.fromTypeName consName) <+> docPat consPat
        consDoc = PrettyString.sep . PrettyString.intercalate (PrettyString.text "|") . map docConstructor $ cons
docMacro (WhereM m ms) =
  PrettyString.sep
  [docMacro m,
   PrettyString.sep [PrettyString.text "where {",
                     PrettyString.nest (PrettyString.vcat (map docMacro ms)),
                     PrettyString.char '}']]
