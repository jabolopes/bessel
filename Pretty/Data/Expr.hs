module Pretty.Data.Expr where

import Prelude hiding ((<>))

import qualified Data.List as List

import Data.Expr
import Data.Literal (Literal(..))
import Data.PrettyString (PrettyString, (<>), (<+>), ($+$))
import qualified Data.PrettyString as PrettyString

isParens :: Bool -> Expr -> Bool
isParens right AppE {} = right
isParens _ IdE {}      = False
isParens _ LetE {}     = False
isParens _ LiteralE {} = False
isParens _ _           = True

docParens :: Bool -> Expr -> PrettyString
docParens right src
  | isParens right src = PrettyString.parens $ docExpr src
  | otherwise = docExpr src

docCond :: [(Expr, Expr)] -> String -> PrettyString
docCond matches blame =
  foldl1 ($+$) (map docMatch matches ++ docBlame)
  where
    docMatch (x, e) =
      PrettyString.sep [docExpr x <+> PrettyString.text "->", PrettyString.nest (docExpr e)]

    -- TODO: This is bound to break. Perhaps we can move the blame to
    -- the interpreter and leave it completely out of the Source and
    -- Expr languages.
    docBlame
     | "irrefutable" `List.isPrefixOf` blame =
       []
     | otherwise =
       [PrettyString.text "_" <+>
        PrettyString.text "->" <+>
        PrettyString.text "blame" <+>
        PrettyString.text blame]

docLiteral :: Literal -> PrettyString
docLiteral (CharL c) = PrettyString.quotes $ PrettyString.char c
docLiteral (IntL i) = PrettyString.int i
docLiteral (RealL d) =  PrettyString.double d
docLiteral (StringL s) = PrettyString.string s

docExpr :: Expr -> PrettyString
docExpr (AnnotationE expr typ) =
  -- TODO: replace show typ with pretty typ
  PrettyString.sep [PrettyString.text (show typ) <+> PrettyString.text ":", PrettyString.nest (docExpr expr)]
docExpr (AppE expr1 expr2) =
  PrettyString.sep [docParens False expr1,
                    PrettyString.nest (docParens True expr2)]
docExpr (CondE matches blame) =
  PrettyString.sep [PrettyString.text "cond", PrettyString.nest (docCond matches blame)]
docExpr (FnDecl kw name body) =
  PrettyString.sep
    [PrettyString.text "let" <+> kwDoc kw <+> PrettyString.text (show name) <+> PrettyString.equals,
     PrettyString.nest (docExpr body)]
  where
    kwDoc Def = PrettyString.text "rec"
    kwDoc NrDef = PrettyString.text "nonrec"
docExpr (IdE name) = PrettyString.text (show name)
docExpr (LambdaE arg body) =
  PrettyString.sep [PrettyString.text "\\" <> PrettyString.text (show arg) <+>
                    PrettyString.text "->", PrettyString.nest (docExpr body)]
docExpr (LetE defn body) =
  PrettyString.vcat
  [docExpr defn <+> PrettyString.text "in", docExpr body]
docExpr (LiteralE literal) =
  docLiteral literal

-- PrettyString for a list of 'Expr's.
docExprList :: [Expr] -> PrettyString
docExprList [] = PrettyString.text "[]"
docExprList (src:srcs) =
  PrettyString.sep .
  (++ [PrettyString.text "]"]) .
  PrettyString.intercalate (PrettyString.text ",") $
  ((PrettyString.text "[" <+> docExpr src):) $
  map (\x -> PrettyString.text "," <+> docExpr x) srcs
