module Pretty.Data.Expr where

import Prelude hiding ((<>))

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

docCond :: (a -> PrettyString) -> [(a, Expr)] -> String -> PrettyString
docCond fn ms blame =
  foldl1 ($+$) (map docMatch ms ++ docBlame)
  where
    docMatch (x, e) =
      PrettyString.sep [fn x <+> PrettyString.text "->", PrettyString.nest (docExpr e)]

    docBlame =
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
docExpr (AppE e1 e2) =
  let
    fn1 | isParens False e1 = PrettyString.parens . docExpr
        | otherwise = docExpr
    fn2 | isParens True e2 = PrettyString.parens . docExpr
        | otherwise = docExpr
  in
    PrettyString.sep [fn1 e1, PrettyString.nest (fn2 e2)]
docExpr (CondE ms blame) =
  PrettyString.sep [PrettyString.text "cond", PrettyString.nest (docCond docExpr ms blame)]
docExpr (FnDecl kw name body) =
  PrettyString.sep [kwDoc kw <+> PrettyString.text (show name) <+> PrettyString.equals, PrettyString.nest (docExpr body)]
  where
    kwDoc Def = PrettyString.text "rec"
    kwDoc NrDef = PrettyString.text "nonrec"
docExpr (IdE name) = PrettyString.text (show name)
docExpr (LambdaE arg body) =
  PrettyString.sep [PrettyString.text "\\" <> PrettyString.text (show arg) <+>
                    PrettyString.text "->", PrettyString.nest (docExpr body)]
docExpr (LetE defn body) =
  PrettyString.vcat
  [PrettyString.text "let" <+> PrettyString.nest (docExpr defn) <+> PrettyString.text "in", docExpr body]
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
