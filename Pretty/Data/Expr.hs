module Pretty.Data.Expr where

import Data.Expr
import Data.PrettyString (PrettyString, (<>), (<+>), ($+$))
import qualified Data.PrettyString as PrettyString

isParens :: Bool -> Expr -> Bool
isParens right AppE {} = right
isParens _ CharE {}    = False
isParens _ IdE {}      = False
isParens _ IntE {}     = False
isParens _ LetE {}     = False
isParens _ RealE {}    = False
isParens _ _           = True

docCond :: (a -> PrettyString) -> [(a, Expr)] -> String -> PrettyString
docCond fn ms blame =
  foldl1 ($+$) (map docMatch ms ++ docBlame)
  where
    docMatch (x, e) =
      PrettyString.sep [fn x <+> PrettyString.equals, PrettyString.nest (docExpr e)]

    docBlame =
      [PrettyString.text "_" <+>
       PrettyString.equals <+>
       PrettyString.text "blame" <+>
       PrettyString.text blame]

docExpr :: Expr -> PrettyString
docExpr (AppE e1 e2) =
  let
    fn1 | isParens False e1 = PrettyString.parens . docExpr
        | otherwise = docExpr
    fn2 | isParens True e2 = PrettyString.parens . docExpr
        | otherwise = docExpr
  in
    PrettyString.sep [fn1 e1, PrettyString.nest (fn2 e2)]
docExpr (CharE c) = PrettyString.quotes (PrettyString.char c)
docExpr (CondE ms blame) =
  PrettyString.sep [PrettyString.text "cond", PrettyString.nest (docCond docExpr ms blame)]
docExpr (FnDecl kw name body) =
  PrettyString.sep [kwDoc kw <+> PrettyString.text (show name), PrettyString.nest (docExpr body)]
  where kwDoc Def = PrettyString.text "def"
        kwDoc NrDef = PrettyString.text "nrdef"
docExpr (IdE name) = PrettyString.text (show name)
docExpr (IntE i) = PrettyString.int i
docExpr (LetE defn body) =
  PrettyString.sep
  [PrettyString.text "let", PrettyString.nest (docExpr defn),
   PrettyString.text "in", docExpr body]
docExpr (LambdaE arg body) =
  PrettyString.sep [PrettyString.text "\\" <> PrettyString.text (show arg) <+>
                    PrettyString.text "->", PrettyString.nest (docExpr body)]
docExpr (RealE d) = PrettyString.double d

-- PrettyString for a list of 'Expr's.
docExprList :: [Expr] -> PrettyString
docExprList [] = PrettyString.text "[]"
docExprList (src:srcs) =
  PrettyString.sep .
  (++ [PrettyString.text "]"]) .
  PrettyString.intercalate (PrettyString.text ",") $
  ((PrettyString.text "[" <+> docExpr src):) $
  map (\x -> PrettyString.text "," <+> docExpr x) srcs
