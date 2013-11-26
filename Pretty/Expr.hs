module Pretty.Expr where

import Data.Expr
import Data.PrettyString (PrettyString, (<>), (<+>), ($$), ($+$))
import qualified Data.PrettyString as PrettyString

data DocType = ExpDocT | RenDocT

isParens :: Bool -> Expr -> Bool
isParens right AppE {} = right
isParens _ CharE {}    = False
isParens _ IdE {}      = False
isParens _ IntE {}     = False
isParens _ RealE {}    = False
isParens _ _           = True

docCond :: (a -> PrettyString) -> DocType -> [(a, Expr)] -> String -> PrettyString
docCond fn t ms blame =
  foldl1 ($+$) (map docMatch ms ++ docBlame)
  where docMatch (x, e) =
          PrettyString.sep [fn x <+> PrettyString.equals, PrettyString.nest (docExpr t e)]

        docBlame = [PrettyString.text "_" <+>
                    PrettyString.equals <+>
                    PrettyString.text "blame" <+>
                    PrettyString.text blame]

docExpr :: DocType -> Expr -> PrettyString
docExpr t (AppE e1 e2) =
  let
    fn1 | isParens False e1 = PrettyString.parens . docExpr t
        | otherwise = docExpr t
    fn2 | isParens True e2 = PrettyString.parens . docExpr t
        | otherwise = docExpr t
  in
    PrettyString.sep [fn1 e1, PrettyString.nest (fn2 e2)]
docExpr _ (CharE c) = PrettyString.quotes (PrettyString.char c)
docExpr t (CondE ms blame) =
  PrettyString.sep [PrettyString.text "cond", PrettyString.nest (docCond (docExpr t) t ms blame)]
docExpr t (FnDecl kw name body) =
  PrettyString.sep [kwDoc kw <+> PrettyString.text name, PrettyString.nest (docExpr t body)]
  where kwDoc Def = PrettyString.text "def"
        kwDoc NrDef = PrettyString.text "nrdef"
docExpr _ (IdE name) = PrettyString.text (show name)
docExpr _ (IntE i) = PrettyString.int i
docExpr t (LambdaE arg body) =
  PrettyString.sep [PrettyString.text "\\" <> PrettyString.text arg <+> PrettyString.text "->", PrettyString.nest (docExpr t body)]
docExpr _ MergeE {} =
  error "Doc.Expr.docExpr: unhandled case for MergeE"
docExpr _ (RealE d) = PrettyString.double d
docExpr t (WhereE e es) =
  docExpr t e $$ PrettyString.cat (map (docExpr t) es)
