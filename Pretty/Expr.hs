module Pretty.Expr where

import Data.Expr
import Data.PrettyString (PrettyString, (<>), (<+>), ($$), ($+$))
import qualified Data.PrettyString as PrettyString

data DocType = ExpDocT | RenDocT

docCond :: (a -> PrettyString) -> DocType -> [(a, Expr)] -> String -> PrettyString
docCond fn t ms blame =
  foldl1 ($+$) (map docMatch ms ++ docBlame)
  where docMatch (x, e) =
          PrettyString.sep [fn x <+> PrettyString.equals, PrettyString.nest (docExpr t e)]

        docBlame = [PrettyString.text "_" <+> PrettyString.equals <+> PrettyString.text blame]

docExpr :: DocType -> Expr -> PrettyString
docExpr t (AppE e1 e2@AppE {}) =
  PrettyString.sep [docExpr t e1, PrettyString.parens (docExpr t e2)]
docExpr t (AppE e1 e2) =
  PrettyString.sep [docExpr t e1, PrettyString.nest (docExpr t e2)]
docExpr _ (CharE c) = PrettyString.char c
docExpr t (CondE ms blame) =
  PrettyString.sep [PrettyString.text "cond", PrettyString.nest (docCond (docExpr t) t ms blame)]
docExpr t (FnDecl kw name body) =
  PrettyString.sep [kwDoc kw <+> PrettyString.text name <+> PrettyString.equals, PrettyString.nest (docExpr t body)]
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
