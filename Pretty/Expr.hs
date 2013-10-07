module Pretty.Expr where

import Data.Expr
import Data.PrettyString (PrettyString, (<>), (<+>), ($$), ($+$))
import qualified Data.PrettyString as PrettyString

data DocType = SrcDocT | ExpDocT | RenDocT

docPat :: DocType -> Pat -> PrettyString
docPat t pat =
  docBinder (patDefns pat) <> PrettyString.text "@" <> PrettyString.parens (docExpr t (patPred pat))
  where docBinder [] = PrettyString.empty
        docBinder ((name, _):_) = PrettyString.text name

docCond :: (a -> PrettyString) -> DocType -> [(a, Expr)] -> String -> PrettyString
docCond fn t ms blame =
  foldl1 ($+$) (map docMatch ms ++ docBlame t)
  where docMatch (x, e) =
          PrettyString.sep [fn x <+> PrettyString.equals, PrettyString.nest (docExpr t e)]

        docBlame SrcDocT = []
        docBlame _ = [PrettyString.text "_" <+> PrettyString.equals <+> PrettyString.text blame]

docExpr :: DocType -> Expr -> PrettyString
docExpr _ (IdE name) = PrettyString.text (show name)
docExpr _ (IntE i) = PrettyString.int i
docExpr _ (RealE d) = PrettyString.double d
docExpr _ (CharE c) = PrettyString.char c
docExpr t (SeqE es) =
  PrettyString.hcat (PrettyString.intercalate (PrettyString.text ", ") (map (docExpr t) es))
docExpr t (AppE e1 e2@AppE {}) =
  PrettyString.sep [docExpr t e1, PrettyString.parens (docExpr t e2)]
docExpr t (AppE e1 e2) =
  PrettyString.sep [docExpr t e1, PrettyString.nest (docExpr t e2)]
docExpr t (CondMacro ms blame) =
  docCond (PrettyString.sep . map (docPat t)) t ms blame
docExpr t (CondE ms blame) =
  PrettyString.sep [PrettyString.text "cond", PrettyString.nest (docCond (docExpr t) t ms blame)]
docExpr t (FnDecl kw name body) =
  PrettyString.sep [kwDoc t kw <+> PrettyString.text name <+> PrettyString.equals, PrettyString.nest (docExpr t body)]
  where kwDoc SrcDocT _ = PrettyString.text "def"
        kwDoc _ Def = PrettyString.text "def"
        kwDoc _ NrDef = PrettyString.text "nrdef"
docExpr t (LambdaE arg body) =
  PrettyString.sep [PrettyString.text "\\" <> PrettyString.text arg <+> PrettyString.text "->", PrettyString.nest (docExpr t body)]
docExpr _ MergeE {} =
  error "Doc.Expr.docExpr: unhandled case for MergeE"
docExpr t (WhereE e es) =
  docExpr t e $$ PrettyString.cat (map (docExpr t) es)
