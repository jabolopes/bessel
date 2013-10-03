module Doc.Expr where

import Data.List (intercalate)
import Text.PrettyPrint

import Data.Expr
import qualified Doc.Doc as Doc

data DocType = SrcDocT | ExpDocT | RenDocT

docPat :: DocType -> Pat -> Doc
docPat t pat =
  docBinder (patDefns pat) <> text "@" <> parens (docExpr t (patPred pat))
  where docBinder [] = empty
        docBinder ((name, _):_) = text name

docCond :: (a -> Doc) -> DocType -> [(a, Expr)] -> String -> Doc
docCond fn t ms blame =
  foldl1 ($+$) (map docMatch ms ++ docBlame t)
  where docMatch (x, e) =
          sep [fn x <+> equals, Doc.nest (docExpr t e)]

        docBlame SrcDocT = []
        docBlame _ = [text "_" <+> equals <+> text blame]

docExpr :: DocType -> Expr -> Doc
docExpr _ (IdE name) = text (show name)
docExpr _ (IntE i) = int i
docExpr _ (RealE d) = double d
docExpr _ (CharE c) = char c
docExpr t (SeqE es) = hcat (punctuate (text ", ") (map (docExpr t) es))
docExpr t (AppE e1 e2@AppE {}) = sep [docExpr t e1, parens (docExpr t e2)]
docExpr t (AppE e1 e2) = sep [docExpr t e1, Doc.nest (docExpr t e2)]
docExpr t (CondMacro ms blame) = docCond (sep . map (docPat t)) t ms blame
docExpr t (CondE ms blame) = sep [text "cond", Doc.nest (docCond (docExpr t) t ms blame)]
docExpr t (FnDecl kw name body) =
  sep [kwDoc t kw <+> text name <+> equals, Doc.nest (docExpr t body)]
  where kwDoc SrcDocT _ = text "def"
        kwDoc _ Def = text "def"
        kwDoc _ NrDef = text "nrdef"
docExpr t (LambdaE arg body) =
  sep [text "\\" <> text arg <+> text "->", Doc.nest (docExpr t body)]
docExpr _ MergeE {} =
  error "Doc.Expr.docExpr: unhandled case for MergeE"
docExpr t (WhereE e es) =
  docExpr t e $$ cat (map (docExpr t) es)
