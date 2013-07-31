module Printer.AbbrevExpr where

import Data.Char (isSymbol)
import Data.List (intercalate)

import Data.Expr (DefnKw(..), Expr(..))
import Data.QualName (QualName)
import qualified Data.QualName as QualName (fromQualName)


showVeryAbbrev :: Expr -> String
showVeryAbbrev (IdE name)
    | isSymbol $ head $ QualName.fromQualName name = "(" ++ QualName.fromQualName name ++ ")"
    | otherwise = QualName.fromQualName name

showVeryAbbrev (IntE i) = show i
showVeryAbbrev (CharE c) = show c
showVeryAbbrev (RealE d) = show d
showVeryAbbrev _ = "..."


showAbbrev :: Expr -> String
showAbbrev expr@IdE {} = showVeryAbbrev expr
showAbbrev expr@IntE {} = showVeryAbbrev expr
showAbbrev expr@RealE {} = showVeryAbbrev expr
showAbbrev expr@CharE {} = showVeryAbbrev expr
showAbbrev (SeqE exprs) = "[" ++ intercalate ", " (map showVeryAbbrev exprs) ++ "]"

showAbbrev (AppE expr1 expr2) =
    showVeryAbbrev expr1 ++ " " ++ showVeryAbbrev expr2

showAbbrev (CondE ms blame) =
    intercalate " | " (map (showMatch . fst) ms)
    where showMatch expr = showVeryAbbrev expr ++ " -> ..."

showAbbrev (FnDecl _ kw name body) =
    showKw kw ++ " " ++ name ++ " " ++ showVeryAbbrev body
    where showKw Def = "def"
          showKw NrDef = "nrdef"

showAbbrev (LambdaE arg Nothing body) =
    "\\" ++ arg ++ ". " ++ showVeryAbbrev body

showAbbrev (LambdaE arg (Just t) body) =
    "\\" ++ arg ++ ":" ++ t ++ ". " ++ showVeryAbbrev body

showAbbrev (WhereE expr exprs) =
    showAbbrev expr ++ " where ..."

showAbbrev expr = showVeryAbbrev expr