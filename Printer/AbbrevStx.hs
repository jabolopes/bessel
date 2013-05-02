module Printer.AbbrevStx where

import Data.Char (isSymbol)
import Data.List (intercalate)

import Data.Stx


showVeryAbbrev stx@(CharStx c) = show c
showVeryAbbrev stx@(IntStx i) = show i
showVeryAbbrev stx@(DoubleStx d) = show d

showVeryAbbrev stx@(IdStx name)
    | isSymbol (head name) = "(" ++ name ++ ")"
    | otherwise = name

showVeryAbbrev _ = "..."


showAbbrev :: Stx String -> String
showAbbrev stx@(CharStx _) = showVeryAbbrev stx
showAbbrev stx@(IntStx _) = showVeryAbbrev stx
showAbbrev stx@(DoubleStx _) = showVeryAbbrev stx
showAbbrev (SeqStx stxs) = "[" ++ intercalate ", " (map showVeryAbbrev stxs) ++ "]"

showAbbrev stx@(IdStx _) = showVeryAbbrev stx

showAbbrev (AppStx stx1 stx2) =
    showVeryAbbrev stx1 ++ " " ++ showVeryAbbrev stx2

showAbbrev (CondStx ms blame) =
    intercalate " | " (map (showMatch . fst) ms)
    where showMatch stx = showVeryAbbrev stx ++ " -> ..."

showAbbrev (DefnStx _ kw name body) =
    showKw kw ++ " " ++ name ++ " " ++ showVeryAbbrev body
    where showKw Def = "def"
          showKw NrDef = "nrdef"

showAbbrev (LambdaStx arg Nothing body) =
    "\\" ++ arg ++ ". " ++ showVeryAbbrev body

showAbbrev (LambdaStx arg (Just t) body) =
    "\\" ++ arg ++ ":" ++ t ++ ". " ++ showVeryAbbrev body

showAbbrev (WhereStx stx stxs) =
    showAbbrev stx ++ " where ..."

showAbbrev stx = showVeryAbbrev stx