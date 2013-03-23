{-# LANGUAGE ParallelListComp #-}
module Data.Stx where

import Data.Char (isSymbol)
import Data.List (intercalate)
import Data.Type


data DefnKw
    = Def
    | NrDef
      deriving (Show)


data Namespace a
    = Namespace [(String, String)] [Stx a]
      deriving (Show)


data Pat a
    = Pat { patPred :: Stx a
          , patDefns :: [(String, [Stx a])] }
      deriving (Show)


mkGeneralPat :: Stx a -> [[Stx a]] -> [Pat a] -> Pat a
mkGeneralPat pred mods pats =
    Pat (mkPred pred pats) (modPats mods pats)
    where mkPred pred [] = pred
          mkPred pred pats =
              AppStx pred $ SeqStx $ map patPred pats

          modDefns _ [] = []
          modDefns mod ((str, mod'):defns) =
              (str, mod' ++ mod):modDefns mod defns

          modPats [] [] = []    
          modPats (mod:mods) (Pat _ defns:pats) =
              modDefns mod defns ++ modPats mods pats


mkPat :: Stx String -> [Stx String] -> [Pat String] -> Pat String
mkPat pred mods pats =
    mkGeneralPat pred (map (:[]) mods) pats


mkPredPat :: Stx String -> Pat String
mkPredPat pred =
    mkGeneralPat pred [] []


mkListPat :: [Pat String] -> Pat String
mkListPat pats =
    mkGeneralPat (IdStx "plist") (map (reverse . listRef) [1..length pats]) pats
    where listRef 1 = [IdStx "hd"]
          listRef i = IdStx "tl":listRef (i - 1)


namePat :: String -> Pat String -> Pat String
namePat name (Pat pred defns) =
    Pat pred $ (name, []):defns


data Stx a
    = CharStx Char
    | IntStx Int
    | DoubleStx Double
    | SeqStx [Stx a]

    | IdStx a

    | AppStx (Stx a) (Stx a)

    | CondMacro [([Pat a], Stx a)] String
    | CondStx [(Stx a, Stx a)] String

    | DefnStx (Maybe Type) DefnKw String (Stx a)

    | LambdaMacro [Pat a] (Stx a)
    | LambdaStx String (Maybe String) (Stx a)

    | ModuleStx [String] (Namespace a)
    | TypeStx String [Stx a]
    | TypeMkStx String
    | TypeUnStx
    | TypeIsStx String
    | WhereStx (Stx a) [Stx a]
      deriving (Show)


isCharStx :: Stx a -> Bool
isCharStx (CharStx _) = True
isCharStx _ = False


isAppStx :: Stx a -> Bool
isAppStx (AppStx _ _) = True
isAppStx _ = False


isLambdaStx :: Stx a -> Bool
isLambdaStx (LambdaStx _ _ _) = True
isLambdaStx _ = False


isModuleStx :: Stx a -> Bool
isModuleStx (ModuleStx _ _) = True
isModuleStx _ = False


isWhereStx :: Stx a -> Bool
isWhereStx (WhereStx _ _) = True
isWhereStx _ = False


isValueStx :: Stx a -> Bool
isValueStx (CharStx _) = True
isValueStx (IntStx _) = True
isValueStx (DoubleStx _) = True
isValueStx (SeqStx _) = True
isValueStx (IdStx _) = True
isValueStx (LambdaStx _ _ _) = True
isValueStx _ = False


andStx :: Stx String -> Stx String -> Stx String
andStx stx1 stx2 =
    let
        m2 = (stx2, IdStx "true")
        m3 = (IdStx "true", IdStx "false")
        m1 = (stx1, CondStx [m2, m3] "irrefutable 'and' pattern")
    in
      CondStx [m1, m3] "irrefutable 'and' pattern"


appStx :: a -> Stx a -> Stx a
appStx str stx = AppStx (IdStx str) stx


applyStx :: a -> [Stx a] -> Stx a
applyStx str = appStx str . SeqStx


binOpStx :: a -> Stx a -> Stx a -> Stx a
binOpStx op stx1 stx2 =
    AppStx (appStx op stx1) stx2


constStx :: Stx a -> Stx a
constStx stx1 = LambdaStx "_" Nothing stx1
-- edit: maybe it should be TvarT or Evar instead of Nothing?


constTrueStx :: Stx String
constTrueStx = constStx (IdStx "true")


foldAppStx :: Stx a -> [Stx a] -> Stx a
foldAppStx x [] = x
foldAppStx x (y:ys) = AppStx y (foldAppStx x ys)


orStx :: Stx String -> Stx String -> Stx String
orStx stx1 stx2 =
    -- note: force 'stx1' and 'stx2' to be of type 'Bool'
    let
        m1 = (stx1, IdStx "true")
        m2 = (stx2, IdStx "true")
        m3 = (IdStx "true", IdStx "false")
    in
      CondStx [m1, m2] "irrefutable 'or' pattern"


signalStx :: String -> String -> Stx String -> Stx String
signalStx id str val =
    appStx "signal" (SeqStx [stringStx id, stringStx str, val])


stringStx :: String -> Stx a
stringStx str = SeqStx $ map CharStx str


needsParen :: Stx a -> Bool
needsParen stx =
  isAppStx stx ||
  isLambdaStx stx ||
  isWhereStx stx


showAbbrev :: Stx String -> String
showAbbrev (CharStx c) = show c
showAbbrev (IntStx i) = show i
showAbbrev (DoubleStx d) = show d
showAbbrev (SeqStx stxs) = "[" ++ intercalate ", " (map showAbbrev stxs) ++ "]"

showAbbrev (IdStx name)
    | isSymbol (head name) = "(" ++ name ++ ")"
    | otherwise = name

showAbbrev (AppStx stx1 stx2) =
  paren stx1 ++ " " ++ paren stx2
  where paren stx | needsParen stx = "(" ++ showAbbrev stx ++ ")"
                  | otherwise = showAbbrev stx

showAbbrev (LambdaStx arg Nothing body) =
  "\\" ++ arg ++ ". " ++ showAbbrev body

showAbbrev (LambdaStx arg (Just t) body) =
  "\\" ++ arg ++ ":" ++ t ++ ". " ++ showAbbrev body

showAbbrev stx = show stx