{-# LANGUAGE ParallelListComp #-}
module Data.Stx where

import Data.List (intercalate)


data DefnKw
    = Def
    | NrDef
      deriving (Eq, Ord, Show)


data Namespace a
    = Namespace [(String, String)] [Stx a]
      deriving (Eq, Ord, Show)


data Stx a
    = CharStx Char
    | IntStx Int
    | DoubleStx Double
    | SeqStx [Stx a]

    | IdStx a

    | AppStx (Stx a) (Stx a)
    | CondStx [(Stx a, Stx a)] String
    | DefnStx DefnKw String (Stx a)
    | LambdaStx String (Stx a)
    | ModuleStx [String] (Namespace a)
    | TypeStx String [Stx a]
    | TypeMkStx String
    | TypeUnStx
    | TypeIsStx String
    | WhereStx (Stx a) [Stx a]
      deriving (Eq, Ord, Show)


isCharStx :: Stx a -> Bool
isCharStx (CharStx _) = True
isCharStx _ = False


isAppStx :: Stx a -> Bool
isAppStx (AppStx _ _) = True
isAppStx _ = False


isLambdaStx :: Stx a -> Bool
isLambdaStx (LambdaStx _ _) = True
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
isValueStx (LambdaStx _ _) = True
isValueStx _ = False


andStx :: Stx String -> Stx String -> Stx String
andStx stx1 stx2 =
    let
        m1 = (binOpStx "==" stx1 (IdStx "false"), IdStx "false")
        m2 = (IdStx "true", stx2)
    in
      CondStx [m1, m2] "irrefutable 'and' pattern"


appStx :: a -> Stx a -> Stx a
appStx str stx = AppStx (IdStx str) stx


applyStx :: a -> [Stx a] -> Stx a
applyStx str = appStx str . SeqStx


binOpStx :: a -> Stx a -> Stx a -> Stx a
binOpStx op stx1 stx2 =
    AppStx (appStx op stx1) stx2


constStx :: Stx a -> Stx a
constStx stx1 = LambdaStx "_" stx1


constTrueStx :: Stx String
constTrueStx = constStx (IdStx "true")


orStx :: Stx String -> Stx String -> Stx String
orStx stx1 stx2 =
    let
        m1 = (binOpStx "==" stx1 (IdStx "false"), stx2)
        m2 = (IdStx "true", IdStx "true")
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

showAbbrev (IdStx name) = name

showAbbrev (AppStx stx1 stx2) =
  paren stx1 ++ " " ++ paren stx2
  where paren stx | needsParen stx = "(" ++ showAbbrev stx ++ ")"
                  | otherwise = showAbbrev stx

showAbbrev (LambdaStx arg body) =
  "\\" ++ arg ++ ". " ++ showAbbrev body

showAbbrev stx = show stx