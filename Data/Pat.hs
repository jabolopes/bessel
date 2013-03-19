module Data.Pat where

import Data.Stx


data Pat a
    = Pat { patPred :: Stx a
          , patDefns :: [(String, [Stx a])] }


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
