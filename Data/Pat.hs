module Data.Pat where

import Data.Stx


data Pat = Pat (Stx String) [(String, Stx String)]
         deriving (Eq, Ord, Show)


mkPat :: Stx String -> [Stx String] -> [Pat] -> Pat
mkPat pred mods pats =
    Pat (patPred pred pats) (modPats mods pats)
    where patPred pred [] = pred
          patPred pred pats = AppStx pred $ SeqStx $ map (\(Pat pred _) -> pred) pats
          
          modDefns _ [] = []
          modDefns mod ((str, mod'):defns) =
              (str, applyStx "o" [mod', mod]):modDefns mod defns

          modPats [] [] = []    
          modPats (mod:mods) (Pat _ defns:pats) =
              modDefns mod defns ++ modPats mods pats


namePat :: String -> Pat -> Pat
namePat name (Pat pred defns) =
    Pat pred $ (name, IdStx "id"):defns