{-# LANGUAGE ParallelListComp #-}
module Macros where

import Data.Stx


mkDefn :: String -> Pat String -> Stx String
mkDefn name pat =
    let
        arg = "arg"
        mkname = "mk" ++ name
        match = namePat arg (mkPredPat (patPred pat))
        body = AppStx (TypeMkStx name) (IdStx arg)
    in
      DefnStx Nothing NrDef mkname (CondMacro [([match], body)] mkname)


isDefn :: String -> Stx String
isDefn name =
    let isname = "is" ++ name in
    DefnStx Nothing NrDef isname (TypeIsStx name)


unDefn :: String -> Pat String -> Stx String
unDefn name pat =
    let
        arg = "arg"
        unname = "un" ++ name
        isname = "is" ++ name
        match = namePat arg (mkPredPat (IdStx isname))
        body = AppStx TypeUnStx (IdStx arg)
    in
      DefnStx Nothing NrDef unname (CondMacro [([match], body)] unname)


destructorDefns :: String -> Pat String -> [Stx String]
destructorDefns name pat =
    let
        isname = "is" ++ name
        unname = "un" ++ name
        body defns arg = foldAppStx (IdStx arg) (defns ++ [IdStx unname])
        argPat = namePat "arg" (mkPredPat (IdStx isname))
    in
     [ DefnStx Nothing NrDef dname (CondMacro [([argPat], body defns "arg")] dname) | (dname, defns) <- patDefns pat ]


typeMacro :: String -> Pat String -> Stx String
typeMacro name pat =
    let defns = [mkDefn name pat, isDefn name, unDefn name pat] in
    TypeStx name (defns ++ destructorDefns name pat)