{-# LANGUAGE ParallelListComp #-}
module Macros where

import Data.Stx
import Data.Type


mkDefn :: String -> Pat String -> Stx String
mkDefn name pat =
    let
        t = ArrowT (SeqT DynT) DynT
        arg = "arg"
        mkname = "mk" ++ name
        match = namePat arg (mkPredPat (patPred pat))
        body = AppStx (TypeMkStx name) (IdStx arg)
    in
      DefnStx (Just t) NrDef mkname (CondMacro [([match], body)] mkname)


isDefn :: String -> Stx String
isDefn name =
    let 
        t = ArrowT DynT DynT
        isname = "is" ++ name
    in
      DefnStx (Just t) NrDef isname (TypeIsStx name)


unDefn :: String -> Pat String -> Stx String
unDefn name pat =
    let
        t = ArrowT DynT DynT
        arg = "arg"
        unname = "un" ++ name
        isname = "is" ++ name
        match = namePat arg (mkPredPat (IdStx isname))
        body = AppStx TypeUnStx (IdStx arg)
    in
      DefnStx (Just t) NrDef unname (CondMacro [([match], body)] unname)


destructorDefns :: String -> Pat String -> [Stx String]
destructorDefns name pat =
    let
        t = ArrowT DynT DynT
        isname = "is" ++ name
        unname = "un" ++ name
        body defns arg = foldAppStx (IdStx arg) (defns ++ [IdStx unname])
        argPat = namePat "arg" (mkPredPat (IdStx isname))
    in
      [ DefnStx (Just t) NrDef dname (CondMacro [([argPat], body defns "arg")] dname) | (dname, defns) <- patDefns pat ]


typeMacro :: String -> Pat String -> Stx String
typeMacro name pat =
    let defns = [mkDefn name pat, isDefn name, unDefn name pat] in
    TypeStx name (defns ++ destructorDefns name pat)