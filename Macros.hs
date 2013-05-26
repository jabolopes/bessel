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
        body = AppStx (CotypeMkStx name) (IdStx arg)
    in
      DefnStx (Just t) NrDef mkname (CondMacro [([match], body)] mkname)


isDefn :: String -> Stx String
isDefn name =
    let 
        t = ArrowT DynT DynT
        isname = "is" ++ name
    in
      DefnStx (Just t) NrDef isname (CotypeIsStx name)


unDefn :: String -> Pat String -> Stx String
unDefn name pat =
    let
        t = ArrowT DynT DynT
        arg = "arg"
        unname = "un" ++ name
        isname = "is" ++ name
        match = namePat arg (mkPredPat (IdStx isname))
        body = AppStx CotypeUnStx (IdStx arg)
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


cotypeMacro :: String -> Pat String -> Stx String
cotypeMacro name pat =
    let defns = [mkDefn name pat, isDefn name, unDefn name pat] in
    CotypeStx name (defns ++ destructorDefns name pat)