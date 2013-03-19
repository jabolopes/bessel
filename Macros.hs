{-# LANGUAGE ParallelListComp #-}
module Macros where

import Data.Pat
import Data.Stx


-- patterns

applyMods val [] = val
applyMods val (mod:mods) =
    AppStx mod (applyMods val mods)


mkPatDefns :: Stx a -> [(String, [Stx a])] -> [Stx a]
mkPatDefns val = map (mkDefn `uncurry`)
    where mkDefn id mods =
              DefnStx NrDef id (applyMods val mods)


-- def

defMacro :: DefnKw -> String -> [([Pat String], Stx String)] -> Stx String
defMacro kw name guards =
    DefnStx kw name (lambdaStx name guards)


-- lambda

lambdaBody :: [a] -> [Pat a] -> Stx a -> Stx a
lambdaBody args pats body =
    let
        defns = [ (arg, patDefns pat) | arg <- args | pat <- pats ]
        defns' = concat [ mkPatDefns (IdStx arg) defns | (arg, defns) <- defns ]
    in
      if null defns' then
          body
      else
          WhereStx body defns'


lambdaStx :: String -> [([Pat String], Stx String)] -> Stx String
lambdaStx sigdesc ms =
    let
        args = [ "_arg" ++ show i | i <- [1..length (fst (head ms))] ]
        (patss, exprs) = unzip ms
        preds = map (\pats -> andStxs [ AppStx (patPred pat) (IdStx arg) | arg <- args | pat <- pats ]) patss
        exprs' = zipWith (lambdaBody args) patss exprs
    in
      lambdas args (CondStx (zip preds exprs') sigdesc)
    where lambdas [] body = body
          lambdas (arg:args) body =
              LambdaStx arg (lambdas args body)

          andStxs stxs =
              foldl1 andStx stxs


lambdaMacro :: [([Pat String], Stx String)] -> Stx String
lambdaMacro = lambdaStx "lambda"


-- type

mkDefn :: String -> Pat String -> Stx String
mkDefn name pat =
    let
        arg = "arg"
        mkname = "mk" ++ name
        match = namePat arg (mkPredPat (patPred pat))
        body = AppStx (TypeMkStx name) (IdStx arg)
    in
      defMacro NrDef mkname [([match], body)]


isDefn :: String -> Stx String
isDefn name =
    let isname = "is" ++ name in
    DefnStx NrDef isname (TypeIsStx name)


unDefn :: String -> Pat String -> Stx String
unDefn name pat =
    let
        arg = "arg"
        unname = "un" ++ name
        match = namePat arg (mkPredPat (TypeIsStx name))
        body = AppStx TypeUnStx (IdStx arg)
    in
      defMacro NrDef unname [([match], body)]


typeMacro :: String -> Pat String -> Stx String
typeMacro name pat =
    let
        fns = [mkDefn name pat, isDefn name, unDefn name pat]
        unname = "un" ++ name
        body defns name arg = applyMods (AppStx (IdStx unname) (IdStx arg)) defns
        ds = [ defMacro NrDef field [([namePat "arg" (mkPredPat (TypeIsStx name))], body defns field "arg")] | (field, defns) <- patDefns pat ]
    in
      TypeStx name (fns ++ ds)