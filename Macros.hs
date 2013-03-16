{-# LANGUAGE ParallelListComp #-}
module Macros where

import Data.List (intercalate)

import Data.Stx


-- patterns

patLambdaStxs :: Stx a -> [(String, Stx a)] -> [Stx a]
patLambdaStxs val = map (patLambdaDefn `uncurry`)
    where patLambdaDefn id mod = DefnStx NrDef id (AppStx mod val)


-- def

defMacro :: DefnKw -> String -> [([Pat String], Stx String)] -> Stx String
defMacro kw name guards =
    DefnStx kw name $ lambdaStx name guards


-- lambda

lambdaBody :: [a] -> [Pat a] -> Stx a -> Stx a
lambdaBody args pats body =
    let
        defns = [ (arg, patDefns pat) | arg <- args | pat <- pats ]
        defns' = concat [ patLambdaStxs (IdStx arg) defns | (arg, defns) <- defns ]
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
    let mkname = "mk" ++ name in
    defMacro NrDef mkname [([pat], TypeMkStx name)]


isDefn :: String -> Stx String
isDefn name =
    let isname = "is" ++ name in
    defMacro NrDef isname [([mkPat constTrueStx [] []], TypeIsStx name)]


unDefn :: String -> Pat String -> Stx String
unDefn name pat =
    let unname = "un" ++ name in
    defMacro NrDef unname [([pat], TypeUnStx)]


typeMacro :: String -> Pat String -> Stx String
typeMacro name pat =
    let
        fns = [mkDefn name pat, isDefn name, unDefn name pat]
        unname = "un" ++ name
        ds = map (\(name, mod) -> DefnStx NrDef name $ applyStx "o" [mod, IdStx unname]) (patDefns pat)
    in
      TypeStx name (fns ++ ds)