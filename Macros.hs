{-# LANGUAGE ParallelListComp #-}
module Macros where

import Data.List (intercalate)

import Data.Stx


-- patterns

patDefnStxs :: [(String, Stx a)] -> [Stx a]
patDefnStxs = map (DefnStx NrDef `uncurry`)


patLambdaStxs :: Stx a -> [(String, Stx a)] -> [Stx a]
patLambdaStxs val = map (patLambdaDefn `uncurry`)
    where patLambdaDefn id mod = DefnStx NrDef id $ AppStx mod val


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

          -- andStx stx1 stx2 =
          --     IdStx "&&" `AppStx` stx1 `AppStx` stx2

          andStxs stxs =
              foldl1 andStx stxs


lambdaMacro :: [([Pat String], Stx String)] -> Stx String
lambdaMacro = lambdaStx "lambda"


-- type

typeExprMacro :: String -> Stx String -> Stx String
typeExprMacro = undefined
-- typeExprMacro name stx =
--     let
--         mkname = "mk" ++ name
--         unname = "un" ++ name
--         isname = "is" ++ name
--         mklambda = TypeMkStx name
--         unlambda = TypeUnStx
--         islambda = TypeIsStx name
--         mk = DefnStx NrDef mkname $ applyStx "ifelse" [stx, mklambda, signalStx mkname "arg1"]
--         -- mk = DefnStx NrDef mkname $ applyStx "cond" [stx, mklambda, signalStx mkname "arg1"]
--         un = DefnStx NrDef unname $ applyStx "ifelse" [IdStx isname, unlambda, signalStx unname "arg1"]
--         -- un = DefnStx NrDef unname $ applyStx "cond" [IdStx isname, unlambda, signalStx unname "arg1"]
--         is = DefnStx NrDef isname islambda
--     in
--       -- edit: attention to the order "mk, is, un" because
--       -- of dependencies between these functions and interpreter
--       -- evaluation order
--       TypeStx name [mk, is, un]


typePatMacro :: String -> Pat String -> Stx String
typePatMacro name (Pat pred defns) = undefined
    -- let
    --     unname = "un" ++ name
    --     defns' = map (\(name, mod) -> DefnStx NrDef name $ applyStx "o" [mod, IdStx unname]) defns
    --     TypeStx name' defns'' = typeExprMacro name pred
    -- in
    --   TypeStx name' $ defns'' ++ defns'