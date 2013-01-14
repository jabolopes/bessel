module Macros where

import Data.Stx
import Data.Pat


-- prime

primesMacro :: Int -> String -> Stx String
primesMacro primes id = primesMacro' primes (IdStx id)
    where primesMacro' primes stx
              | primes <= 0 = stx
              | otherwise = primesMacro' (primes - 1) (AppStx (IdStx "lift") stx)


-- patterns

patDefnStxs :: [(String, Stx a)] -> [Stx a]
patDefnStxs = map (DefnStx NrDef `uncurry`)


patLambdaStxs :: Stx a -> [(String, Stx a)] -> [Stx a]
patLambdaStxs val = map (patLambdaDefn `uncurry`)
    where patLambdaDefn id mod = DefnStx NrDef id $ AppStx mod val


-- cond

ifthenMacro :: Pat -> Stx String -> Stx String
ifthenMacro (Pat pred defns) stx =
    let stx' = WhereStx stx (patDefnStxs defns) in
    applyStx "ifthen" [pred, stx']
    -- applyStx "cond" [pred, stx']


ifelseMacro :: Pat -> Stx String -> Stx String -> Stx String
ifelseMacro (Pat pred defns) stx1 stx2 =
    let
        defns' = patDefnStxs defns
        stx1' = WhereStx stx1 (defns')
        stx2' = WhereStx stx2 (defns')
    in
      applyStx "ifelse" [pred, stx1', stx2']
      -- applyStx "cond" [pred, stx1', stx2']


-- def

defBody :: [Pat] -> Stx String -> String -> Stx String
defBody pats body sigdesc = loop [1..] pats
    where loop _ [] = body
          loop (i:is) (pat:pats) =
              let arg = "arg" ++ show i in
              lambdaStx arg pat (loop is pats) sigdesc


defMacro :: DefnKw -> String -> [Pat] -> Stx String -> Stx String
defMacro kw name pats body =
    DefnStx kw name $ defBody pats body name


defPatMacro :: DefnKw -> String -> [Pat] -> Pat -> Stx String -> Stx String
defPatMacro kw name pats pat body =
    let
        arg = "arg" ++ show (length pats + 1)
        body' = ifelseMacro pat body $ signalStx name arg
    in
      DefnStx kw name $ defBody pats body' name


defExprMacro :: DefnKw -> String -> [Pat] -> Stx String -> Stx String -> Stx String
defExprMacro kw name pats stx body =
    let
        arg = "arg" ++ show (length pats + 1)
        -- body' = applyStx "cond" [stx, body, signalStx name arg]
        body' = applyStx "ifelse" [stx, body, signalStx name arg]
    in
      DefnStx kw name $ defBody pats body' name


-- exdef


exdefBody :: Pat -> String -> Stx String -> Stx String
exdefBody pat name body =
    let body' = ifelseMacro pat (IdStx name) (signalStx name "range") in
    applyStx "o" [body', body]


exdefDefns :: (String -> Stx String -> a) -> Pat -> Stx String -> [a]
exdefDefns fn defpat@(Pat _ defns) body =
    [ fn name (exdefBody defpat name body) | (name, _) <- defns ]


exdefMacro :: Pat -> [Pat] -> Stx String -> [Stx String]
exdefMacro defpat@(Pat _ defns) pats body =
    exdefDefns (\name -> defMacro Def name pats) defpat body


exdefPatMacro :: Pat -> [Pat] -> Pat -> Stx String -> [Stx String]
exdefPatMacro defpat@(Pat _ defns) pats pat body =
    exdefDefns (\name -> defPatMacro Def name pats pat) defpat body


exdefExprMacro :: Pat -> [Pat] -> Stx String -> Stx String -> [Stx String]
exdefExprMacro defpat@(Pat _ defns) pats stx body =
    exdefDefns (\name -> defExprMacro Def name pats stx) defpat body


-- lambda


lambdaStx :: String -> Pat -> Stx String -> String -> Stx String
lambdaStx arg (Pat pred defns) body sigdesc =
    let
        _arg = '_':arg
        defns' = patLambdaStxs (IdStx _arg) defns

        lambda = LambdaStx _arg $ WhereStx body (defns')
        body' = applyStx "ifelse" [pred, lambda, signalStx sigdesc arg]
        -- body' = applyStx "cond" [pred, lambda, signalStx sigdesc arg]
    in
      body'


lambdaMacro :: Pat -> Stx String -> Stx String
lambdaMacro pat body =
    lambdaStx "arg" pat body "lambda"


-- type


typeExprMacro :: String -> Stx String -> Stx String
typeExprMacro name stx =
    let
        mkname = "mk" ++ name
        unname = "un" ++ name
        isname = "is" ++ name
        mklambda = TypeMkStx name
        unlambda = TypeUnStx
        islambda = TypeIsStx name
        mk = DefnStx NrDef mkname $ applyStx "ifelse" [stx, mklambda, signalStx mkname "arg1"]
        -- mk = DefnStx NrDef mkname $ applyStx "cond" [stx, mklambda, signalStx mkname "arg1"]
        un = DefnStx NrDef unname $ applyStx "ifelse" [IdStx isname, unlambda, signalStx unname "arg1"]
        -- un = DefnStx NrDef unname $ applyStx "cond" [IdStx isname, unlambda, signalStx unname "arg1"]
        is = DefnStx NrDef isname islambda
    in
      -- edit: attention to the order "mk, is, un" because
      -- of dependencies between these functions and interpreter
      -- evaluation order
      TypeStx name [mk, is, un]


typePatMacro :: String -> Pat -> Stx String
typePatMacro name (Pat pred defns) =
    let
        unname = "un" ++ name
        defns' = map (\(name, mod) -> DefnStx NrDef name $ applyStx "o" [mod, IdStx unname]) defns
        TypeStx name' defns'' = typeExprMacro name pred
    in
      TypeStx name' $ defns'' ++ defns'