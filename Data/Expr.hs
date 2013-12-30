module Data.Expr where

import qualified Data.List as List (nub)

import Data.QualName

data DefnKw
  = Def | NrDef
    deriving (Show)

data Expr
    = AppE Expr Expr
    | CharE Char

    -- |
    -- This construct is not available in the parser
    -- @
    -- case
    --   pred1 val1 -> val1'
    --   pred2 val2 -> val2'
    --   ...
    --   _ -> blame "..."
    -- @
    | CondE [(Expr, Expr)] String

    | FnDecl DefnKw String Expr
    | IdE QualName
    | IntE Int

    -- |
    -- This construct is not available in the parser
    -- @
    -- \x -> ...
    -- @
    | LambdaE String Expr

    -- info: initialization vals (1st argument) are sorted in Parser
    | MergeE [(QualName, Expr)]

    | RealE Double
    | WhereE Expr [Expr]
      deriving (Show)

isAppE :: Expr -> Bool
isAppE (AppE _ _) = True
isAppE _ = False

isCharE :: Expr -> Bool
isCharE (CharE _) = True
isCharE _ = False

isFnDecl :: Expr -> Bool
isFnDecl FnDecl {} = True
isFnDecl _ = False

isLambdaE :: Expr -> Bool
isLambdaE LambdaE {} = True
isLambdaE _ = False

isWhereE :: Expr -> Bool
isWhereE WhereE {} = True
isWhereE _ = False

isValueE :: Expr -> Bool
isValueE IdE {} = True
isValueE IntE {} = True
isValueE RealE {} = True
isValueE CharE {} = True
isValueE LambdaE {} = True
isValueE _ = False

andE :: Expr -> Expr -> Expr
andE expr1 expr2 =
    -- note: not using 'expr1' and 'expr2' directly in the Boolean
    -- expression in order to force them to have type 'Bool'.
    let
        err = "irrefutable 'and' pattern"
        m2 = (expr2, idE "true#")
        m3 = (idE "true#", idE "false#")
        m1 = (expr1, CondE [m2, m3] err)
    in
      CondE [m1, m3] err

appE :: String -> Expr -> Expr
appE str = AppE (idE str)

binOpE :: String -> Expr -> Expr -> Expr
binOpE op expr = AppE (appE op expr)

constE :: Expr -> Expr
constE = LambdaE "_"

constTrueE :: Expr
constTrueE = constE (idE "true#")

foldAppE :: Expr -> [Expr] -> Expr
foldAppE = foldr AppE

idE :: String -> Expr
idE = IdE . mkQualName . (:[])

intE :: Int -> Expr
intE n
  | n > 0 = IntE n
  | otherwise = appE "negInt" (IntE (- n))

orE :: Expr -> Expr -> Expr
orE expr1 expr2 =
    -- note: not using 'expr1' and 'expr2' directly in the Boolean
    -- expression in order to force them to have type 'Bool'.
    let
        err = "irrefutable 'or' pattern"
        m1 = (expr1, idE "true#")
        m2 = (expr2, idE "true#")
        m3 = (idE "true#", idE "false#")
    in
      CondE [m1, m2, m3] err

realE :: Double -> Expr
realE n
  | n > 0 = RealE n
  | otherwise = appE "negReal" (RealE (- n))

seqE :: [Expr] -> Expr
seqE [] = idE "null"
seqE (e:es) = AppE (appE "cons" e) (seqE es)

stringE :: String -> Expr
stringE str = seqE (map CharE str)

freeVarsList :: [String] -> [String] -> [Expr] -> ([String], [String])
freeVarsList env fvars [] = (env, fvars)
freeVarsList env fvars (x:xs) =
    let (env', fvars') = freeVars' env fvars x in
    freeVarsList env' fvars' xs

freeVars' :: [String] -> [String] -> Expr -> ([String], [String])
freeVars' env fvars (AppE expr1 expr2) =
    let (env', fvars') = freeVars' env fvars expr1 in
    freeVars' env' fvars' expr2
freeVars' env fvars (CharE _) = (env, fvars)
freeVars' env fvars (CondE ms _) =
    loop env fvars ms
    where loop env fvars [] = (env, fvars)
          loop env fvars ((expr1, expr2):exprs) =
              let
                  (env', fvars') = freeVars' env fvars expr1
                  (env'', fvars'') = freeVars' env' fvars' expr2
              in
                loop env'' fvars'' exprs
freeVars' env fvars (FnDecl Def name expr) =
    freeVars' (name:env) fvars expr
freeVars' env fvars (FnDecl NrDef name expr) =
    let (env', fvars') = freeVars' env fvars expr in
    (name:env', fvars')
freeVars' env fvars (IdE name)
    | fromQualName name `elem` env = (env, fvars)
    | otherwise = (env, fromQualName name:fvars)
freeVars' env fvars (IntE _) = (env, fvars)
freeVars' env fvars (LambdaE arg body) =
    freeVars' (arg:env) fvars body
freeVars' env fvars (MergeE vals) =
    loop env fvars vals
    where loop env fvars [] = (env, fvars)
          loop env fvars ((_, expr):vals) =
              let (env', fvars') = freeVars' env fvars expr in
              loop env' fvars' vals
freeVars' env fvars (RealE _) = (env, fvars)
freeVars' env fvars (WhereE expr exprs) =
    let (env', fvars') = loop env fvars exprs in
    freeVars' env' fvars' expr
    where loop env fvars [] = (env, fvars)
          loop env fvars (expr:exprs) =
              let (env', fvars') = freeVars' env fvars expr in
              loop env' fvars' exprs

freeVars :: Expr -> [String]
freeVars = List.nub . snd . freeVars' [] []
