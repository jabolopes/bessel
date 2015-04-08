module Data.Expr where

import qualified Data.List as List (nub)

import Data.QualName
import qualified Data.QualName as QualName

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

  -- |
  -- @
  -- let fn ...
  -- @
  | FnDecl DefnKw QualName Expr

  | IdE QualName
  | IntE Int

  -- |
  -- This construct is not available in the parser
  -- @
  -- \x -> ...
  -- @
  | LambdaE QualName Expr

  -- |
  -- @
  -- let fn x y = ... in ...
  -- @
  | LetE Expr Expr

  | RealE Double

isAppE :: Expr -> Bool
isAppE AppE {} = True
isAppE _ = False

isCharE :: Expr -> Bool
isCharE CharE {} = True
isCharE _ = False

isFnDecl :: Expr -> Bool
isFnDecl FnDecl {} = True
isFnDecl _ = False

isLambdaE :: Expr -> Bool
isLambdaE LambdaE {} = True
isLambdaE _ = False

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
constE = LambdaE $ QualName.unqualified "_"

constTrueE :: Expr
constTrueE = constE (idE "true#")

foldAppE :: Expr -> [Expr] -> Expr
foldAppE = foldr AppE

idE :: String -> Expr
idE = IdE . qualified

intE :: Int -> Expr
intE n
  | n >= 0 = IntE n
  | otherwise = appE "negInt" (IntE (- n))

letE :: [Expr] -> Expr -> Expr
letE defs expr =
  foldr LetE expr defs

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
  | n >= 0 = RealE n
  | otherwise = appE "negReal" (RealE (- n))

seqE :: [Expr] -> Expr
seqE [] = idE "null"
seqE (e:es) = AppE (appE "cons" e) (seqE es)

stringE :: String -> Expr
stringE str = seqE (map CharE str)

freeVars :: Expr -> [String]
freeVars = map QualName.fromQualName . List.nub . snd . freeVars' [] []
  where
    freeVars' :: [QualName] -> [QualName] -> Expr -> ([QualName], [QualName])
    freeVars' env fvars (AppE expr1 expr2) =
      let (env', fvars') = freeVars' env fvars expr1 in
      freeVars' env' fvars' expr2
    freeVars' env fvars CharE {} =
      (env, fvars)
    freeVars' env fvars (CondE ms _) =
      loop env fvars ms
      where
        loop env fvars [] =
          (env, fvars)
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
      | name `elem` env = (env, fvars)
      | otherwise = (env, name:fvars)
    freeVars' env fvars IntE {} =
      (env, fvars)
    freeVars' env fvars (LetE defn body) =
      let (env', fvars') = freeVars' env fvars defn in
      freeVars' env' fvars' body
    freeVars' env fvars (LambdaE arg body) =
      freeVars' (arg:env) fvars body
    freeVars' env fvars RealE {} =
      (env, fvars)
