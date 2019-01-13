module Data.Expr where

import qualified Data.List as List (nub)

import Data.Name (Name)
import qualified Data.Name as Name
import Typechecker.Type (Type)

data DefnKw
  = Def | NrDef

data Expr
  = AnnotationE Expr Type
  | AppE Expr Expr
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
  | FnDecl DefnKw Name Expr

  | IdE Name
  | IntE Int

  -- |
  -- This construct is not available in the parser
  -- @
  -- \x -> ...
  -- @
  | LambdaE Name Expr

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

falseE :: Expr
falseE = idE "false#"

trueE :: Expr
trueE = idE "true#"

andE :: Expr -> Expr -> Expr
andE expr1 expr2 =
  -- note: not using 'expr1' and 'expr2' directly in the Boolean
  -- expression in order to force them to have type 'Bool'.
  let
    err = "irrefutable 'and' pattern"
    m2 = (expr2, trueE)
    m3 = (trueE, falseE)
    m1 = (expr1, CondE [m2, m3] err)
  in
   CondE [m1, m3] err

appE :: Name -> Expr -> Expr
appE name = AppE (IdE name)

binOpE :: Name -> Expr -> Expr -> Expr
binOpE op expr = AppE (appE op expr)

constE :: Expr -> Expr
constE = LambdaE (Name.untyped "_")

constTrueE :: Expr
constTrueE = constE trueE

foldAppE :: Expr -> [Expr] -> Expr
foldAppE = foldl AppE

idE :: String -> Expr
idE = IdE . Name.untyped

intE :: Int -> Expr
intE n
  | n >= 0 = IntE n
  | otherwise = appE negInt (IntE (- n))
  where
    negInt = Name.untyped "negInt"

letE :: [Expr] -> Expr -> Expr
letE defs expr =
  foldr LetE expr defs

orE :: Expr -> Expr -> Expr
orE expr1 expr2 =
    -- note: not using 'expr1' and 'expr2' directly in the Boolean
    -- expression in order to force them to have type 'Bool'.
    let
        err = "irrefutable 'or' pattern"
        m1 = (expr1, trueE)
        m2 = (expr2, trueE)
        m3 = (trueE, falseE)
    in
      CondE [m1, m2, m3] err

realE :: Double -> Expr
realE n
  | n >= 0 = RealE n
  | otherwise = appE negReal (RealE (- n))
  where
    negReal = Name.untyped "negReal"

seqE :: [Expr] -> Expr
seqE [] = idE "null"
seqE (e:es) = AppE (appE consE e) (seqE es)
  where
    consE = Name.untyped "cons"

stringE :: String -> Expr
stringE str = seqE (map CharE str)

freeVars :: Expr -> [Name]
freeVars = List.nub . snd . freeVars' [] []
  where
    freeVars' :: [Name] -> [Name] -> Expr -> ([Name], [Name])
    freeVars' env fvars (AnnotationE expr _) =
      freeVars' env fvars expr
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
