module Data.Expr where

import qualified Data.List as List (nub)

import Data.Literal (Literal(..))
import Data.Name (Name)
import qualified Data.Name as Name
import Typechecker.Type (Type)

data DefnKw
  = Def | NrDef

data Expr
  = AnnotationE Expr Type
  | AppE Expr Expr

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

  -- TODO: document
  | LiteralE Literal

isAppE :: Expr -> Bool
isAppE AppE {} = True
isAppE _ = False

isCharE :: Expr -> Bool
isCharE (LiteralE CharL {}) = True
isCharE _ = False

isFnDecl :: Expr -> Bool
isFnDecl FnDecl {} = True
isFnDecl _ = False

isLambdaE :: Expr -> Bool
isLambdaE LambdaE {} = True
isLambdaE _ = False

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

foldAppE :: Expr -> [Expr] -> Expr
foldAppE = foldl AppE

appToList :: Expr -> [Expr]
appToList (AppE fn arg) = appToList fn ++ [arg]
appToList x = [x]

binOpE :: Name -> Expr -> Expr -> Expr
binOpE op expr = AppE (appE op expr)

constE :: Expr -> Expr
constE = LambdaE (Name.untyped "_")

constTrueE :: Expr
constTrueE = constE trueE

idE :: String -> Expr
idE = IdE . Name.untyped

charE :: Char -> Expr
charE = LiteralE . CharL

intE :: Int -> Expr
intE n
  | n >= 0 = LiteralE $ IntL n
  | otherwise = appE negInt . LiteralE . IntL $ -n
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
  | n >= 0 = LiteralE $ RealL n
  | otherwise = appE negReal . LiteralE . RealL $ -n
  where
    negReal = Name.untyped "negReal"

seqE :: [Expr] -> Expr
seqE [] = idE "null"
seqE (e:es) = AppE (appE consE e) (seqE es)
  where
    consE = Name.untyped "cons"

stringE :: String -> Expr
stringE = LiteralE . StringL

tupleE :: [Expr] -> Expr
tupleE [] = idE "unit"
tupleE exprs = foldAppE (IdE . mkTupleName $ length exprs) exprs
  where
    mkTupleName 0 = error "mkTupleName undefined for length 0"
    mkTupleName 1 = error "mkTupleName undefined for length 1"
    mkTupleName len = Name.untyped $ "mkTuple" ++ show len

freeVars :: Expr -> [Name]
freeVars = List.nub . snd . free [] []
  where
    freeMatches env fvars [] =
      (env, fvars)
    freeMatches env fvars ((expr1, expr2):exprs) =
      let
        (env', fvars') = free env fvars expr1
        (env'', fvars'') = free env' fvars' expr2
      in
       freeMatches env'' fvars'' exprs

    free :: [Name] -> [Name] -> Expr -> ([Name], [Name])
    free env fvars (AnnotationE expr _) =
      free env fvars expr
    free env fvars (AppE expr1 expr2) =
      let (env', fvars') = free env fvars expr1 in
      free env' fvars' expr2
    free env fvars (CondE matches _) =
      freeMatches env fvars matches
    free env fvars (FnDecl Def name expr) =
      free (name:env) fvars expr
    free env fvars (FnDecl NrDef name expr) =
      let (env', fvars') = free env fvars expr in
      (name:env', fvars')
    free env fvars (IdE name)
      | name `elem` env = (env, fvars)
      | otherwise = (env, name:fvars)
    free env fvars (LambdaE arg body) =
      free (arg:env) fvars body
    free env fvars (LetE defn body) =
      let (env', fvars') = free env fvars defn in
      free env' fvars' body
    free env fvars LiteralE {} =
      (env, fvars)
