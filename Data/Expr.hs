module Data.Expr where

import Data.List (nub, sort)

import Data.QualName
import Data.Type
import Utils


type PatDefn = (String, [Expr])


data Pat
  = Pat { patPred :: Expr
        , patDefns :: [PatDefn] }
    deriving (Show)


mkGeneralPat :: Expr -> [[Expr]] -> [Pat] -> Pat
mkGeneralPat pred mods pats =
    Pat (mkPred pred pats) (modPats mods pats)
    where mkPred pred [] = pred
          mkPred pred pats =
              AppE pred $ SeqE $ map patPred pats

          modDefns _ [] = []
          modDefns mod ((str, mod'):defns) =
              (str, mod' ++ mod):modDefns mod defns

          modPats [] [] = []    
          modPats (mod:mods) (Pat _ defns:pats) =
              modDefns mod defns ++ modPats mods pats


mkPat :: Expr -> [Expr] -> [Pat] -> Pat
mkPat pred mods = mkGeneralPat pred (map (:[]) mods)


mkPredPat :: Expr -> Pat
mkPredPat pred = mkGeneralPat pred [] []


-- edit: document why this function has a special case for the empty
-- list
--
-- edit: plist already checks the lenght of the list, however, it
-- might be better to generate end of list pattern
-- @
-- nrdef [] = tail (... (tail xs)...)
-- @
mkListPat :: [Pat] -> Pat
mkListPat [] =
  Pat (appE "plist" (SeqE [])) []

mkListPat pats =
  mkGeneralPat (idE "plist") (map (reverse . listRef) [1..length pats]) pats
  where listRef 1 = [idE "hd"]
        listRef i = idE "tl":listRef (i - 1)


namePat :: String -> Pat -> Pat
namePat name (Pat pred defns) =
    Pat pred $ (name, []):defns


data DefnKw
  = Def | NrDef
    deriving (Show)


mkQualName :: [String] -> QualName
mkQualName name =
  QualName { fromQualName = flattenId name }


type Observation = (String, Type)


data Expr
    = IdE QualName

    | IntE Int
    | RealE Double
    | CharE Char
    | SeqE [Expr]

    | AppE Expr Expr
    | CastE Expr Type

    -- |
    -- This construct is not available in the parser
    -- @
    -- x@Int y@isInt = val1
    -- x@Int y@isReal = val2
    --  @     @ = blame "..."
    -- @
    | CondMacro [([Pat], Expr)] String

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

    -- info: observations (1st argument) are sorted in Parser
    | CotypeDecl Type

    | FnDecl (Maybe Type) DefnKw String Expr

    | LambdaMacro [Pat] Expr
    | LambdaE String (Maybe String) Expr

    -- info: initialization vals (1st argument) are sorted in Parser
    | MergeE [(QualName, Expr)]

    | WhereE Expr [Expr]
      deriving (Show)


isCharE :: Expr -> Bool
isCharE (CharE _) = True
isCharE _ = False


isAppE :: Expr -> Bool
isAppE (AppE _ _) = True
isAppE _ = False


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
isValueE SeqE {} = True
isValueE LambdaE {} = True
isValueE (CastE expr _) = isValueE expr
isValueE _ = False


andE :: Expr -> Expr -> Expr
andE expr1 expr2 =
    -- note: not using 'expr1' and 'expr2' directly in the Boolean
    -- expression in order to force them to have type 'Bool'.
    let
        err = "irrefutable 'and' pattern"
        m2 = (expr2, idE "true")
        m3 = (idE "true", idE "false")
        m1 = (expr1, CondE [m2, m3] err)
    in
      CondE [m1, m3] err


idE :: String -> Expr
idE = IdE . mkQualName . (:[])


appE :: String -> Expr -> Expr
appE str = AppE (idE str)


binOpE :: String -> Expr -> Expr -> Expr
binOpE op expr = AppE (appE op expr)


constE :: Expr -> Expr
constE = LambdaE "_" Nothing
-- edit: maybe it should be TvarT or Evar instead of Nothing?


constTrueE :: Expr
constTrueE = constE (idE "true")


foldAppE :: Expr -> [Expr] -> Expr
foldAppE = foldr AppE


orE :: Expr -> Expr -> Expr
orE expr1 expr2 =
    -- note: not using 'expr1' and 'expr2' directly in the Boolean
    -- expression in order to force them to have type 'Bool'.
    let
        err = "irrefutable 'or' pattern"
        m1 = (expr1, idE "true")
        m2 = (expr2, idE "true")
        m3 = (idE "true", idE "false")
    in
      CondE [m1, m2, m3] err


signalE :: String -> String -> Expr -> Expr
signalE id str val =
    appE "signal" (SeqE [stringE id, stringE str, val])


stringE :: String -> Expr
stringE str = SeqE $ map CharE str


freeVarsList :: [String] -> [String] -> [Expr] -> ([String], [String])
freeVarsList env fvars [] = (env, fvars)
freeVarsList env fvars (x:xs) =
    let (env', fvars') = freeVars' env fvars x in
    freeVarsList env' fvars' xs


freeVarsPat :: [String] -> [String] -> Pat -> ([String], [String])
freeVarsPat env fvars pat =
    let (env', fvars') = freeVars' env fvars (patPred pat) in
    freeVarsList (env' ++ map fst (patDefns pat)) fvars' (concatMap snd (patDefns pat))


freeVarsPats :: [String] -> [String] -> [Pat] -> ([String], [String])
freeVarsPats env fvars [] = (env, fvars)
freeVarsPats env fvars (pat:pats) =
    let (env', fvars') = freeVarsPat env fvars pat in
    freeVarsPats env' fvars' pats


freeVars' :: [String] -> [String] -> Expr -> ([String], [String])
freeVars' env fvars (IdE name)
    | fromQualName name `elem` env = (env, fvars)
    | otherwise = (env, fromQualName name:fvars)

freeVars' env fvars (IntE _) = (env, fvars)
freeVars' env fvars (RealE _) = (env, fvars)
freeVars' env fvars (CharE _) = (env, fvars)

freeVars' env fvars (SeqE exprs) =
    loop env fvars exprs
    where loop env fvars [] = (env, fvars)
          loop env fvars (expr:exprs) =
              let (env', fvars') = freeVars' env fvars expr in
              loop env' fvars' exprs

freeVars' env fvars (AppE expr1 expr2) =
    let (env', fvars') = freeVars' env fvars expr1 in
    freeVars' env' fvars' expr2

freeVars' env fvars (CondMacro ms _) =
    loop env fvars ms
    where loop env fvars [] = (env, fvars)
          loop env fvars ((pats, expr):ms) =
              let
                  (env', fvars') = freeVarsPats env fvars pats
                  (env'', fvars'') = freeVars' env' fvars' expr
              in
                loop env'' fvars'' ms
              

freeVars' env fvars (CondE ms _) =
    loop env fvars ms
    where loop env fvars [] = (env, fvars)
          loop env fvars ((expr1, expr2):exprs) =
              let
                  (env', fvars') = freeVars' env fvars expr1
                  (env'', fvars'') = freeVars' env' fvars' expr2
              in
                loop env'' fvars'' exprs

freeVars' env fvars (FnDecl _ Def name expr) =
    freeVars' (name:env) fvars expr

freeVars' env fvars (FnDecl _ NrDef name expr) =
    let (env', fvars') = freeVars' env fvars expr in
    (name:env', fvars')

freeVars' _ _ LambdaMacro {} =
    error "freeVars'(LambdaMacro): not implemented"

freeVars' env fvars (LambdaE arg _ body) =
    freeVars' (arg:env) fvars body

freeVars' env fvars (MergeE vals) =
    loop env fvars vals
    where loop env fvars [] = (env, fvars)
          loop env fvars ((_, expr):vals) =
              let (env', fvars') = freeVars' env fvars expr in
              loop env' fvars' vals

freeVars' env fvars (WhereE expr exprs) =
    let (env', fvars') = loop env fvars exprs in
    freeVars' env' fvars' expr
    where loop env fvars [] = (env, fvars)
          loop env fvars (expr:exprs) =
              let (env', fvars') = freeVars' env fvars expr in
              loop env' fvars' exprs

freeVars' _ _ _ =
    error "freeVars': unhandled case"


freeVars :: Expr -> [String]
freeVars = nub . sort . snd . freeVars' [] []
