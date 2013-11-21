{-# LANGUAGE ParallelListComp, TupleSections #-}
module Stage.Expander where

import Prelude hiding (mod)

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Error.Class (throwError)

import Data.Expr (DefnKw(..), Expr(..))
import qualified Data.Expr as Expr
import Data.Macro
import Monad.NameM (NameM)
import qualified Monad.NameM as NameM
import Data.PrettyString (PrettyString)
import qualified Data.PrettyString as PrettyString (text)
import qualified Renamer as Renamer
import Pretty.Stage.Expander as Pretty

type ExpanderM a = NameM (Either PrettyString) a

-- expand CondM

genPatNames :: [Pat] -> ExpanderM [String]
genPatNames = mapM (genName . patBinder)
  where genName name
          | null name = NameM.genNameM "arg"
          | otherwise = NameM.genNameM name

-- | Generates a function definition for a pattern, given its @name@
-- and @mods@
--
-- @
-- x@
-- @
--
-- @
-- def x = ...
-- @
patFnDecl :: Expr -> String -> [Expr] -> Expr
patFnDecl val binder mods =
  FnDecl NrDef binder (Expr.foldAppE val mods)

-- | Generates function definitions for a pattern's bindings
--
-- @
-- xs@[x@, y@]
-- @
--
-- @
-- def xs = x1#
-- def x = hd x1#
-- def y = hd (tl x1#)
-- @
patDefns :: Expr -> Pat -> [Expr]
patDefns val pat = patDefnsMods pat []
  where patDefnsMods pat mods
          | null (patBinder pat) = patDefns' (patGuard pat)
          | otherwise = patFnDecl val (patBinder pat) mods:patDefns' (patGuard pat)
          where patDefns' AllPG = []
                patDefns' PredicatePG {} = []
                patDefns' (ListPG hdPat tlPat) =
                  patDefnsMods hdPat (Expr.idE "hd":mods) ++
                  patDefnsMods tlPat (Expr.idE "tl":mods)
                patDefns' (TuplePG pats) =
                  concat [ patDefnsMods pat (mod ++ mods) | pat <- pats | mod <- patMods ]
                  where listRef 1 = [Expr.idE "hd"]
                        listRef i = Expr.idE "tl":listRef (i - 1)
                        patMods = map (reverse . listRef) [1..length pats]

patConstantPred :: Macro -> (String, String)
patConstantPred CharM {} = ("isChar#", "eqChar#")
patConstantPred IntM {} = ("isInt#", "eqInt#")
patConstantPred RealM {} = ("isReal#", "eqReal#")

-- | Generates a predicate for a pattern, according the predicate
-- guard
--
-- @
-- x@
-- x@isInt
-- (x@ +> xs@)
-- x@[@, @]
-- x@1
-- x@"ola"
-- @
--
-- @
-- const true
-- isInt
-- isList (const true) (const true)
-- isTuple [const true, const true]
-- isInt && eqInt 1
-- isTuple [isChar, isChar, isChar]
-- @
patPred :: Pat -> ExpanderM Expr
patPred = patPred' . patGuard
  where patPred' AllPG = return Expr.constTrueE

        patPred' (PredicatePG pred@(IdM _)) = expandMacro pred

        patPred' (PredicatePG (StringM cs)) =
          patPred' . TuplePG . map (predicatePat . CharM) $ cs

        patPred' (PredicatePG val) =
          do let (isFn, eqFn) = patConstantPred val
             arg <- NameM.genNameM "arg"
             let argId = idM arg
                 pred = AndM (appM isFn argId) ((appM eqFn val) `AppM` argId)
             LambdaE arg <$> expandMacro pred

        patPred' (ListPG hdPat tlPat) =
          do hdPred <- patPred hdPat
             tlPred <- patPred tlPat
             return $ Expr.appE "isList" hdPred `AppE` tlPred
        patPred' (TuplePG pats) =
          Expr.appE "isTuple" . Expr.seqE <$> mapM patPred pats

-- |
-- @
-- x@ y@ = body
-- @
--
-- @
-- body where {
--   def x = ...
--   def y = ...
-- }
-- @
expandMatchBody :: [String] -> [Pat] -> Macro -> ExpanderM Expr
expandMatchBody args pats body =
  case concat [ patDefns (Expr.idE arg) pat | arg <- args | pat <- pats ] of
    [] -> expandMacro body
    defns ->
      do body' <- expandMacro body
         return (WhereE body' defns)

-- |
-- @
-- x@pat1 y@pat1' -> body1
-- x@pat2 y@pat2' -> body2
-- @
--
-- @
-- \x# ->
--   \y# ->
--     cond
--       pred1 x# && pred2 y# -> body1
--       pred1 x# && pred2 y# -> body2
--       _ _ -> blame "..."
--     where {
--       def x = ...
--       def y = ...
--     }
-- @
--
-- The above generated code is a simplification because '&&' is
-- encoded through 'CondE'.
expandMatch :: [String] -> [Pat] -> Macro -> ExpanderM (Expr, Expr)
expandMatch args pats body =
  (,) <$> andPred <*> expandMatchBody args pats body
  where applyPred arg pat =
          do pred <- patPred pat
             return . AppE pred . Expr.idE $ arg

        andPred =
          foldl1 Expr.andE <$> sequence [ applyPred arg pat | arg <- args | pat <- pats ]

expandMatches :: [String] -> [([Pat], Macro)] -> ExpanderM [(Expr, Expr)]
expandMatches _ [] = return []
expandMatches args ((pats, body):ms) =
  (:) <$> expandMatch args pats body <*> expandMatches args ms

expandCond :: [([Pat], Macro)] -> String -> ExpanderM Expr
expandCond ms blame =
  do let pats = head . fst . unzip $ ms
     case checkMatches (length pats) ms of
       Left err -> throwError err
       Right () ->
         do args <- genPatNames pats
            ms' <- expandMatches args ms
            return . lambdas args . CondE ms' $ blame
  where checkMatches :: Int -> [([Pat], Macro)] -> Either PrettyString ()
        checkMatches _ [] = Right ()
        checkMatches n ((pats, _):ms)
          | length pats == n = checkMatches n ms
          | otherwise = Left (Pretty.condMatchPatternsMismatch undefined)

        lambdas [] body = body
        lambdas (arg:args) body =
          LambdaE arg (lambdas args body)

-- /expand CondM

-- expand FnDeclM

-- |
-- @
-- def fn : ...
--   x@ = ... fn ...
-- @
--
-- @
-- def fn : ...
--   = fix# (gen#@ x@ = ... gen# ...)
-- @
fixDecl :: String -> Expr -> Either String Expr
fixDecl name body =
  do FnDecl _ name' body' <- Renamer.renameDeclaration (FnDecl Def name body)
     let argBody = LambdaE name' body'
         declBody = Expr.appE "fix#" argBody
     return $ FnDecl NrDef name declBody

expandFnDecl :: String -> Expr -> ExpanderM Expr
expandFnDecl name body =
  do if name `elem` Expr.freeVars body
     then
       case fixDecl name body of
         Left err -> throwError (PrettyString.text err)
         Right expr -> return expr
     else
       return $ FnDecl NrDef name body

-- /expand FnDeclM

expandSeq :: [Macro] -> ExpanderM Expr
expandSeq ms = Expr.seqE <$> mapM expandMacro ms

expandString :: String -> Expr
expandString = Expr.stringE

expandMacro :: Macro -> ExpanderM Expr
expandMacro (AndM m1 m2) = Expr.andE <$> expandMacro m1 <*> expandMacro m2
expandMacro (AppM m1 m2) = AppE <$> expandMacro m1 <*> expandMacro m2
expandMacro (BinOpM op m1 m2) = Expr.binOpE op <$> expandMacro m1 <*> expandMacro m2
expandMacro (CharM c) = return (CharE c)
expandMacro (CondM ms) = expandCond ms "lambda"
expandMacro (FnDeclM name (CondM ms)) = expandFnDecl name =<< expandCond ms name
expandMacro (FnDeclM name body) = expandFnDecl name =<< expandMacro body
expandMacro (IdM name) = return (Expr.IdE name)
expandMacro (IntM n) = return (Expr.intE n)
expandMacro (ModuleM me _ _) = throwError (Pretty.devModuleNested me)
expandMacro (OrM m1 m2) = Expr.orE <$> expandMacro m1 <*> expandMacro m2
expandMacro (RealM n) = return (Expr.realE n)
expandMacro (SeqM ms) = expandSeq ms
expandMacro (StringM str) = return (expandString str)
expandMacro (WhereM m ms) = WhereE <$> expandMacro m <*> mapM expandMacro ms

expand :: Macro -> Either PrettyString Expr
expand m = fst <$> NameM.runNameM (expandMacro m) NameM.initialNameState
