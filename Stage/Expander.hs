{-# LANGUAGE ParallelListComp, TupleSections #-}
module Stage.Expander where

import Prelude hiding (mod)

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Error.Class (throwError)

import Data.Expr (DefnKw(..), Expr(..))
import qualified Data.Expr as Expr
import Data.QualName (QualName)
import qualified Data.QualName as QualName (fromQualName, isTypeName)
import Data.PrettyString (PrettyString)
import qualified Data.PrettyString as PrettyString (text)
import Data.Source
import Monad.NameM (NameM)
import qualified Monad.NameM as NameM
import qualified Renamer as Renamer
import Pretty.Stage.Expander as Pretty
import Pretty.Data.Source as Pretty

type ExpanderM a = NameM (Either PrettyString) a

-- expand CondM

genPatNames :: [Source] -> ExpanderM [String]
genPatNames = mapM genPatName
  where genName name
          | null name = NameM.genNameM "arg"
          | otherwise = NameM.genNameM name

        genPatName (PatS binder _) = genName binder
        genPatName _ = genName ""

-- | Generates a function definition for a pattern, given the
-- pattern's @binder@ and @mods@
--
-- @
-- x@
-- @
--
-- @
-- def x = ...
-- @
patFnDecl :: String -> [Expr] -> Expr -> Expr
patFnDecl binder mods val =
  FnDecl NrDef binder (Expr.foldAppE val mods)

isTypeGuard :: Source -> Bool
isTypeGuard (AppS fn _) = isTypeGuard fn
isTypeGuard (IdS name) = QualName.isTypeName $ QualName.fromQualName name
isTypeGuard _ = False

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
patDefns :: Expr -> Source -> [Expr]
patDefns val = sourceDefns []
  where sourceDefns mods src
          | isTypeGuard src =
            concatMap (sourceDefns (Expr.idE "unCons#":mods)) $ tail . appToList $ src
        sourceDefns mods (BinOpS "+>" hdPat tlPat) =
          sourceDefns (Expr.idE "hd":mods) hdPat ++
          sourceDefns (Expr.idE "tl":mods) tlPat
        sourceDefns mods (PatS binder guard) =
          hdDefn ++ tlDefn
          where hdDefn =
                  case binder of
                    "" -> []
                    _ -> (:[]) $ patFnDecl binder mods val

                tlDefn =
                  case guard of
                    Nothing -> []
                    Just src -> sourceDefns mods src
        sourceDefns mods (SeqS pats) =
          concat [ sourceDefns (mod ++ mods) pat | pat <- pats | mod <- patMods ]
          where listRef 1 = [Expr.idE "hd"]
                listRef i = Expr.idE "tl":listRef (i - 1)
                patMods = map (reverse . listRef) [1..length pats]
        sourceDefns _ _ = []

patConstantPred :: Source -> (String, String)
patConstantPred CharS {} = ("isChar#", "eqChar#")
patConstantPred IntS {} = ("isInt#", "eqInt#")
patConstantPred RealS {} = ("isReal#", "eqReal#")

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
patPred :: Source -> ExpanderM Expr        
patPred = sourcePred
  where sourcePred src
          | isTypeGuard src =
            let IdS qualName = head . appToList $ src in
            return . Expr.idE . consIsName $ qualName
        sourcePred (BinOpS "+>" hdPat tlPat) =
          do hdPred <- sourcePred hdPat
             tlPred <- sourcePred tlPat
             return $ Expr.appE "isList" hdPred `AppE` tlPred
        sourcePred pred@(IdS _) =
          expandOne pred
        sourcePred (PatS _ Nothing) =
          return Expr.constTrueE
        sourcePred (PatS _ (Just src)) =
          patPred src
        sourcePred (SeqS pats) =
          Expr.appE "isTuple" . Expr.seqE <$> mapM sourcePred pats
        sourcePred (StringS cs) =
          sourcePred . SeqS . map CharS $ cs
        sourcePred m =
          do let (isFn, eqFn) = patConstantPred m
             arg <- NameM.genNameM "arg"
             let argId = idS arg
                 pred = AndS (appS isFn argId) ((appS eqFn m) `AppS` argId)
             LambdaE arg <$> expandOne pred

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
expandMatchBody :: [String] -> [Source] -> Source -> ExpanderM Expr
expandMatchBody args pats body =
  case concat [ patDefns (Expr.idE arg) pat | arg <- args | pat <- pats ] of
    [] -> expandOne body
    defns ->
      do body' <- expandOne body
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
expandMatch :: [String] -> [Source] -> Source -> ExpanderM (Expr, Expr)
expandMatch args pats body =
  (,) <$> andPred <*> expandMatchBody args pats body
  where applyPred arg pat =
          do pred <- patPred pat
             return . AppE pred . Expr.idE $ arg

        andPred =
          foldl1 Expr.andE <$> sequence [ applyPred arg pat | arg <- args | pat <- pats ]

expandMatches :: [String] -> [([Source], Source)] -> ExpanderM [(Expr, Expr)]
expandMatches names = loop
  where loop [] = return []
        loop ((pats, body):ms) =
          (:) <$> expandMatch names pats body <*> loop ms

expandCond :: [([Source], Source)] -> String -> ExpanderM Expr
expandCond ms blame =
  do let args = head . fst . unzip $ ms
     case checkMatches (length args) ms of
       Left err -> throwError err
       Right () ->
         do argNames <- genPatNames args
            ms' <- expandMatches argNames ms
            return . lambdas argNames . CondE ms' $ blame
  where checkMatches _ [] = Right ()
        checkMatches n ((args, _):ms')
          | length args == n = checkMatches n ms'
          | otherwise = Left . Pretty.condMatchPatternsMismatch . Pretty.docCond $ ms

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

expandSeq :: [Source] -> ExpanderM Expr
expandSeq ms = Expr.seqE <$> mapM expandOne ms

expandString :: String -> Expr
expandString = Expr.stringE

-- expand TypeDeclM

consIsName :: QualName -> String
consIsName consName =
  "is" ++ (QualName.fromQualName consName)

consMkName :: QualName -> String
consMkName = QualName.fromQualName

consPredFn :: QualName -> Source
consPredFn consName =
  let
    consNameStr = QualName.fromQualName consName
    body = appS "isCons#" . appS "link#" . StringS $ consNameStr
  in
    FnDeclS (consIsName consName) body

typePredFn :: QualName -> [QualName] -> Source
typePredFn typeName consNames =
  let
    fnName = consIsName typeName
    predNames = map (idS . consIsName) consNames
    body = foldl1 OrS predNames
  in
    FnDeclS fnName body

consConsFn :: QualName -> String -> Source -> Source
consConsFn consName binder guard =
  let
    consNameStr = QualName.fromQualName consName
    body = appS "mkCons#" . appS "link#" . StringS $ consNameStr
  in
    FnDeclS (consMkName consName) $
      CondS [([PatS binder (Just guard)], body `AppS` idS binder)]

expandConstructor :: (QualName, Source) -> ExpanderM [Expr]
expandConstructor (consName, pat) =
  do binder <- patBinder pat
     let guard = patGuard pat
         predSource = consPredFn consName
         consSource = consConsFn consName binder guard
     (++) <$> expandSource predSource <*> expandSource consSource
  where patBinder (PatS binder _) = return binder
        patBinder _               = NameM.genNameM "arg"
        
        patGuard (PatS _ (Just src)) = patGuard src
        patGuard src = src

expandTypeDecl :: QualName -> [(QualName, Source)] -> ExpanderM [Expr]
expandTypeDecl typeName cons =
  do consFns <- concat <$> mapM expandConstructor cons
     typeFn <- expandSource $ typePredFn typeName $ map fst cons
     return $ consFns ++ typeFn

-- /expand TypeDeclM

expandOne :: Source -> ExpanderM Expr
expandOne macro = head <$> expandSource macro

returnOne :: ExpanderM Expr -> ExpanderM [Expr]
returnOne m = (:[]) <$> m

expandSource :: Source -> ExpanderM [Expr]
expandSource (AndS m1 m2) =
  returnOne $ Expr.andE <$> expandOne m1 <*> expandOne m2
expandSource (AppS m1 m2) =
  returnOne $ AppE <$> expandOne m1 <*> expandOne m2
expandSource (BinOpS op m1 m2) =
  returnOne $ Expr.binOpE op <$> expandOne m1 <*> expandOne m2
expandSource (CharS c) =
  returnOne . return . CharE $ c
expandSource (CondS ms) =
  returnOne $ expandCond ms "lambda"
expandSource (FnDeclS name (CondS ms)) =
  returnOne $ expandFnDecl name =<< expandCond ms name
expandSource (FnDeclS name body) =
  returnOne $ expandFnDecl name =<< expandOne body
expandSource (IdS name) =
  returnOne . return . Expr.IdE $ name
expandSource (IntS n) =
  returnOne . return . Expr.intE $ n
expandSource (ModuleS me _ _) =
  throwError (Pretty.devModuleNested me)
expandSource (OrS m1 m2) =
  returnOne $ Expr.orE <$> expandOne m1 <*> expandOne m2
expandSource pat@PatS {} =
  throwError . Pretty.devPattern . Pretty.docSource $ pat
expandSource (RealS n) =
  returnOne . return . Expr.realE $ n
expandSource (SeqS ms) =
  returnOne $ expandSeq ms
expandSource (StringS str) =
  returnOne . return . expandString $ str
expandSource (TypeDeclS typeName cons) =
  expandTypeDecl typeName cons
expandSource (WhereS src srcs) =
  returnOne $ WhereE <$> expandOne src <*> (concat <$> mapM expandSource srcs)

expand :: Source -> Either PrettyString [Expr]
expand src = fst <$> NameM.runNameM (expandSource src) NameM.initialNameState
