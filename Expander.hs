{-# LANGUAGE ParallelListComp, TupleSections #-}
module Expander where

import Control.Monad.Error (throwError)
import Control.Monad.State
import Data.Functor ((<$>))
import Data.List (intercalate, isPrefixOf, maximumBy, nub, partition, sort)

import Data.Definition (Definition(..))
import Data.FileSystem (FileSystem)
import qualified Data.FileSystem as FileSystem
import Data.SrcFile (SrcFileT(..), SrcFile(..))
import qualified Data.SrcFile as SrcFile
import Data.Expr
import qualified Data.Expr as Expr (appE, freeVars, idE)
import Data.QualName
import qualified Data.QualName as QualName (fromQualName)
import Data.Type
import Renamer (renameDeclaration)


data ExpanderState =
    ExpanderState { macroCount :: Int }

initialExpanderState =
    ExpanderState { macroCount = 0 }


type ExpanderM a = StateT ExpanderState (Either String) a


genNameM :: String -> ExpanderM String
genNameM name =
    do c <- macroCount <$> get
       modify $ \s -> s { macroCount = macroCount s + 1 }
       return (name ++ "#" ++ show c)


-- cond macros

-- |
-- Generates a function definition for a pattern.
--
-- @
-- xs@[x@, y@]
-- @
--
-- @
-- def x = head xs
-- def y = head (tail xs)
-- @
mkPatDefns :: Expr -> [PatDefn] -> [Expr]
mkPatDefns val = map (mkDefn `uncurry`)
    where mkDefn name mods =
              FnDecl Nothing NrDef name (foldAppE val mods)


-- |
-- @
-- x@pat y@pat = body
-- @
--
-- @
-- body where { def x = ...
--              def y = ... }
-- @
lambdaBody :: [String] -> [Pat] -> Expr -> Expr
lambdaBody args pats body =
    let
        defns = [ (arg, patDefns pat) | arg <- args | pat <- pats ]
        defns' = concat [ mkPatDefns (idE arg) patDefns | (arg, patDefns) <- defns ]
    in
      if null defns' then
          body
      else
          WhereE body defns'


-- |
-- @
-- x@pat1 y@pat1' -> body1
-- x@pat2 y@pat2' -> body2
-- ...
-- _ -> blame "..."
-- @
--
-- @
-- cond
--   pred1 x && pred2 y -> body1 where { x = ... && y = ... }
--   pred1 x && pred2 y -> body1 where { x = ... && y = ... }
--   _ _ -> blame "..."
-- @
--
-- The generated code above is a simplified representation because
-- '&&' is encoded through 'CondE'.
expandCondMacro :: [([Pat], Expr)] -> String -> ExpanderM Expr
expandCondMacro ms blame =
    do let npats = length $ fst $ maximumBy (\x y -> compare (length (fst x)) (length (fst y))) ms
       args <- replicateM npats (genNameM "arg")
       let (patss, vals) = unzip ms
           preds = map (combinePreds args) patss
       vals' <- mapM expandOneM vals
       let vals'' = zipWith (lambdaBody args) patss vals'
           ms' = zip preds vals''
       returnOneM $ lambdas args (CondE ms' blame)
    where lambdas [] body = body
          lambdas (arg:args) body =
              LambdaE arg Nothing (lambdas args body)

          applyPred arg pat =
              AppE (patPred pat) (idE arg)

          alignArgs args pats =
              drop (length args - length pats) args

          combinePreds args pats =
              foldl1 andE (zipWith applyPred (alignArgs args pats) pats)

-- /cond macros


-- fixpoint macros

-- |
-- @
--   x@ = ... fn ...
-- @
--
-- @
--   fn@ x@ = ... fn ...
-- @
fixArgBody name body =
  LambdaE name Nothing body


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
fixDecl :: Expr -> Either String Expr
fixDecl expr@(FnDecl t _ name body) =
  do FnDecl _ _ name' body' <- renameDeclaration expr
     let argBody = fixArgBody name' body'
         declBody = Expr.appE "fix#" argBody
     return $ FnDecl t NrDef name declBody

-- |
-- @
-- a -> b
-- @
--
-- @
-- (a -> b) -> a -> b
-- @
fixArgT t@(ArrowT argT rangeT) =
  Just $ ArrowT t $ ArrowT argT rangeT

fixArgT t =
  error $ "Expander.fixArgT: expected arrow type" ++
          "\n\n\t t = " ++ show t ++ "\n"


fixArgDecl defName defT argName body =
  FnDecl (fixArgT defT) NrDef argName (fixArgBody defName body)


-- |
-- @
-- def fn : a -> b
--   x@ = ... fn ...
-- @
--
-- @
-- def fn : a -> b
--   = fix# arg
--   where {
--     def arg : (a -> b) -> a -> b
--       fn@ x@ = ... fn ...
--   }
-- @
-- fixFn :: String -> Type -> String -> Expr -> Either String Expr
-- fixFn defName defT argName body =
--   do let argDecl = fixArgDecl defName defT argName body
--      (arg, argName') <- renameDeclaration argDecl
--      return $ FnDecl (Just defT) NrDef defName $ whereBody argName' arg
--   where whereBody argName arg =
--           WhereE (Expr.appE "fix#" (Expr.idE argName)) [arg]

-- /fixpoint macros


oneM :: Expr -> Expr
oneM = id


-- edit: in case someday we want to return multiple exprs in expand
expandOneM :: Expr -> ExpanderM Expr
expandOneM = expandM
-- expandOneM expr = head <$> expandM expr


-- edit: in case someday we want to return multiple exprs in expand
returnOneM :: Expr -> ExpanderM Expr
returnOneM = return . oneM


expandM :: Expr -> ExpanderM Expr
expandM expr@IdE {} = returnOneM expr
expandM expr@IntE {} = returnOneM expr
expandM expr@RealE {} = returnOneM expr
expandM expr@CharE {} = returnOneM expr
expandM (SeqE exprs) = oneM . SeqE <$> mapM expandOneM exprs

expandM (AppE expr1 expr2) =
    oneM <$> ((AppE <$> expandOneM expr1) `ap` expandOneM expr2)

expandM (CondMacro ms blame) =
    expandCondMacro ms blame

expandM (CondE ms blame) =
    oneM . (`CondE` blame) <$> mapM expandMatch ms
    where expandMatch (expr1, expr2) =
              do expr1' <- expandOneM expr1
                 expr2' <- expandOneM expr2
                 return (expr1', expr2')

expandM CotypeDecl {} =
  error "Expander.expandM(CotypeDecl): cotypes must be eliminated in reorderer"

expandM expr@(FnDecl ann kw name body) =
    do body' <- expandOneM body
       if name `elem` Expr.freeVars body'
       then
         case fixDecl (FnDecl ann kw name body') of
           Left err -> throwError err
           Right expr -> returnOneM expr
       else
         returnOneM $ FnDecl ann NrDef name body'

expandM (LambdaMacro typePats body) =
  oneM . lambdas typePats <$> expandOneM body
  where lambdas [] body = body
        lambdas (pat:pats) body =
          let
            arg = fst (head (patDefns pat))
            IdE ann = patPred pat
          in
            LambdaE arg (Just (QualName.fromQualName ann)) (lambdas pats body)

expandM (LambdaE arg ann body) =
  oneM . LambdaE arg ann <$> expandOneM body

expandM (MergeE vals) =
  oneM . MergeE <$> mapM expandVals vals
  where expandVals (key, expr) =
          (key,) <$> expandOneM expr

expandM (WhereE body defns) =
  do body' <- expandOneM body
     oneM . WhereE body' <$> mapM expandM defns


expandDefinitionM :: Definition -> ExpanderM Definition
expandDefinitionM def@Definition { srcExpr = Just expr } =
    do expr' <- expandOneM expr
       return def { expExpr = Just expr' }


expandDefinition :: FileSystem -> Definition -> Either String Definition
expandDefinition fs def =
    fst <$> runStateT (expandDefinitionM def) initialExpanderState 


expandDefinitions :: FileSystem -> SrcFile -> [Definition] -> Either String SrcFile
expandDefinitions _ srcfile [] = return srcfile

expandDefinitions fs srcfile (def:defs) =
    do def' <- expandDefinition fs def
       let srcfile' = SrcFile.updateDefinitions srcfile [def']
           fs' = FileSystem.add fs srcfile'
       expandDefinitions fs' srcfile' defs


expand :: FileSystem -> SrcFile -> Either String SrcFile
expand _ srcfile@SrcFile { t = CoreT } =
    return srcfile

expand fs srcfile =
    expandDefinitions fs srcfile (SrcFile.defsAsc srcfile)