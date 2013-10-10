module Stage.Expander where

import Control.Applicative ((<$>), (<*>))
import Data.List (nub, partition)

import Data.Expr (Expr(..))
import qualified Data.Expr as Expr
import Data.Macro
import Data.Module
import qualified Data.Module as Module
import Data.PrettyString (PrettyString)
import Pretty.Stage.Expander as Pretty

expandCond :: [([Pat], Macro)] -> String -> Either PrettyString Expr
expandCond ms blame = CondMacro <$> expandMatches ms <*> return blame
  where expandMatches [] = return []
        expandMatches ((pats, m):ms) =
          do ps <- mapM expandPat pats
             e <- expandMacro m
             ((ps, e):) <$> expandMatches ms

expandModule :: Macro -> Either PrettyString Module
expandModule (ModuleM me uses decls)
  | length (map fst uses) /= length (nub (map fst uses)) =
    Left (Pretty.moduleContainsDuplicateUses me uses)
  | length (map snd uses) /= length (nub (map snd uses)) =
    Left (Pretty.moduleContainsDuplicateQualifiers me uses)
  | otherwise =
    do let (unprefixedUses, prefixedUses) = partition (null . snd) uses
           deps = map fst uses
       decls' <- mapM expandMacro decls
       return (Module.initial SrcT me deps) { modUnprefixedUses = map fst unprefixedUses
                                            , modPrefixedUses = prefixedUses
                                            , modDecls = decls' }

expandPatGuard :: PatGuard -> Either PrettyString Expr.Pat
expandPatGuard AllPG = return Expr.mkAllPat
expandPatGuard (PredicatePG m) = Expr.mkPredPat <$> expandMacro m
expandPatGuard (ListPG pat1 pat2) =
  do e1 <- expandPat pat1
     e2 <- expandPat pat2
     return $ Expr.mkCombPat (Expr.idE "isList") [Expr.idE "hd", Expr.idE "tl"] [e1, e2]
expandPatGuard (TuplePG pats) = Expr.mkListPat <$> mapM expandPat pats

expandPat :: Pat -> Either PrettyString Expr.Pat
expandPat pat
  | null (patBinder pat) = expandPatGuard (patGuard pat)
  | otherwise = Expr.namePat (patBinder pat) <$> expandPatGuard (patGuard pat)

expandSeq :: [Macro] -> Either PrettyString Expr
expandSeq ms = Expr.seqE <$> mapM expandMacro ms

expandString :: String -> Either PrettyString Expr
expandString str = return (Expr.stringE str)

expandMacro :: Macro -> Either PrettyString Expr
expandMacro (AndM m1 m2) = Expr.andE <$> expandMacro m1 <*> expandMacro m2
expandMacro (AppM m1 m2) = AppE <$> expandMacro m1 <*> expandMacro m2
expandMacro (BinOpM op m1 m2) = Expr.binOpE op <$> expandMacro m1 <*> expandMacro m2
expandMacro (CharM c) = return (CharE c)
expandMacro (CondM ms) = expandCond ms "lambda"
expandMacro (FnDeclM kw name (CondM ms)) = FnDecl kw name <$> expandCond ms name
expandMacro (FnDeclM kw name body) = FnDecl kw name <$> expandMacro body
expandMacro (IdM name) = return (Expr.IdE name)
expandMacro (IntM n) = return (Expr.intE n)
expandMacro (ModuleM me _ _) = Left (Pretty.devModuleNested me)
expandMacro (OrM m1 m2) = Expr.orE <$> expandMacro m1 <*> expandMacro m2
expandMacro (RealM n) = return (Expr.realE n)
expandMacro (SeqM ms) = expandSeq ms
expandMacro (StringM str) = expandString str
expandMacro (WhereM m ms) = WhereE <$> expandMacro m <*> mapM expandMacro ms
