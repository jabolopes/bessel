{-# LANGUAGE DoRec, NamedFieldPuns #-}
module Interpreter where

import Control.Monad.State
import Data.Functor ((<$>))
import Data.Map as Map (Map)
import qualified Data.Map as Map ((!), toList)

import qualified Data.Env as Env (empty, union)
import Data.SrcFile
import qualified Data.SrcFile as SrcFile (env)
import Data.Stx
import Monad.InterpreterM


evalM :: Stx String -> InterpreterM Expr
evalM (CharStx c) = return $ CharExpr c
evalM (IntStx i) = return $ IntExpr i
evalM (DoubleStx d) = return $ DoubleExpr d
evalM (SeqStx stxs) = SeqExpr <$> mapM evalM stxs

evalM (IdStx str) =
    do msym <- findBindM str
       case msym of
         Just expr -> return expr
         Nothing -> error $ "Interpreter.evalM(IdStx): unbound symbols must be caught by the renamer" ++
                            "\n\n\t str = " ++ str ++
                            "\n\n"

evalM (AppStx stx1 stx2) =
    do expr1 <- evalM stx1
       expr2 <- evalM stx2
       case expr1 of
         FnExpr fn -> fn expr2
         _ -> error $ "Interpreter.evalM(AppStx): application of non-functions must be detected by the renamer" ++
                      "\n\n\t stx1 = " ++ show stx1 ++
                      "\n\n\t -> expr1 = " ++ show expr1 ++
                      "\n\n\t stx2 = " ++ show stx2 ++
                      "\n\n\t -> expr2 = " ++ show expr2 ++
                      "\n\n"

evalM (DefnStx Def str body) =
    do rec addBindM str expr
           expr <- evalM body
       return expr

evalM (DefnStx NrDef str body) =
    do expr <- evalM body
       addBindM str expr
       return expr

evalM (LambdaStx str body) =
    FnExpr . closure <$> get
    where closure env expr =
              withLexicalEnvM env $ do
                addBindM str expr
                withEnvM $ evalM body

evalM (ModuleStx prefix ns) =
    error $ "Interpreter.evalM(ModuleStx): modules must be flattened by the renamer" ++
            "\n\n\t prefix = " ++ show prefix ++
            "\n\n\t namespace = " ++ show ns ++ "\n\n"

evalM (TypeStx name stxs) =
    do mapM_ evalM stxs
       return $ SeqExpr $ map CharExpr name

evalM (TypeMkStx name) =
    return $ FnExpr $ \expr ->
        return $ TypeExpr name (read name) expr

evalM (TypeUnStx) =
    return $ FnExpr $ \(TypeExpr _ _ expr) -> return $ expr

evalM (TypeIsStx name) =
    return $ FnExpr $ \(TypeExpr _ tid _) ->
        if (read name) == tid then
            return true
        else
            return false

evalM (WhereStx stx stxs) =
    withEnvM $ do
      mapM_ evalM stxs
      withEnvM $ evalM stx


interpretNamespace :: Map String SrcFile -> [String] -> Namespace String -> ExprEnv
interpretNamespace fs deps (Namespace _ stxs) =
    let
        envs = map (SrcFile.env . (fs Map.!)) deps
        env = foldr1 Env.union envs
    in
      snd $ runState (mapM_ evalM stxs) env


interpretSrcFile :: Map String SrcFile -> SrcFile -> ExprEnv
interpretSrcFile _ SrcFile { srcNs = Right (_, binds) } =
    let
        binds' = [ (x, y) | (x, (_, y)) <- Map.toList binds ]
        m = do mapM_ (\(name, expr) -> addBindM name expr) binds'
               return $ snd $ last binds'
    in
      snd $ runState m Env.empty

interpretSrcFile fs SrcFile { deps, renNs = Just ns } =
    interpretNamespace fs deps ns


interpret :: Map String SrcFile -> SrcFile -> SrcFile
interpret fs srcfile =
    let env = interpretSrcFile fs srcfile in
    srcfile { env = env }


interpretInteractive :: SrcFile -> Stx String -> (SrcFile, Expr)
interpretInteractive srcfile stx =
    let (expr, env) = runState (evalM stx) (SrcFile.env srcfile) in
    (srcfile { env = env }, expr)