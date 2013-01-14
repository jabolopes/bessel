{-# LANGUAGE DoRec #-}
module Interpreter where

import Control.Monad.State
import Data.Functor ((<$>))
import qualified Data.Map as Map (toList)

import qualified Data.Env as Env (empty)
import Data.SrcFile
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

evalM (ModuleStx _ (Namespace _ stxs)) =
    do mapM_ evalM $ init stxs
       evalM $ last stxs

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


interpretIncremental :: ExprEnv -> [Stx String] -> (Expr, ExprEnv)
interpretIncremental state stxs =
    let
        m = do mapM_ evalM $ init stxs
               evalM $ last stxs
    in
      runState m state


interpretNamespace (Namespace _ stxs) =
    do mapM_ evalM $ init stxs
       evalM $ last stxs


interpretSrcFile :: ExprEnv -> SrcFile -> (Expr, ExprEnv)
interpretSrcFile state (SrcFile _ _ _ (Left (Namespace _ stxs))) =
    interpretIncremental state stxs

interpretSrcFile state (SrcFile _ _ _ (Right binds)) =
    let
        binds' = map (\(name, (_, expr)) -> (name, expr)) $ Map.toList binds
        m = do mapM_ (\(name, expr) -> addBindM name expr) binds'
               return $ snd $ last binds'
    in
      runState m state


interpretSrcFiles :: ExprEnv -> [SrcFile] -> (Expr, ExprEnv)
interpretSrcFiles state [srcfile] =
    interpretSrcFile state srcfile

interpretSrcFiles state (srcfile:srcfiles) =
    let (_, state') = interpretSrcFile state srcfile in
    interpretSrcFiles state' srcfiles


interpret :: [SrcFile] -> (Expr, ExprEnv)
interpret srcfiles =
    interpretSrcFiles Env.empty srcfiles