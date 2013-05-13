{-# LANGUAGE RecursiveDo, NamedFieldPuns #-}
module Interpreter where

import Control.Monad.State
import Data.Functor ((<$>))
import Data.Map as Map (Map)
import qualified Data.Map as Map ((!), fromList, insert, keys, lookup, toList, union)
import Data.Maybe (catMaybes)

import qualified Data.Env as Env (initial, empty, getBinds)
import Data.SrcFile
import qualified Data.SrcFile as SrcFile (name, deps, symbols, exprs, renNs)
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
                            "\n\n\t str = " ++ str ++ "\n"

evalM (AppStx stx1 stx2) =
    do expr1 <- evalM stx1
       expr2 <- evalM stx2
       case expr1 of
         FnExpr fn -> fn expr2
         _ -> error $ "Interpreter.evalM(AppStx): application of non-functions must be detected by the renamer" ++
                      "\n\n\t stx1 = " ++ show stx1 ++
                      "\n\n\t -> expr1 = " ++ show expr1 ++
                      "\n\n\t stx2 = " ++ show stx2 ++
                      "\n\n\t -> expr2 = " ++ show expr2 ++ "\n"

evalM (CondStx ms blame) = evalMatches ms
    where evalMatches [] =
              error $ "Interpreter.evalM(CondStx): non-exhaustive patterns in " ++ blame
                    
          evalMatches ((pred, expr):ms) =
              do val <- evalM pred
                 case val of
                   BoolExpr False -> evalMatches ms
                   _ -> evalM expr

evalM (DefnStx _ Def str body) =
    do rec addBindM str expr
           expr <- evalM body
       return expr

evalM (DefnStx _ NrDef str body) =
    do expr <- evalM body
       addBindM str expr
       return expr

evalM (LambdaStx str _ body) =
    FnExpr . closure <$> get
    where closure env expr =
              withLexicalEnvM env $ do
                addBindM str expr
                withEnvM (evalM body)

evalM (ModuleStx prefix ns) =
    error $ "Interpreter.evalM(ModuleStx): modules must be flattened by the renamer" ++
            "\n\n\t prefix = " ++ show prefix ++
            "\n\n\t namespace = " ++ show ns ++ "\n"

evalM (TypeStx name stxs) =
    error $ "Interpreter.evalM(TypeStx): datatypes must be flattened by the renamer" ++
            "\n\n\t name = " ++ show name ++
            "\n\n\t stxs = " ++ show stxs ++ "\n"

evalM (TypeMkStx name) =
    return $ FnExpr $ \expr ->
        return $ TypeExpr name (read name) expr

evalM (TypeUnStx) =
    return $ FnExpr $ \(TypeExpr _ _ expr) -> return expr

evalM (TypeIsStx name) =
    return $ FnExpr $ \(TypeExpr _ tid _) ->
        return $ if (read name) == tid
                 then true
                 else false

evalM (WhereStx stx stxs) =
    withEnvM $ do
      mapM_ evalM stxs
      withEnvM (evalM stx)


interpretNamespace :: Map String SrcFile -> [String] -> Namespace String -> ([Expr], ExprEnv)
interpretNamespace fs deps (Namespace _ stxs) =
    let
        exprss = map (SrcFile.exprs . (fs Map.!)) deps
        env = Env.initial (foldr1 Map.union exprss)
    in
     runState (mapM evalM stxs) env


envToExprs :: SrcFile -> ExprEnv -> Map String Expr
envToExprs srcfile exprEnv =
    let
        binds = Env.getBinds exprEnv
        names = Map.keys (SrcFile.symbols srcfile)
    in
     Map.fromList $ catMaybes [ case Map.lookup name binds of
                                   Nothing -> Nothing
                                   Just sym -> Just (name, sym) | name <- names ]


interpret :: Map String SrcFile -> SrcFile -> SrcFile
interpret _ srcfile@SrcFile { t = CoreT } =
    srcfile

interpret fs srcfile@SrcFile { t = SrcT, deps, renNs = Just ns } =
    let
        (_, env) = interpretNamespace fs deps ns
        env' = envToExprs srcfile env
    in
     srcfile { exprs = env' }
     
interpret _ SrcFile { t = InteractiveT } =
    error "Interpreter.interpret: for interactive srcfiles use 'interpretInteractive' instead of 'interpret'"


interpretInteractive :: Map String SrcFile -> SrcFile -> (SrcFile, Expr)
interpretInteractive fs srcfile@SrcFile { t = InteractiveT, deps, renNs = Just ns } =
    let
        (exprs, env) = interpretNamespace interactiveFs interactiveDeps ns
        env' = envToExprs srcfile env
    in
     (srcfile { exprs = Map.union (SrcFile.exprs srcfile) env' }, last exprs)
    where interactiveDeps =
            SrcFile.deps srcfile ++ [SrcFile.name srcfile]

          interactiveFs =
            Map.insert (SrcFile.name srcfile) srcfile fs