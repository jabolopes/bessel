{-# LANGUAGE RecursiveDo, NamedFieldPuns #-}
module Interpreter where

import Control.Monad.State
import Data.Functor ((<$>))
import Data.Map as Map (Map)
import qualified Data.Map as Map ((!), fromList, insert, keys, lookup, toList, union)
import Data.Maybe (catMaybes)

import Data.FileSystem (FileSystem)
import qualified Data.FileSystem as FileSystem
import qualified Data.Env as Env (initial, empty, getBinds)
import Data.SrcFile
import qualified Data.SrcFile as SrcFile (updateDefinitions, defsAsc, deps, name, symbols, exprs)
import Data.Stx
import Monad.InterpreterM
import Utils


import Data.Maybe (fromJust)
import Data.Symbol

import Data.Definition (Definition(..))
import qualified Data.Definition as Definition



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

-- evalM (CotypeMkStx name) =
--     return $ FnExpr $ \expr ->
--         return $ TypeExpr name (read name) expr

evalM (WhereStx stx stxs) =
    withEnvM $ do
      mapM_ evalM stxs
      withEnvM (evalM stx)


interpretNamespace :: FileSystem -> [String] -> Namespace String -> ([Expr], ExprEnv)
interpretNamespace fs deps (Namespace _ stxs) =
    let
        exprss = map (SrcFile.exprs . (FileSystem.get fs)) deps
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


interpretDefinition :: FileSystem -> Definition -> Definition
interpretDefinition fs def@Definition { renStx = Just stx } =
    do let defs = map (FileSystem.definition fs) (freeNames def)
           exprs = Map.fromList [ (sym, fromJust (Definition.expr def)) | def <- defs, let Just (FnSymbol sym) = Definition.symbol def ]
           expr = fst $ runState (evalM stx) (Env.initial exprs)
       def { expr = Just expr }


interpretDefinitions :: FileSystem -> SrcFile -> [Definition] -> SrcFile
interpretDefinitions _ srcfile [] = srcfile

interpretDefinitions fs srcfile (def:defs) =
    let
        def' = interpretDefinition fs def
        srcfile' = SrcFile.updateDefinitions srcfile [def']
        fs' = FileSystem.add fs srcfile'
    in
      interpretDefinitions fs' srcfile' defs


interpret :: FileSystem -> SrcFile -> SrcFile
interpret _ srcfile@SrcFile { t = CoreT } =
    srcfile

interpret fs srcfile =
    interpretDefinitions fs srcfile (SrcFile.defsAsc srcfile)