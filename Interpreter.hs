{-# LANGUAGE RecursiveDo, NamedFieldPuns #-}
module Interpreter where

import Control.Monad.State
import Data.Functor ((<$>))
import Data.Map as Map (Map)
import qualified Data.Map as Map ((!), fromList, insert, keys, lookup, toList, union)
import Data.Maybe (catMaybes, fromJust)

import Data.Definition (Definition(..))
import qualified Data.Definition as Definition
import qualified Data.Env as Env (initial, empty, getBinds)
import Data.FileSystem (FileSystem)
import qualified Data.FileSystem as FileSystem
import Data.SrcFile
import qualified Data.SrcFile as SrcFile (updateDefinitions, defsAsc, deps, name)
import Data.Expr
import Data.QualName (QualName)
import qualified Data.QualName as QualName (fromQualName)
import Data.Symbol
import Monad.InterpreterM
import Utils


evalM :: Expr -> InterpreterM Val
evalM (CharE c) = return $ CharVal c
evalM (IntE i) = return $ IntVal i
evalM (RealE d) = return $ DoubleVal d
evalM (SeqE exprs) = SeqVal <$> mapM evalM exprs

evalM (IdE str) =
    do msym <- findBindM (QualName.fromQualName str)
       case msym of
         Just val -> return val
         Nothing -> error $ "Interpreter.evalM(IdE): unbound symbols must be caught by the renamer" ++
                            "\n\n\t str = " ++ QualName.fromQualName str ++ "\n"

evalM (AppE expr1 expr2) =
    do val1 <- evalM expr1
       val2 <- evalM expr2
       case val1 of
         FnVal fn -> fn val2
         _ -> error $ "Interpreter.evalM(AppE): application of non-functions must be detected by the renamer" ++
                      "\n\n\t expr1 = " ++ show expr1 ++
                      "\n\n\t -> val1 = " ++ show val1 ++
                      "\n\n\t expr2 = " ++ show expr2 ++
                      "\n\n\t -> val2 = " ++ show val2 ++ "\n"

evalM (CastE _ expr) = evalM expr

evalM (CondE ms blame) = evalMatches ms
    where evalMatches [] =
              error $ "Interpreter.evalM(CondE): non-exhaustive patterns in " ++ blame
                    
          evalMatches ((pred, val):ms) =
              do pred' <- evalM pred
                 case pred' of
                   BoolVal False -> evalMatches ms
                   _ -> evalM val

evalM CotypeDecl {} =
  error "Interpreter.evalM(CotypeDecl): cotypes must be eliminated in reoderer"

evalM (FnDecl _ Def str body) =
    do rec addBindM str val
           val <- evalM body
       return val

evalM (FnDecl _ NrDef str body) =
    do val <- evalM body
       addBindM str val
       return val

evalM (LambdaE str _ body) =
    FnVal . closure <$> get
    where closure env val =
              withLexicalEnvM env $ do
                addBindM str val
                withEnvM (evalM body)

evalM (MergeE vals) =
  TypeVal <$> SeqVal <$> mapM (evalM . snd) vals

evalM (WhereE expr exprs) =
    withEnvM $ do
      mapM_ evalM exprs
      withEnvM (evalM expr)


interpretDefinition :: FileSystem -> Definition -> Definition
interpretDefinition fs def@Definition { renExpr = Just expr } =
    do let defs = map (FileSystem.definition fs) (freeNames def)
           vals = Map.fromList [ (sym, fromJust (Definition.val def)) | def <- defs, let Just (FnSymbol sym) = Definition.symbol def ]
           val = fst $ runState (evalM expr) (Env.initial vals)
       def { val = Just val }


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