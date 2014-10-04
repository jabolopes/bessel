{-# LANGUAGE ParallelListComp, LambdaCase, TupleSections #-}
module Stage.Interpreter where

import Prelude hiding (mod, pred)

import Control.Arrow ((***))
import Control.Monad.State
import Data.Functor ((<$>))
import Data.IORef
import qualified Data.Map as Map (empty, fromList)
import Data.Maybe (isNothing, isJust, mapMaybe)
import Data.Either (lefts, rights)

import Data.Definition (Definition(..))
import qualified Data.Definition as Definition
import qualified Data.Env as Env (findBind, initial)
import Data.Expr (DefnKw(..), Expr(..))
import qualified Data.Expr as Expr
import Data.FileSystem (FileSystem)
import qualified Data.FileSystem as FileSystem
import Data.Module (ModuleT(..), Module(..))
import qualified Data.Module as Module
import qualified Data.QualName as QualName (fromQualName)
import Data.Result (Result(..))
import Data.Symbol
import Monad.InterpreterM
import qualified Utils

evalM :: Expr -> InterpreterM Val
evalM (CharE c) = return $ CharVal c
evalM (IntE i) = return $ IntVal i
evalM (RealE d) = return $ RealVal d
evalM (IdE str) =
    do msym <- findBindM (QualName.fromQualName str)
       case msym of
         Just val -> liftIO $ readIORef val
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
evalM (CondE ms blame) = evalMatches ms
    where evalMatches [] =
              error $ "Interpreter.evalM(CondE): non-exhaustive patterns in " ++ blame
                    
          evalMatches ((pred, val):xs) =
              do pred' <- evalM pred
                 case pred' of
                   BoolVal False -> evalMatches xs
                   _ -> evalM val
evalM (FnDecl Def str body) =
  do ref <- liftIO . newIORef $ FnVal (\_ -> error $ str ++ ": loop")
     addBindM str ref
     val <- evalM body
     replaceBindM str val
     return val
evalM (FnDecl NrDef str body) =
  do val <- evalM body
     ref <- liftIO $ newIORef val
     addBindM str ref
     return val
evalM expr@(LambdaE arg body) =
  do vars <- freeVars
     return . FnVal $ closure vars
  where
    freeVars =
      do vals <- mapM (\name -> (name,) <$> findBindM name) $ Expr.freeVars expr
         if all (isJust . snd) vals
           then return $ map (id *** (\(Just x) -> x)) vals
           else error $ "Interpreter.evalM.freeReferences: undefined free variables must be caught in previous stages" ++
                        "\n\n\t vals = " ++ show (Expr.freeVars expr) ++ "\n\n"

    closure vars val =
      withEmptyEnvM $ do
        forM_ vars $ \(name, ref) ->
          addBindM name ref
        addBindM arg =<< liftIO (newIORef val)
        evalM body
evalM (LetE defn body) =
  withEnvM $ do
    _ <- evalM defn
    withEnvM (evalM body)
evalM (MergeE vals) =
  TypeVal <$> SeqVal <$> mapM (evalM . snd) vals

freeNameDefinitions :: FileSystem -> Definition -> [Definition]
freeNameDefinitions fs def =
  case mapM lookupDefinition (Definition.defFreeNames def) of
    Bad _ -> error "Interpreter.freeNamesDefinitions: undefined free variables must be caught in previous stages"
    Ok defs -> defs
  where
    lookupDefinition (modName, defName) =
      FileSystem.lookupDefinition fs (QualName.fromQualName modName) $
        Utils.stripModule (QualName.fromQualName modName) (QualName.fromQualName defName)

liftInterpreterM :: InterpreterM a -> IO a
liftInterpreterM m =
  fst <$> runStateT m (Env.initial Map.empty)

interpretDefinition :: FileSystem -> Definition -> IO Definition
interpretDefinition fs def@Definition { defSym = Just (FnSymbol symbol), defRen = Right expr } =
  do let defs = freeNameDefinitions fs def
     case (filter isNothing (map Definition.defSym defs), lefts (map Definition.defVal defs)) of
       ([], []) ->
         let
           syms = mapMaybe Definition.defSym defs
           vals = rights (map Definition.defVal defs)
         in
          do -- edit: fix: why FnSymbol ?
             env <- Map.fromList <$> sequence [ do return (sym, v) | FnSymbol sym <- syms | v <- vals ]
             (_, env') <- runStateT (evalM expr) (Env.initial env)
             case Env.findBind env' symbol of
               Nothing -> return def { defVal = Left $ "failed to evaluate " ++ QualName.fromQualName (Definition.defName def) }
               Just ref -> return def { defVal = Right ref }
       _ ->
         return def { defVal = Left "definition depends on free names that failed to evaluate" }
interpretDefinition _ def = return def

interpretDefinitions :: FileSystem -> Module -> [Definition] -> IO Module
interpretDefinitions _ mod [] = return mod
interpretDefinitions fs mod (def:defs) =
  case Definition.defRen def of
    Left _ -> interpretDefinitions fs mod defs
    Right _ ->
      do def' <- interpretDefinition fs def
         let mod' = Module.ensureDefinitions mod [def']
             fs' = FileSystem.add fs mod'
         interpretDefinitions fs' mod' defs

interpret :: FileSystem -> Module -> IO Module
interpret _ mod@Module { modType = CoreT } = return mod
interpret fs mod = interpretDefinitions fs mod (Module.defsAsc mod)
