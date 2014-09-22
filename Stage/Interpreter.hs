{-# LANGUAGE ParallelListComp #-}
module Stage.Interpreter where

import Prelude hiding (mod, pred)

import Control.Monad.State
import Data.Functor ((<$>))
import qualified Data.Map as Map (fromList)
import Data.Maybe (catMaybes, isNothing, isJust, mapMaybe)
import Data.Either (lefts, rights)

import Data.Definition (Definition(..))
import qualified Data.Definition as Definition
import qualified Data.Env as Env (initial)
import Data.FileSystem (FileSystem)
import qualified Data.FileSystem as FileSystem
import Data.Module
import qualified Data.Module as Module (defsAsc, ensureDefinitions)
import Data.Expr
import qualified Data.QualName as QualName (fromQualName)
import Data.Symbol
import Monad.InterpreterM

evalM :: Expr -> InterpreterM Val
evalM (CharE c) = return $ CharVal c
evalM (IntE i) = return $ IntVal i
evalM (RealE d) = return $ RealVal d
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
         FnVal fn -> return (fn val2)
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
evalM (FnDecl Def _ _) =
  error "Interpreter.evalM(FnDecl Def ...): recursive functions must be eliminated in previous stages"
evalM (FnDecl NrDef str body) =
  do val <- evalM body
     addBindM str val
     return val
evalM (LambdaE arg body) =
  do env <- get
     return . FnVal $ closure env
  where closure env val =
          let
            m = withLexicalEnvM env $ do
                  addBindM arg val
                  withEnvM (evalM body)
          in
            fst (runState m env)
evalM (LetE defn body) =
  withEnvM $ do
    _ <- evalM defn
    withEnvM (evalM body)
evalM (MergeE vals) =
  TypeVal <$> SeqVal <$> mapM (evalM . snd) vals

freeNameDefinitions :: FileSystem -> Definition -> [Definition]
freeNameDefinitions fs def =
  let defs = map (FileSystem.lookupDefinition fs) (Definition.defFreeNames def) in
  if all isJust defs then
    catMaybes defs
  else
    error "Interpreter.freeNamesDefinitions: undefined free variables must be caught in previous stages"

interpretDefinition :: FileSystem -> Definition -> Definition
interpretDefinition fs def@Definition { defRen = Right expr } =
  do let defs = freeNameDefinitions fs def
     case (filter isNothing (map Definition.defSym defs), lefts (map Definition.defVal defs)) of
       ([], []) ->
         let
           syms = mapMaybe Definition.defSym defs
           vals = rights (map Definition.defVal defs)
           -- edit: fix: why FnSymbol ?
           env = Map.fromList [ (sym, v) | FnSymbol sym <- syms | v <- vals ]
           val = fst $ runState (evalM expr) (Env.initial env)
         in
           def { defVal = Right val }
       _ ->
         def { defVal = Left "definition depends on free names that failed to evaluate" }
interpretDefinition _ def = def

interpretDefinitions :: FileSystem -> Module -> [Definition] -> Module
interpretDefinitions _ mod [] = mod
interpretDefinitions fs mod (def:defs) =
  either
    (const $ interpretDefinitions fs mod defs)
    (let
      def' = interpretDefinition fs def
      mod' = Module.ensureDefinitions mod [def']
      fs' = FileSystem.add fs mod'
     in
       const $ interpretDefinitions fs' mod' defs)
    (Definition.defRen def)

interpret :: FileSystem -> Module -> Module
interpret _ mod@Module { modType = CoreT } = mod
interpret fs mod = interpretDefinitions fs mod (Module.defsAsc mod)
