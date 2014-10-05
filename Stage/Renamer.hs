{-# LANGUAGE BangPatterns, MultiWayIf, NamedFieldPuns, ParallelListComp, TupleSections #-}
module Stage.Renamer where

import Prelude hiding (mod)

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Error (catchError, throwError)
import Control.Monad.State
import Data.List (isPrefixOf)

import Data.Definition (Definition(..))
import qualified Data.Definition as Definition
import Data.Expr (DefnKw(..), Expr(..))
import qualified Data.Expr as Expr (freeVars, idE)
import Data.FileSystem (FileSystem)
import qualified Data.FileSystem as FileSystem
import Data.Frame (Frame)
import qualified Data.Frame as Frame (frId)
import Data.FrameTree (FrameTree)
import qualified Data.FrameTree as FrameTree (empty, rootId, getFrame, getLexicalSymbol, addSymbol, addFrame)
import Data.Module (ModuleT (..), Module(..))
import qualified Data.Module as Module
import Data.PrettyString (PrettyString)
import qualified Data.QualName as QualName
import qualified Data.Result as Result
import Data.Symbol (Symbol (..))
import qualified Monad.Utils as Utils (returnOne)
import qualified Pretty.Stage.Renamer as Pretty
import qualified Utils

data RenamerState =
  RenamerState { renCurrentFrameId :: Int
               , renFrameTree :: FrameTree
               , renNameCount :: Int }

initialRenamerState :: RenamerState
initialRenamerState =
  let frameTree = FrameTree.empty in
  RenamerState { renCurrentFrameId = FrameTree.rootId frameTree
               , renFrameTree = frameTree
               , renNameCount = 0 }

type RenamerM a = StateT RenamerState (Either PrettyString) a

getCurrentFrameM :: RenamerM Frame
getCurrentFrameM =
  do frameTree <- renFrameTree <$> get
     currentFrameId <- renCurrentFrameId <$> get
     case FrameTree.getFrame frameTree currentFrameId of
       Nothing -> error "Stage.Renamer.getCurrentFrameM: current frame does not exist"
       Just x -> return x

genNumM :: RenamerM Int
genNumM =
  do count <- renNameCount <$> get
     modify $ \s -> s { renNameCount = count + 1 }
     return count

genNameM :: String -> RenamerM String
genNameM name = (name ++) . show <$> genNumM

lookupSymbolM :: String -> RenamerM (Maybe Symbol)
lookupSymbolM name =
  do frameTree <- renFrameTree <$> get
     currentFrame <- getCurrentFrameM
     return $ FrameTree.getLexicalSymbol frameTree currentFrame name

getSymbolM :: String -> RenamerM Symbol
getSymbolM name =
  do msym <- lookupSymbolM name
     case msym of
       Nothing -> throwError . Pretty.nameNotDefined . show $ name
       Just x -> return x

getFnSymbolM :: String -> RenamerM String
getFnSymbolM name =
  do sym <- getSymbolM name
     case sym of
       FnSymbol x -> return x
       _ -> throwError . Pretty.nameNotFunction . show $ name

addSymbolM :: String -> Symbol -> RenamerM ()
addSymbolM name sym = checkShadowing >> addSymbol
  where checkShadowing =
          do msym <- lookupSymbolM name
             case msym of
               Nothing -> return ()
               Just _ -> throwError . Pretty.nameAlreadyDefined . show $ name

        addSymbol =
          do frameTree <- renFrameTree <$> get
             currentFrame <- getCurrentFrameM
             let frameTree' = FrameTree.addSymbol frameTree currentFrame name sym
             modify $ \s -> s { renFrameTree = frameTree' }

addFnSymbolM :: String -> String -> RenamerM ()
addFnSymbolM name = addSymbolM name . FnSymbol

withScopeM :: RenamerM a -> RenamerM a
withScopeM m =
  do frameTree <- renFrameTree <$> get
     currentFrame <- getCurrentFrameM
     let (frameTree', currentFrame') = FrameTree.addFrame frameTree currentFrame
     modify $ \s -> s { renFrameTree = frameTree'
                      , renCurrentFrameId = Frame.frId currentFrame' }
     val <- m
     modify $ \s -> s { renCurrentFrameId = Frame.frId currentFrame }
     return val

renameOneM :: Expr -> RenamerM Expr
renameOneM expr = head <$> renameM expr

renameLambdaM :: String -> Expr -> RenamerM Expr
renameLambdaM arg body =
  do arg' <- genNameM arg
     LambdaE arg' <$>
       withScopeM
         (do addFnSymbolM arg arg'
             withScopeM (renameOneM body))

renameM :: Expr -> RenamerM [Expr]
renameM (AppE expr1 expr2) =
  Utils.returnOne $ AppE <$> renameOneM expr1 <*> renameOneM expr2
renameM expr@CharE {} = return [expr]
renameM (CondE ms blame) =
  Utils.returnOne $ CondE <$> mapM renameMatch ms <*> return blame
  where
    renameMatch (expr1, expr2) =
      (,) <$> renameOneM expr1 <*> renameOneM expr2
renameM (FnDecl Def name body) =
  if name `elem` Expr.freeVars body
  then do
    name' <- genNameM name
    addFnSymbolM name name'
    Utils.returnOne $ FnDecl Def name' <$> renameOneM body
  else
    renameM (FnDecl NrDef name body)
renameM (FnDecl NrDef name body) =
  do name' <- genNameM name
     body' <- renameOneM body
     addFnSymbolM name name'
     Utils.returnOne . return $ FnDecl NrDef name' body'
renameM (IdE name) =
  Utils.returnOne $ Expr.idE <$> getFnSymbolM (QualName.fromQualName name)
renameM expr@IntE {} = Utils.returnOne $ return expr
renameM (LetE defn body) =
  withScopeM $ do
    defn' <- renameOneM defn
    body' <- withScopeM (renameOneM body)
    Utils.returnOne . return $ LetE defn' body'
renameM (LambdaE arg body) =
  Utils.returnOne (renameLambdaM arg body)
renameM (MergeE vals) =
  Utils.returnOne $ MergeE <$> mapM renameValsM vals
  where renameValsM (name, expr) =  (name,) <$> renameOneM expr
renameM expr@RealE {} = Utils.returnOne $ return expr

lookupUnprefixedFreeVar :: FileSystem -> [String] -> String -> [Definition]
lookupUnprefixedFreeVar fs unprefixed name =
  Result.mapResult lookupDefinition unprefixed
  where
    lookupDefinition modName =
      FileSystem.lookupDefinition fs $ QualName.mkQualName [modName, name]

lookupPrefixedFreeVar :: FileSystem -> [(String, String)] -> String -> [Definition]
lookupPrefixedFreeVar fs prefixed name =
  Result.mapResult lookupDefinition $ filter isPrefix prefixed
  where
    isPrefix (_, y) =
      Utils.splitId y `isPrefixOf` Utils.splitId name

    lookupDefinition (useName, asName) =
      do let moduleName = QualName.moduleName $ QualName.mkQualName [name]
             defName = QualName.definitionName $ QualName.mkQualName [name]
         if | moduleName == asName ->
              FileSystem.lookupDefinition fs $ QualName.mkQualName [useName, defName]
            | otherwise ->
              fail ""

lookupFreeVars :: FileSystem -> [String] -> [(String, String)] -> [String] -> RenamerM [Definition]
lookupFreeVars _ _ _ [] = return []
lookupFreeVars fs unprefixed prefixed (name:names) =
  do let fns = [lookupUnprefixedFreeVar fs unprefixed, lookupPrefixedFreeVar fs prefixed]
     case concatMap ($ name) fns of
       [] -> throwError . Pretty.nameNotDefined . show $ name
       [def] -> (def:) <$> lookupFreeVars fs unprefixed prefixed names
       defs -> throwError . Pretty.nameMultiplyDefined name $ map (QualName.fromQualName . Definition.defName) defs

renameDefinitionM :: FileSystem -> Definition -> RenamerM Definition
renameDefinitionM fs def@Definition { defExp = Right expr } =
  do let prefixed = Definition.prefixedUses def
         unprefixed = Definition.unprefixedUses def
         names = Expr.freeVars expr
     freeNameDefs <- lookupFreeVars fs unprefixed prefixed names
     addFreeNames names freeNameDefs
     (do expr' <- renameOneM expr
         return $ def { defFreeNames = map Definition.defName freeNameDefs,
                        defRen = Right expr' })
       `catchError`
         (\err -> return $ def { defFreeNames = map Definition.defName freeNameDefs
                               , defRen = Left err })
  where
    addFreeNames [] [] = return ()
    addFreeNames (name:names) (def:defs) =
      do addSymbolM name . FnSymbol . QualName.fromQualName $ Definition.defName def
         addFreeNames names defs
renameDefinitionM _ def = return def

renameDefinition :: FileSystem -> Definition -> Either PrettyString Definition
renameDefinition fs def =
  fst <$> runStateT (renameDefinitionM fs def) initialRenamerState

renameDefinitions :: FileSystem -> Module -> [Definition] -> Either PrettyString Module
renameDefinitions _ mod [] = return mod
renameDefinitions fs mod (def:defs) =
  do def' <- renameDefinition fs def
     let mod' = Module.ensureDefinitions mod [def']
         fs' = FileSystem.add fs mod'
     renameDefinitions fs' mod' defs

rename :: FileSystem -> Module -> Either PrettyString Module
rename _ mod@Module { modType = CoreT } = return mod
rename fs mod = renameDefinitions fs mod (Module.defsAsc mod)
