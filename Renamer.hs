{-# LANGUAGE NamedFieldPuns, ParallelListComp, TupleSections #-}
module Renamer where

import Control.Monad.Error (throwError)
import Control.Monad.State
import Data.Functor ((<$>))
import Data.List (intercalate, isPrefixOf, maximumBy, nub, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)

import Data.FileSystem (FileSystem)
import qualified Data.FileSystem as FileSystem (add, get, lookupDefinition)
import Data.Frame (Frame)
import qualified Data.Frame as Frame (fid)
import Data.FrameTree (FrameTree)
import qualified Data.FrameTree as FrameTree
import Data.SrcFile (SrcFileT (..), SrcFile(..))
import qualified Data.SrcFile as SrcFile (name, defsAsc, symbols, addDefinitionSymbols, updateDefinitions)
import Data.Stx
import Data.Symbol (Symbol (..))
import qualified Data.Symbol as Symbol
import Data.Tuple (swap)
import Data.Type
import Utils (rebaseName, flattenId, splitId)

import Data.Definition (Definition(Definition, freeNames, expStx, symbol, renStx))
import qualified Data.Definition as Definition


data RenamerState =
    RenamerState { frameTree :: FrameTree
                 , nameCount :: Int
                 , typeCount :: Int
                 , currentFrame :: Int
                 , currentNamespace :: String }


initialRenamerState :: String -> RenamerState
initialRenamerState ns =
    let frameTree = FrameTree.empty in
    RenamerState { frameTree = frameTree
                 , nameCount = 0
                 , typeCount = 0
                 , currentFrame = FrameTree.rootId frameTree
                 , currentNamespace = ns }


type RenamerM a = StateT RenamerState (Either String) a


getCurrentFrameM :: RenamerM Frame
getCurrentFrameM =
    do tree <- frameTree <$> get
       fid <- currentFrame <$> get
       case FrameTree.getFrame tree fid of
         Nothing -> error "Renamer.getCurrentFrameM: current frame does not exist"
         Just x -> return x


genNumM :: RenamerM Int
genNumM =
    do count <- nameCount <$> get
       modify $ \s -> s { nameCount = count + 1 }
       return count


genNameM :: String -> RenamerM String
genNameM name = (name ++) . show <$> genNumM


genTypeIdM :: RenamerM Int
genTypeIdM =
    do count <- typeCount <$> get
       modify $ \s -> s { typeCount = count + 1 }
       return count


lookupSymbol :: FrameTree -> Frame -> String -> Maybe Symbol
lookupSymbol = FrameTree.getLexicalSymbol


lookupSymbolM :: String -> RenamerM (Maybe Symbol)
lookupSymbolM name =
    do tree <- frameTree <$> get
       currentFrame <- getCurrentFrameM
       return $ lookupSymbol tree currentFrame name


getSymbolM :: String -> RenamerM Symbol
getSymbolM name =
    do msym <- lookupSymbolM name
       case msym of
         Nothing -> throwError $ "name " ++ show name ++ " is not defined"
         Just t -> return t


getFnSymbolM :: String -> RenamerM String
getFnSymbolM name =
    do sym <- getSymbolM name
       case sym of
         FnSymbol name -> return name
         _ -> throwError $ "name " ++ show name ++ " is not a function"


getTypeSymbolM :: String -> RenamerM Int
getTypeSymbolM name =
    do sym <- getSymbolM name
       case sym of
         TypeSymbol tid -> return tid
         _ -> throwError $ "name " ++ show name ++ " is not an inductive type"


addSymbolM :: String -> Symbol -> RenamerM ()
addSymbolM name sym = checkShadowing name >> addSymbol name sym
    where checkShadowing name =
              do msym <- lookupSymbolM name
                 case msym of
                   Nothing -> return ()
                   Just _ -> throwError $ "name " ++ show name ++ " is already defined"

          addSymbol name sym =
              do tree <- frameTree <$> get
                 currentFrame <- getCurrentFrameM
                 let tree' = FrameTree.addSymbol tree currentFrame name sym
                 modify $ \s -> s { frameTree = tree' }


addFnSymbolM :: String -> String -> RenamerM ()
addFnSymbolM name = addSymbolM name . FnSymbol


addTypeSymbolM :: String -> Int -> RenamerM ()
addTypeSymbolM name = addSymbolM name . TypeSymbol


withScopeM :: RenamerM a -> RenamerM a
withScopeM m =
    do tree <- frameTree <$> get
       frame <- getCurrentFrameM
       let (tree', frame') = FrameTree.addFrame tree frame
       modify $ \s -> s { frameTree = tree', currentFrame = Frame.fid frame' }
       val <- m
       modify $ \s -> s { currentFrame = Frame.fid frame }
       return val


-- lambda

renameLambdaM :: String -> Maybe String -> Stx String -> RenamerM (Stx String)
renameLambdaM arg ann body =
    do arg' <- genNameM arg
       LambdaStx arg' ann <$>
         withScopeM
           (do addFnSymbolM arg arg'
               withScopeM (renameOneM body))


renameUnannotatedLambdaM :: String -> Stx String -> RenamerM (Stx String)
renameUnannotatedLambdaM arg = renameLambdaM arg Nothing


renameAnnotatedLambdaM :: String -> String -> Stx String -> RenamerM (Stx String)
renameAnnotatedLambdaM arg ann body = undefined
-- edit: remove undefined
-- renameAnnotatedLambdaM arg ann body =
--     do argT <- getTypeSymbolM ann
--        renameLambdaM arg (Just argT) body

-- /lambda


renameOneM :: Stx String -> RenamerM (Stx String)
renameOneM stx = head <$> renameM stx


renameM :: Stx String -> RenamerM [Stx String]
renameM stx@(CharStx _) = return [stx]
renameM stx@(IntStx _) = return [stx]
renameM stx@(DoubleStx _) = return [stx]
renameM (SeqStx stxs) = (:[]) . SeqStx <$> mapM renameOneM stxs

renameM (IdStx name) =
    (:[]) . IdStx <$> getFnSymbolM name

renameM (AppStx stx1 stx2) =
    (:[]) <$> ((AppStx <$> renameOneM stx1) `ap` renameOneM stx2)

renameM CondMacro {} =
    error "Renamer.renameM(CondMacro): macros must be expanded in expander"

renameM (CondStx ms blame) =
    (:[]) . (`CondStx` blame) <$> mapM renameMatch ms
    where renameMatch (stx1, stx2) =
              do stx1' <- renameOneM stx1
                 stx2' <- renameOneM stx2
                 return (stx1', stx2')

renameM CotypeStx {} =
  error "Renaner.renameM(CotypeStx): cotypes must be eliminated in reoderer"

renameM (DefnStx ann Def name body) =
    if name `elem` freeVars body
    then do
      name' <- genNameM name
      addFnSymbolM name name'
      (:[]) . DefnStx ann Def name' <$> renameOneM body
    else
        renameM (DefnStx ann NrDef name body)

renameM (DefnStx ann NrDef name body) =
    do name' <- genNameM name
       body' <- renameOneM body
       addFnSymbolM name name'
       return [DefnStx ann NrDef name' body']

renameM LambdaMacro {} =
    error "Renamer.renameM(LambdaMacro): macros must be expanded in expander"

renameM (LambdaStx arg Nothing body) =
    (:[]) <$> renameUnannotatedLambdaM arg body

renameM (LambdaStx arg (Just ann) body) =
    (:[]) <$> renameAnnotatedLambdaM arg ann body

renameM (MergeStx vals) =
  (:[]) <$> MergeStx <$> mapM renameValsM vals
  where renameValsM (name, stx) =  (name,) <$> renameOneM stx

renameM (WhereStx stx stxs) =
    withScopeM $ do
      stxs' <- concat <$> mapM renameM stxs
      stx' <- withScopeM (renameOneM stx)
      return [WhereStx stx' stxs']


lookupUnprefixedFreeVar :: FileSystem -> [String] -> String -> [Definition]
lookupUnprefixedFreeVar fs unprefixed var =
    let unprefixedVars = map (++ '.':var) unprefixed in
    mapMaybe (FileSystem.lookupDefinition fs) unprefixedVars


lookupPrefixedFreeVar :: FileSystem -> [(String, String)] -> String -> [Definition]
lookupPrefixedFreeVar fs prefixed name =
    mapMaybe (definition . rebase name) $ filter (isPrefix name) prefixed
    where isPrefix name (_, y) = splitId y `isPrefixOf` splitId name
          rebase name (x, y) = rebaseName x y name
          definition = FileSystem.lookupDefinition fs


lookupFreeVars :: FileSystem -> [String] -> [(String, String)] -> [String] -> RenamerM [Definition]
lookupFreeVars _ _ _ [] = return []

lookupFreeVars fs unprefixed prefixed (name:names) =
    do let fns = [lookupUnprefixedFreeVar fs unprefixed,
                  lookupPrefixedFreeVar fs prefixed]
       case concatMap ($ name) fns of
         [] -> throwError $ "name " ++ show name ++ " is not defined"
         [def] -> (def:) <$> lookupFreeVars fs unprefixed prefixed names
         _ -> throwError $ "name " ++ show name ++ " is multiply defined"


renameDefinitionM :: FileSystem -> Definition -> RenamerM Definition
renameDefinitionM fs def@Definition { expStx = Just stx } =
    do let unprefixed = Definition.unprefixedUses def
           prefixed = Definition.prefixedUses def
           names = freeVars stx
       defs <- lookupFreeVars fs unprefixed prefixed names
       sequence_ [ addSymbolM name sym | name <- names | def <- defs, let Just sym = Definition.symbol def ]
       stx' <- renameOneM stx
       sym <- getSymbolM $ flattenId $ tail $ splitId $ Definition.name def
       return $ def { freeNames = map Definition.name defs
                    , symbol = Just sym
                    , renStx = Just stx' }


renameDefinition :: FileSystem -> String -> Definition -> Either String Definition
renameDefinition fs ns def =
  fst <$> runStateT (renameDefinitionM fs def) (initialRenamerState ns)


renameDefinitions :: FileSystem -> SrcFile -> [Definition] -> Either String SrcFile
renameDefinitions _ srcfile [] = return srcfile

renameDefinitions fs srcfile (def:defs) =
    do def' <- renameDefinition fs (SrcFile.name srcfile) def
       let srcfile' = SrcFile.updateDefinitions srcfile [def']
           fs' = FileSystem.add fs srcfile'
       renameDefinitions fs' srcfile' defs


rename :: FileSystem -> SrcFile -> Either String SrcFile
rename _ srcfile@SrcFile { t = CoreT } =
    return srcfile

rename fs srcfile =
    renameDefinitions fs srcfile (SrcFile.defsAsc srcfile)