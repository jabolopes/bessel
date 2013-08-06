{-# LANGUAGE NamedFieldPuns, ParallelListComp, TupleSections #-}
module Renamer where

import Control.Monad.Error (throwError)
import Control.Monad.State
import Data.Functor ((<$>))
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)

import Data.Definition (Definition(Definition, freeNames, expExpr, symbol, renExpr))
import qualified Data.Definition as Definition
import Data.FileSystem (FileSystem)
import qualified Data.FileSystem as FileSystem (add, lookupDefinition)
import Data.Frame (Frame)
import qualified Data.Frame as Frame (fid)
import Data.FrameTree (FrameTree)
import qualified Data.FrameTree as FrameTree (empty, rootId, getFrame, getLexicalSymbol, addSymbol, addFrame)
import Data.SrcFile (SrcFileT (..), SrcFile(..))
import qualified Data.SrcFile as SrcFile (name, defsAsc, updateDefinitions)
import Data.Expr (DefnKw(..), Expr(..))
import qualified Data.Expr as Expr (appE, freeVars, idE)
import qualified Data.QualName as QualName (fromQualName)
import Data.Symbol (Symbol (..))
import Data.Type (Type(ArrowT))
import Utils (rebaseName, flattenId, splitId)


data RenamerState =
    RenamerState { frameTree :: FrameTree
                 , nameCount :: Int
                 , typeCount :: Int
                 , currentFrame :: Int
                 , existFreeVars :: Bool }


initialRenamerState :: RenamerState
initialRenamerState =
    let frameTree = FrameTree.empty in
    RenamerState { frameTree = frameTree
                 , nameCount = 0
                 , typeCount = 0
                 , currentFrame = FrameTree.rootId frameTree
                 , existFreeVars = False }


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

renameLambdaM :: String -> Maybe String -> Expr -> RenamerM (Expr)
renameLambdaM arg ann body =
    do arg' <- genNameM arg
       LambdaE arg' ann <$>
         withScopeM
           (do addFnSymbolM arg arg'
               withScopeM (renameOneM body))


renameUnannotatedLambdaM :: String -> Expr -> RenamerM (Expr)
renameUnannotatedLambdaM arg = renameLambdaM arg Nothing


renameAnnotatedLambdaM :: String -> String -> Expr -> RenamerM (Expr)
renameAnnotatedLambdaM arg ann body = undefined
-- edit: remove undefined
-- renameAnnotatedLambdaM arg ann body =
--     do argT <- getTypeSymbolM ann
--        renameLambdaM arg (Just argT) body

-- /lambda


renameOneM :: Expr -> RenamerM (Expr)
renameOneM expr = head <$> renameM expr


renameM :: Expr -> RenamerM [Expr]
renameM expr@(IdE name) =
  do b <- existFreeVars <$> get
     if b
     then do
       msym <- lookupSymbolM (QualName.fromQualName name)
       case msym of
         Just (FnSymbol sym) -> return $ (:[]) $ Expr.idE sym
         _ -> return [expr]
     else
       (:[]) . Expr.idE <$> getFnSymbolM (QualName.fromQualName name)

renameM expr@IntE {} = return [expr]
renameM expr@RealE {} = return [expr]
renameM expr@CharE {} = return [expr]
renameM (SeqE exprs) = (:[]) . SeqE <$> mapM renameOneM exprs

renameM (AppE expr1 expr2) =
    (:[]) <$> ((AppE <$> renameOneM expr1) `ap` renameOneM expr2)

renameM (CastE typ expr) =
  (:[]) <$> CastE typ <$> renameOneM expr

renameM CondMacro {} =
    error "Renamer.renameM(CondMacro): macros must be expanded in expander"

renameM (CondE ms blame) =
    (:[]) . (`CondE` blame) <$> mapM renameMatch ms
    where renameMatch (expr1, expr2) =
              do expr1' <- renameOneM expr1
                 expr2' <- renameOneM expr2
                 return (expr1', expr2')

renameM CotypeDecl {} =
  error "Renaner.renameM(CotypeDecl): cotypes must be eliminated in reorderer"

renameM (FnDecl Def name body) =
    if name `elem` Expr.freeVars body
    then do
      name' <- genNameM name
      addFnSymbolM name name'
      (:[]) . FnDecl Def name' <$> renameOneM body
    else
        renameM (FnDecl NrDef name body)

renameM (FnDecl NrDef name body) =
    do name' <- genNameM name
       body' <- renameOneM body
       addFnSymbolM name name'
       return [FnDecl NrDef name' body']

renameM (LambdaE arg Nothing body) =
    (:[]) <$> renameUnannotatedLambdaM arg body

renameM (LambdaE arg (Just ann) body) =
    (:[]) <$> renameAnnotatedLambdaM arg ann body

renameM (MergeE vals) =
  (:[]) <$> MergeE <$> mapM renameValsM vals
  where renameValsM (name, expr) =  (name,) <$> renameOneM expr

renameM (WhereE expr exprs) =
    withScopeM $ do
      exprs' <- concat <$> mapM renameM exprs
      expr' <- withScopeM (renameOneM expr)
      return [WhereE expr' exprs']


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


renameDeclaration :: Expr -> Either String Expr
renameDeclaration expr@(FnDecl _ name _) =
  let state = initialRenamerState { existFreeVars = True } in
  fst <$> runStateT (renameOneM expr) state


renameDefinitionM :: FileSystem -> Definition -> RenamerM Definition
renameDefinitionM fs def@Definition { expExpr = Just expr } =
    do let unprefixed = Definition.unprefixedUses def
           prefixed = Definition.prefixedUses def
           names = Expr.freeVars expr
       defs <- lookupFreeVars fs unprefixed prefixed names
       sequence_ [ addSymbolM name sym | name <- names | def <- defs, let Just sym = Definition.symbol def ]
       expr' <- renameOneM expr
       sym <- getSymbolM $ flattenId $ tail $ splitId $ Definition.name def
       return $ def { freeNames = map Definition.name defs
                    , symbol = Just sym
                    , renExpr = Just expr' }


renameDefinition :: FileSystem -> String -> Definition -> Either String Definition
renameDefinition fs ns def =
  fst <$> runStateT (renameDefinitionM fs def) initialRenamerState


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