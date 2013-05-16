{-# LANGUAGE NamedFieldPuns, ParallelListComp #-}
module Renamer where

import Control.Monad.Error
import Control.Monad.State
import Data.Functor ((<$>))
import Data.List (intercalate, maximumBy, nub, partition, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust)

import Data.FileSystem (FileSystem)
import qualified Data.FileSystem as FileSystem
import Data.Frame (Frame)
import qualified Data.Frame as Frame (frameId, symbols)
import Data.FrameEnv (FrameEnv)
import qualified Data.FrameEnv as FrameEnv
import Data.SrcFile (SrcFileT (..), SrcFile(..))
import qualified Data.SrcFile as SrcFile (name, srcNs, symbols, addDefinitionSymbols)
import Data.Stx
import Data.Symbol (Symbol (..))
import qualified Data.Symbol as Symbol
import Data.Tuple (swap)
import Utils (fromSingleton, split)


data RenamerState =
    RenamerState { fs :: FileSystem
                 , frameEnv :: FrameEnv
                 , currentFrame :: Int
                 , currentCount :: Int
                 , currentNamespace :: String
                 , nslevel :: Bool
                 , unprefixedUses :: [String]
                 , prefixedUses :: Map String String }


initialRenamerState :: FileSystem -> RenamerState
initialRenamerState fs =
    let frameEnv = FrameEnv.empty in
    RenamerState { fs = fs
                 , frameEnv = frameEnv
                 , currentFrame = FrameEnv.rootId frameEnv
                 , currentCount = 0
                 , currentNamespace = ""
                 , nslevel = True
                 , unprefixedUses = []
                 , prefixedUses = Map.empty }


type RenamerM a = StateT RenamerState (Either String) a


getCurrentFrameM :: RenamerM Frame
getCurrentFrameM =
    do frameEnv <- frameEnv <$> get
       FrameEnv.getFrame frameEnv <$> currentFrame <$> get


genNumM :: RenamerM Int
genNumM =
    do count <- currentCount <$> get
       modify $ \state -> state { currentCount = count + 1 }
       return count


genNameM :: String -> RenamerM String
genNameM name =
    do nslevel <- nslevel <$> get
       if nslevel
       then do
         ns <- currentNamespace <$> get
         let qual | null ns = name
                  | otherwise = ns ++ "." ++ name
         return qual
       else
         (name ++) . show <$> genNumM


withNslevel :: Bool -> RenamerM a -> RenamerM a
withNslevel l m =
    do l' <- nslevel <$> get
       modify $ \state -> state { nslevel = l }
       val <- m
       modify $ \state -> state { nslevel = l' }
       return val


withNamespace :: [String] -> RenamerM a -> RenamerM a
withNamespace ns m =
    do ns' <- currentNamespace <$> get
       let qual | null ns' = intercalate "." ns
                | otherwise = ns' ++ "." ++ intercalate "." ns
       modify $ \state -> state { currentNamespace = qual }
       val <- m
       modify $ \state -> state { currentNamespace = ns' }
       return val


getUnprefixedSymbol
    :: FileSystem
    -> FrameEnv
    -> [String]
    -> Frame
    -> String
    -> Either String Symbol
getUnprefixedSymbol fs frameEnv unprefixedUses currentFrame name =
    case getLocalSymbol frameEnv currentFrame of
      Just sym -> Right sym
      Nothing -> getNamespaceSymbol fs unprefixedUses
    where getLocalSymbol frameEnv currentFrame =
              FrameEnv.getLexicalSymbol frameEnv currentFrame name

          getNamespaceSymbol fs unprefixedUses =
              let
                  srcfiles = map (FileSystem.get fs) unprefixedUses
                  symss = map SrcFile.symbols srcfiles
                  syms = catMaybes [ Map.lookup name syms | syms <- symss ]
              in
                case syms of
                  [] -> Left $ "name " ++ show name ++ " is not defined"
                  [sym] -> Right sym
                  _ -> Left $ "name " ++ show name ++ " is multiply defined"


getPrefixedSymbol
    :: FileSystem
    -> FrameEnv
    -> Map String String
    -> [String]
    -> String
    -> Either String Symbol
getPrefixedSymbol fs frameEnv prefixedUses prefix name =
    case getModuleSymbol frameEnv of
      Just sym -> Right sym
      Nothing -> case getPrefixModule prefixedUses of
                   Left str -> Left str
                   Right prefix' -> getNamespaceSymbol fs prefix'
    where getModuleSymbol env =
              do fr <- FrameEnv.getModuleFrame env prefix
                 FrameEnv.getFrameSymbol env fr name

          getPrefixModule prefixedUses =
              case Map.lookup (intercalate "." prefix) prefixedUses of
                Nothing -> Left $ "namespace or module " ++ show (intercalate "." prefix) ++ " is not defined"
                Just x -> Right x

          getNamespaceSymbol fs prefix' =
              do let syms = SrcFile.symbols (FileSystem.get fs prefix')
                 case Map.lookup name syms of
                   Nothing -> Left $ "name " ++ show (intercalate "." prefix ++ "." ++ name) ++ " is not defined"
                   Just x -> Right x


lookupSymbolM :: String -> RenamerM (Either String Symbol)
lookupSymbolM name =
    do let names = split '.' name
       fs <- fs <$> get
       frameEnv <- frameEnv <$> get
       currentFrame <- getCurrentFrameM
       if null (tail names)
       then do
         unprefixedUses <- unprefixedUses <$> get
         return $ getUnprefixedSymbol fs frameEnv unprefixedUses currentFrame (head names)
       else do
         prefixedUses <- prefixedUses <$> get
         return $ getPrefixedSymbol fs frameEnv prefixedUses (init names) (last names)


getSymbolM :: String -> RenamerM Symbol
getSymbolM name =
    do msym <- lookupSymbolM name
       case msym of
         Left str -> throwError str
         Right t -> return t


getFnSymbolM :: String -> RenamerM String
getFnSymbolM name =
    do sym <- getSymbolM name
       case sym of
         FnSymbol name -> return name
         _ -> throwError $ "name " ++ show name ++ " is not a function"


getTypeSymbolM :: String -> RenamerM String
getTypeSymbolM name =
    do sym <- getSymbolM name
       case sym of
         TypeSymbol name -> return name
         _ -> throwError $ "name " ++ show name ++ " is not a type"


addSymbolM :: String -> Symbol -> RenamerM ()
addSymbolM name sym = checkShadowing name >> addSymbol name sym
    where checkShadowing name =
              do msym <- lookupSymbolM name
                 case msym of
                   Left _ -> return ()
                   Right _ -> throwError $ "name " ++ show name ++ " has already been defined"

          addSymbol name sym =
              do frameEnv <- frameEnv <$> get
                 currentFrame <- getCurrentFrameM
                 let frameEnv' = FrameEnv.addSymbol frameEnv currentFrame name sym
                 modify $ \state -> state { frameEnv = frameEnv' }


addFnSymbolM :: String -> String -> RenamerM ()
addFnSymbolM name rename = addSymbolM name (FnSymbol rename)


addTypeSymbolM :: String -> String -> RenamerM ()
addTypeSymbolM name rename = addSymbolM name (TypeSymbol rename)


withScopeM :: RenamerM a -> RenamerM a
withScopeM m =
    do frameEnv <- frameEnv <$> get
       currentFrame <- getCurrentFrameM
       let (frameEnv', frame') = FrameEnv.addFrame frameEnv currentFrame
       modify $ \state -> state { frameEnv = frameEnv'
                                , currentFrame = Frame.frameId frame' }
       val <- m
       modify $ \state -> state { currentFrame = Frame.frameId currentFrame }
       return val


withPrefixedScopeM :: [String] -> RenamerM a -> RenamerM a
withPrefixedScopeM prefix m =
    do (val, frame) <- withScopeM $ do
                         val <- withNamespace prefix m
                         frame <- getCurrentFrameM
                         return (val, frame)
       frameEnv <- frameEnv <$> get
       let frameEnv' = FrameEnv.addModuleFrame frameEnv prefix frame
       modify $ \state -> state { frameEnv = frameEnv' }
       return val


mkPatDefns :: Stx a -> [(String, [Stx a])] -> [Stx a]
mkPatDefns val = map (mkDefn `uncurry`)
    where mkDefn id mods =
              DefnStx Nothing NrDef id (foldAppStx val mods)


lambdaBody :: [a] -> [Pat a] -> Stx a -> Stx a
lambdaBody args pats body =
    let
        defns = [ (arg, patDefns pat) | arg <- args | pat <- pats ]
        defns' = concat [ mkPatDefns (IdStx arg) defns | (arg, defns) <- defns ]
    in
      if null defns' then
          body
      else
          WhereStx body defns'


expandLambda :: [([Pat String], Stx String)] -> String -> RenamerM (Stx String)
expandLambda ms blame =
    do let nargs = length $ fst $ maximumBy (\x y -> compare (length (fst x)) (length (fst y))) ms
       args <- replicateM nargs (genNameM "arg")
       let (patss, exprs) = unzip ms
           preds = map (combinePreds args) patss
           exprs' = zipWith (lambdaBody args) patss exprs
           ms' = zip preds exprs'
       renameOneM (lambdas args (CondStx ms' blame))
    where lambdas [] body = body
          lambdas (arg:args) body =
              LambdaStx arg Nothing (lambdas args body)

          applyPred arg pat =
              AppStx (patPred pat) (IdStx arg)

          alignArgs args pats =
              drop (length args - length pats) args

          combinePreds args pats =
              foldl1 andStx (zipWith applyPred (alignArgs args pats) pats)


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
renameAnnotatedLambdaM arg ann body =
    do argT <- getTypeSymbolM ann
       renameLambdaM arg (Just argT) body


renameNamespaceM :: Namespace String -> RenamerM (Namespace String)
renameNamespaceM (Namespace uses stxs) =
    do let (unprefixed, prefixed) = partition (null . snd) uses

       checkUniqueImports unprefixed prefixed
       checkUniqueQualifiers prefixed

       modify $ \state -> state { unprefixedUses = map fst unprefixed
                                , prefixedUses = Map.fromList (map swap prefixed) }

       withNslevel True $ do
         stxs' <- concat <$> mapM renameM stxs
         return $ Namespace uses stxs'

    where checkUniqueImports unprefixed prefixed
              | length (nub $ sort unprefixed) /= length unprefixed =
                  throwError "renameNamespaceM: duplicated 'use' forms"
              | length (nub $ sort $ map fst prefixed) /= length (map fst prefixed) =
                  throwError "renameNamespaceM: duplicated 'use' forms"
              | otherwise =
                  return ()

          checkUniqueQualifiers prefixed
              | length (nub $ sort $ map snd prefixed) /= length (map snd prefixed) =
                  throwError "renameNamespaceM: duplicated qualified in 'use' forms"
              | otherwise =
                  return ()


renameModuleM :: Namespace String -> RenamerM (Namespace String)
renameModuleM (Namespace uses stxs) =
    do let (unprefixed, prefixed) = partition (null . snd) uses

       -- edit: check that the new imports don't collide with the old ones
       -- checkUniqueImports unprefixed prefixed
       -- checkUniqueQualifiers prefixed

       modify $ \state -> state { unprefixedUses = unprefixedUses state ++ map fst unprefixed
                                , prefixedUses = prefixedUses state `Map.union` Map.fromList (map swap prefixed) }

       withNslevel True $
         Namespace uses . concat <$> mapM renameM stxs


renameOneM stx = fromSingleton <$> renameM stx


renameM :: Stx String -> RenamerM [Stx String]
renameM stx@(CharStx _) = return [stx]
renameM stx@(IntStx _) = return [stx]
renameM stx@(DoubleStx _) = return [stx]
renameM (SeqStx stxs) = (:[]) . SeqStx <$> mapM renameOneM stxs

renameM (IdStx name) =
    (:[]) . IdStx <$> getFnSymbolM name

renameM (AppStx stx1 stx2) =
    (:[]) <$> ((AppStx <$> renameOneM stx1) `ap` renameOneM stx2)

renameM (CondMacro ms blame) =
    (:[]) <$> expandLambda ms blame

renameM (CondStx ms blame) =
    (:[]) . (`CondStx` blame) <$> mapM renameMatch ms
    where renameMatch (stx1, stx2) =
              do stx1' <- renameOneM stx1
                 stx2' <- renameOneM stx2
                 return (stx1', stx2')

renameM (DefnStx ann Def name body) =
    do let fvars = freeVars body
       if name `elem` fvars
       then do
         name' <- genNameM name
         addFnSymbolM name name'
         (:[]) . DefnStx ann Def name' <$>
           withNslevel False (renameOneM body)
       else
           -- edit: do let !_ | trace ("renameM: " ++ show name ++ " is nrdef") True = True
           renameM (DefnStx ann NrDef name body)

renameM (DefnStx ann NrDef name body) =
    do name' <- genNameM name
       body' <- withNslevel False (renameOneM body)
       addFnSymbolM name name'
       return [DefnStx ann NrDef name' body']

renameM (LambdaMacro typePats body) =
    renameM (lambdas typePats body)
    where lambdas [] body = body
          lambdas (pat:pats) body =
              let
                  arg = fst (head (patDefns pat))
                  IdStx ann = patPred pat
              in
                LambdaStx arg (Just ann) (lambdas pats body)

renameM (LambdaStx arg Nothing body) =
    (:[]) <$> renameUnannotatedLambdaM arg body

renameM (LambdaStx arg (Just ann) body) =
    (:[]) <$> renameAnnotatedLambdaM arg ann body

renameM stx@(ModuleStx [] ns) =
    do Namespace _ stxs <- renameModuleM ns
       return stxs

renameM stx@(ModuleStx prefix ns) =
    do Namespace _ stxs <- withPrefixedScopeM prefix (renameModuleM ns)
       return stxs

renameM (TypeStx name stxs) =
    do name' <- show <$> genNumM
       addTypeSymbolM name name'
       mapM renameOneM stxs

renameM (TypeMkStx name) =
    (:[]) . TypeMkStx <$> getTypeSymbolM name

renameM TypeUnStx =
    return [TypeUnStx]

renameM (TypeIsStx name) =
    (:[]) . TypeIsStx <$> getTypeSymbolM name

renameM (WhereStx stx stxs) =
    withScopeM $ do
      stxs' <- concat <$> mapM renameM stxs
      stx' <- withScopeM $ renameOneM stx
      return [WhereStx stx' stxs']


renameSrcFile :: FileSystem -> Namespace String -> Either String (Namespace String, Map String Symbol)
renameSrcFile fs ns =
  do (ns', state') <- runStateT (renameNamespaceM ns) (initialRenamerState fs)
     let symbols = Frame.symbols $ FrameEnv.getRootFrame $ frameEnv state'
     return (ns', symbols)


rename :: FileSystem -> SrcFile -> Either String SrcFile
rename _ srcfile@SrcFile { t = CoreT } =
    return srcfile

rename fs srcfile@SrcFile { t = SrcT, srcNs = Just ns } =
    do (ns', syms) <- renameSrcFile fs ns
       return (SrcFile.addDefinitionSymbols srcfile syms) { renNs = Just ns' }

rename fs srcfile@SrcFile { t = InteractiveT, srcNs = Just ns } =
    do (ns', syms) <- renameSrcFile interactiveFs interactiveNs
       let syms' = syms `Map.union` SrcFile.symbols srcfile
       return (SrcFile.addDefinitionSymbols srcfile syms') { renNs = Just ns' }
    where interactiveNs =
              let SrcFile { srcNs = Just (Namespace uses stxs) } = srcfile in
              Namespace (uses ++ [(SrcFile.name srcfile, "")]) stxs

          interactiveFs =
              FileSystem.add fs srcfile { srcNs = Just interactiveNs }