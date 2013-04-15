{-# LANGUAGE NamedFieldPuns #-}
module Renamer where

import Control.Monad.Error
import Control.Monad.State
import Data.Functor ((<$>))
import Data.List (intercalate, nub, partition, sort)
import Data.Map (Map)
import qualified Data.Map as Map ((!), elems, empty, fromList, insert, keys, lookup, union)
import Data.Maybe (catMaybes, fromJust)

import Data.Frame (Frame)
import qualified Data.Frame as Frame (frameId, symbols)
import Data.FrameEnv (FrameEnv)
import qualified Data.FrameEnv as FrameEnv
import Data.SrcFile (SrcFileT (..), SrcFile(..))
import qualified Data.SrcFile as SrcFile (name, srcNs, symbols)
import Data.Stx
import Data.Symbol (Symbol (..))
import qualified Data.Symbol as Symbol
import Data.Tuple (swap)
import Utils (fromSingleton, split)


data RenamerState =
    RenamerState { fs :: Map String SrcFile
                 , frameEnv :: FrameEnv
                 , currentFrame :: Int
                 , currentCount :: Int
                 , currentNamespace :: String
                 , nslevel :: Bool
                 , unprefixedUses :: [String]
                 , prefixedUses :: Map String String }


initialRenamerState :: Map String SrcFile -> RenamerState
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
         -- return name
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


getUnprefixedSymbol :: String -> RenamerM Symbol
getUnprefixedSymbol name =
    do msym <- getLocalSymbol
       case msym of
         Nothing -> getNamespaceSymbol
         Just sym -> return sym
    where getLocalSymbol =
              do frameEnv <- frameEnv <$> get
                 currentFrame <- getCurrentFrameM
                 return $ FrameEnv.getLexicalSymbol frameEnv currentFrame name

          getNamespaceSymbol =
              do fs <- fs <$> get
                 unprefixedUses <- unprefixedUses <$> get
                 let srcfiles = map (fs Map.!) unprefixedUses
                     symss = map SrcFile.symbols srcfiles
                     syms = catMaybes [ Map.lookup name syms | syms <- symss ]
                 case syms of
                   [] -> throwError $ "name " ++ show name ++ " is not defined"
                   [sym] -> return sym
                   _ -> throwError $ "name " ++ show name ++ " is multiply defined"


getPrefixedSymbol :: [String] -> String -> RenamerM Symbol
getPrefixedSymbol prefix name =
    do frameEnv <- frameEnv <$> get
       case getModuleSymbol frameEnv of
         Nothing -> getNamespaceSymbol
         Just sym -> return sym
    where getModuleSymbol env =
              do fr <- FrameEnv.getModuleFrame env prefix
                 FrameEnv.getFrameSymbol env fr name

          getNamespaceSymbol =
              do prefixedUses <- prefixedUses <$> get
                 prefix' <- case Map.lookup (intercalate "." prefix) prefixedUses of
                               Nothing -> throwError $ "namespace or module " ++ show (intercalate "." prefix) ++ " is not defined"
                               Just x -> return x
                 fs <- fs <$> get
                 let syms = SrcFile.symbols (fs Map.! prefix')
                 case Map.lookup name syms of
                   Nothing -> throwError $ "name " ++ show (intercalate "." prefix ++ "." ++ name) ++ " is not defined"
                   Just sym -> return sym


getSymbolM :: [String] -> RenamerM Symbol
getSymbolM names =
    do frameEnv <- frameEnv <$> get
       currentFrame <- getCurrentFrameM
       if null (tail names)
       then getUnprefixedSymbol (head names)
       else getPrefixedSymbol (init names) (last names)


getFnSymbolM :: [String] -> RenamerM String
getFnSymbolM names =
    do sym <- getSymbolM names
       case sym of
         FnSymbol name -> return name
         _ -> throwError $ "name " ++ show (intercalate "." names) ++ " is not a function"


getTypeSymbolM :: [String] -> RenamerM String
getTypeSymbolM names =
    do sym <- getSymbolM names
       case sym of
         TypeSymbol name -> return name
         _ -> throwError $ "name " ++ show (intercalate "." names) ++ " is not a type"


addSymbolM :: String -> Symbol -> RenamerM ()
addSymbolM name sym = checkShadowing name >> addSymbol name sym
    where checkShadowing name =
              -- edit: fix this
              -- do getSymbolM [name] `catchError` const (return undefined)
              --    throwError $ "name " ++ show name ++ " has already been defined"
              return ()

          addSymbol name sym =
              do frameEnv <- frameEnv <$> get
                 currentFrame <- getCurrentFrameM
                 let frameEnv' = FrameEnv.addSymbol frameEnv currentFrame name sym
                 modify $ \state -> state { frameEnv = frameEnv' }


addFnSymbolM :: String -> String -> RenamerM ()
addFnSymbolM name rename = addSymbolM name $ FnSymbol rename


addTypeSymbolM :: String -> String -> RenamerM ()
addTypeSymbolM name rename = addSymbolM name $ TypeSymbol rename


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


renameNamespaceM :: Namespace String -> RenamerM (Namespace String)
renameNamespaceM (Namespace uses stxs) =
    do let (unprefixed, prefixed) = partition (null . snd) uses

       checkUniqueImports unprefixed prefixed
       checkUniqueQualifiers prefixed

       modify $ \state -> state { unprefixedUses = map fst unprefixed
                                , prefixedUses = Map.fromList $ map swap prefixed }

       withNslevel True $
         Namespace uses . concat <$> mapM renameM stxs

    where checkUniqueImports unprefixed prefixed
              | length (nub $ sort $ unprefixed) /= length unprefixed =
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
                                , prefixedUses = Map.union (prefixedUses state) (Map.fromList (map swap prefixed)) }

       withNslevel True $
         Namespace uses . concat <$> mapM renameM stxs


renameOneM stx = fromSingleton <$> renameM stx


renameM :: Stx String -> RenamerM [Stx String]
renameM stx@(CharStx _) = return [stx]
renameM stx@(IntStx _) = return [stx]
renameM stx@(DoubleStx _) = return [stx]
renameM (SeqStx stxs) = (:[]) . SeqStx <$> mapM renameOneM stxs

renameM (IdStx name) =
    (:[]) . IdStx <$> getFnSymbolM (split '.' name)

renameM (AppStx stx1 stx2) =
    (:[]) <$> ((AppStx <$> renameOneM stx1) `ap` renameOneM stx2)

renameM (CondStx ms blame) =
    (:[]) . (\ms -> CondStx ms blame) <$> mapM renameMatch ms
    where renameMatch (stx1, stx2) =
              do stx1' <- renameOneM stx1
                 stx2' <- renameOneM stx2
                 return (stx1', stx2')

renameM (DefnStx Def name body) =
    do name' <- genNameM name
       addFnSymbolM name name'
       (:[]) . DefnStx Def name' <$>
         withNslevel False (renameOneM body)

renameM (DefnStx NrDef name body) =
    do name' <- genNameM name
       body' <- withNslevel False (renameOneM body)
       addFnSymbolM name name'
       return [DefnStx NrDef name' body']

renameM (LambdaStx arg body) =
    do arg' <- genNameM arg
       (:[]) . LambdaStx arg' <$>
         withScopeM
           (do addFnSymbolM arg arg'
               withScopeM $ renameOneM body)

renameM stx@(ModuleStx [] ns) =
    do Namespace _ stxs <- renameModuleM ns
       return stxs

renameM stx@(ModuleStx prefix ns) =
    do Namespace _ stxs <- withPrefixedScopeM prefix (renameModuleM ns)
       return stxs

renameM (TypeStx name stxs) =
    do name' <- show <$> genNumM
       addTypeSymbolM name name'
       (:[]) . TypeStx name' <$> mapM renameOneM stxs

renameM (TypeMkStx name) =
    (:[]) . TypeMkStx <$> getTypeSymbolM [name]

renameM TypeUnStx =
    return [TypeUnStx]

renameM (TypeIsStx name) =
    (:[]) . TypeIsStx <$> getTypeSymbolM [name]

renameM (WhereStx stx stxs) =
    withScopeM $ do
      stxs' <- concat <$> mapM renameM stxs
      stx' <- withScopeM $ renameOneM stx
      return [WhereStx stx' stxs']


renameSrcFile :: Map String SrcFile -> Namespace String -> Either String (Namespace String, Map String Symbol)
renameSrcFile fs ns =
  do let state = initialRenamerState fs
     (ns', state') <- runStateT (renameNamespaceM ns) state
     let symbols = Frame.symbols $ FrameEnv.getRootFrame $ frameEnv state'
         renSymbols = Map.fromList [ (Symbol.name sym, sym) | sym <- Map.elems symbols ]
     return (ns', renSymbols)


rename :: Map String SrcFile -> SrcFile -> Either String SrcFile
rename _ srcfile@SrcFile { t = CoreT } =
    return srcfile

rename fs srcfile@SrcFile { t = SrcT, srcNs = Just ns } =
    do (ns', renSymbols) <- renameSrcFile fs ns
       return $ srcfile { symbols = renSymbols, renNs = Just ns' }

rename fs srcfile@SrcFile { t = InteractiveT, symbols, srcNs = Just ns } =
    do (ns', renSymbols) <- renameSrcFile interactiveFs interactiveNs
       return $ srcfile { symbols = Map.union renSymbols symbols, renNs = Just ns' }
    where interactiveNs =
              let SrcFile { srcNs = Just (Namespace uses stxs) } = srcfile in
              Namespace (uses ++ [(SrcFile.name srcfile, "")]) stxs

          interactiveFs =
              Map.insert (SrcFile.name srcfile) srcfile { srcNs = Just interactiveNs } fs