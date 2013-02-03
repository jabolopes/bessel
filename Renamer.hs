{-# LANGUAGE DoRec, NamedFieldPuns #-}
module Renamer where

import Control.Monad.Error
import Control.Monad.State
import Data.Functor ((<$>))
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map (keys, lookup)
import Data.Maybe (fromJust)

import Data.Frame (Frame)
import qualified Data.Frame as Frame (frameId)
import Data.FrameEnv (FrameEnv)
import qualified Data.FrameEnv as FrameEnv
import Data.SrcFile (SrcFile(..))
import qualified Data.SrcFile as SrcFile (name, frame)
import Data.Stx
import Data.Symbol
import Utils (split)


data RenamerState =
    RenamerState { fs :: Map String SrcFile
                 , frameEnv :: FrameEnv
                 , currentFrame :: Int
                 , currentCount :: Int
                 , currentNamespace :: String
                 , nslevel :: Bool }


initialRenamerState :: Map String SrcFile -> String -> RenamerState
initialRenamerState fs ns =
    let frameEnv = FrameEnv.empty in
    RenamerState { fs = fs
                 , frameEnv = frameEnv
                 , currentFrame = FrameEnv.rootId frameEnv
                 , currentCount = 0
                 , currentNamespace = ns
                 , nslevel = True }


srcfileRenamerState :: Map String SrcFile -> SrcFile -> RenamerState
srcfileRenamerState fs srcfile =
    let frameEnv = fromJust (SrcFile.frame srcfile) in
    RenamerState { fs = fs
                 , frameEnv = frameEnv
                 , currentFrame = FrameEnv.rootId frameEnv
                 , currentCount = 0
                 , currentNamespace = SrcFile.name srcfile
                 , nslevel = True }



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
         return $ ns ++ "." ++ name
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
       modify $ \state -> state { currentNamespace = ns' ++ "." ++ intercalate "." ns }
       val <- m
       modify $ \state -> state { currentNamespace = ns' }
       return val


getSymbol :: FrameEnv -> Frame -> String -> RenamerM Symbol
getSymbol frameEnv frame name =
    case FrameEnv.getLexicalSymbol frameEnv frame name of
      Nothing -> throwError $ "name " ++ show name ++ " is not defined"
      Just sym -> return sym


-- edit: check if name is multiply defined (i.e., shadowing)
getUnprefixedSymbol :: FrameEnv -> Frame -> String -> RenamerM Symbol
getUnprefixedSymbol frameEnv currentFrame name =
    get $ currentFrame:FrameEnv.getUnprefixedFrames frameEnv
    where get [frame] = getSymbol frameEnv frame name
          get (frame:frames) =
              case FrameEnv.getLexicalSymbol frameEnv frame name of
                Nothing -> get frames
                Just sym -> return sym


getPrefixedSymbol :: FrameEnv -> [String] -> String -> RenamerM Symbol
getPrefixedSymbol env prefix name =
    maybe getNamespaceSymbol return getModuleSymbol
    where getModuleSymbol =
              do fr <- FrameEnv.getModuleFrame env prefix
                 FrameEnv.getFrameSymbol env fr name

          getNamespace _ [] = getCurrentFrameM
          getNamespace frameEnv prefix =
              case FrameEnv.getPrefixedFrame frameEnv prefix of
                Nothing -> throwError $ "namespace " ++ show (intercalate "." prefix) ++ " is not defined"
                Just fr -> return fr

          getNamespaceSymbol =
              do fr' <- getNamespace env prefix
                 getUnprefixedSymbol env fr' name


getSymbolM :: [String] -> RenamerM Symbol
getSymbolM names =
    do frameEnv <- frameEnv <$> get
       currentFrame <- getCurrentFrameM
       if null (tail names)
       then getUnprefixedSymbol frameEnv currentFrame (head names)
       else getPrefixedSymbol frameEnv (init names) (last names)


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


useNamespaceM :: String -> String -> RenamerM ()
useNamespaceM prefix name =
    do frameEnv <- frameEnv <$> get
       checkShadowing frameEnv name
       frameEnv' <- linkFrameEnv frameEnv prefix name
       modify $ \state -> state { frameEnv = frameEnv' }
    where checkShadowing env name =
              -- edit: eliminate split
              case FrameEnv.getPrefixedFrame env (split '.' name) of
                Nothing -> return ()
                _ -> throwError $ "namespace qualification " ++ show name ++ " has already been defined"

          linkFrameEnv frameEnv prefix name =
              do fs <- fs <$> get
                 case Map.lookup name fs of
                   Nothing -> throwError $ "Renamer.useNamespaceM: srcfile is not in the filesystem" ++
                                           "\n\n\t prefix = " ++ prefix ++
                                           "\n\n\t name = " ++ name ++
                                           "\n\n\t fs = " ++ show fs ++ "\n"
                   Just srcfile -> do let moduleEnv = fromJust (SrcFile.frame srcfile)
                                      if null prefix
                                      then return $ FrameEnv.addUnprefixedEnv frameEnv name moduleEnv
                                      else return $ FrameEnv.addPrefixedEnv frameEnv prefix moduleEnv


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
renameNamespaceM stx@(Namespace uses stxs) =
    do mapM_ (\(ns, prefix) -> useNamespaceM prefix ns) uses
       withNslevel True $
         Namespace uses . concat <$> mapM renameM stxs


fromSingleton [x] = x

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
    do Namespace _ stxs <- renameNamespaceM ns
       return stxs

renameM stx@(ModuleStx prefix ns) =
    do Namespace _ stxs <- withPrefixedScopeM prefix (renameNamespaceM ns)
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


mkInteractiveFrame :: Map String SrcFile -> [String] -> FrameEnv
mkInteractiveFrame fs deps =
    case runStateT mkInteractiveFrameM (initialRenamerState fs "Interactive") of
      Left str -> error str
      Right (x, _) -> x
    where mkInteractiveFrameM =
              do mapM_ (useNamespaceM "") deps
                 frameEnv <$> get


renameSrcFile :: SrcFile -> RenamerM SrcFile
renameSrcFile srcfile@SrcFile { srcNs = Left ns } =
    do ns' <- renameNamespaceM ns
       return $ srcfile { renNs = Just ns' }

renameSrcFile srcfile@SrcFile { srcNs = Right (_, binds) } =
    do mapM_ (\name -> addFnSymbolM name name) $ Map.keys binds
       return srcfile


rename :: Map String SrcFile -> SrcFile -> Either String SrcFile
rename fs srcfile =
    do let state = initialRenamerState fs (SrcFile.name srcfile)
       (srcfile', state') <- runStateT (renameSrcFile srcfile) state
       return $ srcfile' { frame = Just (frameEnv state') }


renameInteractive :: Map String SrcFile -> SrcFile -> Stx String -> Either String (Stx String)
renameInteractive fs srcfile stx =
    fst <$> runStateT (renameOneM stx) (srcfileRenamerState fs srcfile)