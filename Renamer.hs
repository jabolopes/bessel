{-# LANGUAGE BangPatterns, DoRec, NamedFieldPuns, RecordWildCards #-}
module Renamer where

import Control.Monad.Error
import Control.Monad.State
import Data.Functor ((<$>))
import Data.List (intercalate)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Frame
import Data.FrameEnv (FrameEnv (..))
import qualified Data.FrameEnv as FrameEnv
import Data.SrcFile
import Data.Stx
import Data.Symbol
import Utils


data RenamerState =
    RenamerState { frameEnv :: FrameEnv
                 , currentFrame :: Int
                 , currentCount :: Int
                 , currentSrcfiles :: Map String SrcFile }


initialRenamerState :: RenamerState
initialRenamerState =
    let frameEnv = FrameEnv.empty in
    RenamerState { frameEnv = frameEnv
                 , currentFrame = FrameEnv.rootId frameEnv
                 , currentCount = 0
                 , currentSrcfiles = Map.empty }


type RenamerM a = StateT RenamerState (Either String) a


getCurrentFrameM :: RenamerM Frame
getCurrentFrameM =
    do frameEnv <- frameEnv <$> get
       FrameEnv.getFrame frameEnv <$> currentFrame <$> get


putCurrentFrameM :: Frame -> RenamerM ()
putCurrentFrameM frame =
    do frameEnv <- frameEnv <$> get
       let frameEnv' = FrameEnv.putFrame frameEnv frame
       modify $ \state -> state { frameEnv = frameEnv'
                                , currentFrame = frameId frame }


genNumM :: RenamerM String
genNumM =
    do count <- currentCount <$> get
       modify $ \state -> state { currentCount = count + 1 }
       return $ show count


genNameM :: String -> RenamerM String
genNameM name =
    do count <- currentCount <$> get
       modify $ \state -> state { currentCount = count + 1 }
       return $ name ++ show count


getSymbolM fn names =
    do frameEnv <- frameEnv <$> get
       currentFrame <- getCurrentFrameM
       case FrameEnv.getModuleFrame frameEnv currentFrame $ init names of
         Nothing -> throwError $ "module not bound " ++ show (intercalate "." (init names))
         Just frame -> case fn frameEnv frame $ last names of
                         Nothing -> throwError $ "name not bound " ++ show (intercalate "." names)
                         Just symbol -> return symbol


getFnSymbolM :: [String] -> RenamerM String
getFnSymbolM names =
    do FnSymbol name <- getSymbolM FrameEnv.getFnSymbol names
       return name


getTypeSymbolM :: [String] -> RenamerM String
getTypeSymbolM names =
    do TypeSymbol name <- getSymbolM FrameEnv.getTypeSymbol names
       return name


addSymbolM :: String -> Symbol -> RenamerM ()
addSymbolM name sym =
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
                                , currentFrame = frameId frame' }
       val <- m
       modify $ \state -> state { currentFrame = frameId currentFrame }
       return val


bindModuleM :: String -> RenamerM ()
bindModuleM name =
    do frameEnv <- frameEnv <$> get
       currentFrame <- getCurrentFrameM

       -- edit: 'Nothing' instead of 'Just' means this module is being
       -- processed before its dependencies
       SrcFile _ _ (Just modFrame) _ <- ((Map.! name) . currentSrcfiles) <$> get

       let (frameEnv', modFrame') = FrameEnv.putFrameChild frameEnv currentFrame modFrame
       modify $ \state -> state { frameEnv = frameEnv'
                                , currentFrame = frameId modFrame' }


-- withModuleM :: String -> RenamerM a -> RenamerM a
-- withModuleM name m =
--     do currentFrame <- getCurrentFrameM
--        bindModuleM name
--        val <- withScopeM m
--        modify $ \state -> state { currentFrame = frameId currentFrame }
--        return val


withPrefixM :: String -> RenamerM a -> RenamerM a
withPrefixM prefix m =
    do frameEnv <- frameEnv <$> get
       currentFrame <- getCurrentFrameM
       let (frameEnv', modFrame) = FrameEnv.addFrame frameEnv currentFrame
           (frameEnv'', currentFrame') = FrameEnv.addModuleFrame frameEnv' currentFrame prefix modFrame
       modify $ \state -> state { frameEnv = frameEnv''
                                , currentFrame = frameId modFrame }
       val <- m
       modify $ \state -> state { currentFrame = frameId currentFrame' }
       return val


renameNamespaceM :: Namespace String -> RenamerM (Namespace String)
renameNamespaceM stx@(Namespace uses stxs) =
    do mapM_ bindModuleM uses
       Namespace uses <$> mapM renameM stxs


renameM :: Stx String -> RenamerM (Stx String)
renameM stx@(CharStx _) = return stx
renameM stx@(IntStx _) = return stx
renameM stx@(DoubleStx _) = return stx
renameM (SeqStx stxs) = SeqStx <$> mapM renameM stxs

renameM (IdStx name) =
    IdStx <$> getFnSymbolM (split '.' name)

renameM (AppStx stx1 stx2) =
    (AppStx <$> renameM stx1) `ap` renameM stx2

renameM (DefnStx Def name body) =
    do name' <- genNameM name
       addFnSymbolM name name'
       DefnStx Def name' <$> renameM body

renameM (DefnStx NrDef name body) =
    do name' <- genNameM name
       body' <- renameM body
       addFnSymbolM name name'
       return $ DefnStx NrDef name' body'

renameM (LambdaStx arg body) =
    do arg' <- genNameM arg
       LambdaStx arg' <$> (withScopeM $ do
                             addFnSymbolM arg arg'
                             withScopeM $ renameM body)

renameM stx@(ModuleStx "" ns) =
    ModuleStx "" <$> renameNamespaceM ns

renameM stx@(ModuleStx prefix ns) =
    ModuleStx prefix <$> (withPrefixM prefix $ renameNamespaceM ns)

renameM (TypeStx name stxs) =
    do name' <- genNameM name
       addTypeSymbolM name name'
       TypeStx name' <$> mapM renameM stxs

renameM (TypeMkStx name arg) =
    do name' <- getTypeSymbolM [name]
       arg' <- getFnSymbolM [arg]
       return $ TypeMkStx name' arg'

renameM (TypeUnStx name arg) =
    do name' <- getTypeSymbolM [name]
       arg' <- getFnSymbolM [arg]
       return $ TypeUnStx name' arg'

renameM (TypeIsStx name arg) =
    do name' <- getTypeSymbolM [name]
       arg' <- getFnSymbolM [arg]
       return $ TypeIsStx name' arg'

renameM (WhereStx stx stxs) =
    withScopeM $ do
      stxs' <- mapM renameM stxs
      stx' <- withScopeM $ renameM stx
      return $ WhereStx stx' stxs'


mkInteractiveFrame :: SrcFile -> RenamerState -> RenamerState
mkInteractiveFrame srcfile@(SrcFile name deps _ _) state =
    case runStateT mkInteractiveFrameM state of
      Left str -> error str
      Right (_, x) -> x
    where modNames = deps ++ [name]

          mkInteractiveFrameM =
              do frameEnv <- frameEnv <$> get
                 currentFrame <- getCurrentFrameM
                 let (frameEnv', frame) = FrameEnv.addFrame frameEnv currentFrame
                 modify $ \state -> state { frameEnv = frameEnv'
                                          , currentFrame = frameId frame }
                 mapM_ bindModuleM modNames


saveSrcfile :: SrcFile -> RenamerM SrcFile
saveSrcfile srcfile@(SrcFile name _ _ _) =
    do modify $ \state -> state { currentSrcfiles = Map.insert name srcfile (currentSrcfiles state) }
       return srcfile


renameSrcFiles :: [SrcFile] -> RenamerM [SrcFile]
renameSrcFiles srcfiles = mapM renameSrcFile srcfiles
    where renameSrcFile (SrcFile name deps Nothing (Left ns)) =
              do (ns', frame) <- withScopeM $ do
                                   ns' <- renameNamespaceM ns
                                   frame <- getCurrentFrameM
                                   return (ns', frame)
                 saveSrcfile $ SrcFile name deps (Just frame) $ Left ns'

          renameSrcFile srcfile@(SrcFile name deps Nothing content@(Right binds)) =
              do frame <- withScopeM $ do
                            mapM_ (\name -> addFnSymbolM name name) $ Map.keys binds
                            frame <- getCurrentFrameM
                            return frame
                 saveSrcfile $ SrcFile name deps (Just frame) content
                 

rename :: [SrcFile] -> Either String ([SrcFile], RenamerState)
rename srcfiles =
    runStateT (renameSrcFiles srcfiles) initialRenamerState


renameIncremental :: RenamerState -> Stx String -> Either String (Stx String, RenamerState)
renameIncremental state stx =
    runStateT (renameM stx) state