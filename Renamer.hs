{-# LANGUAGE DoRec, NamedFieldPuns #-}
module Renamer where

import Control.Monad.Error
import Control.Monad.State
import Data.Functor ((<$>))
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map ((!), empty, insert, keys, toList)
import Data.Maybe (fromJust)

import Data.Frame
import qualified Data.Frame as Frame (frameId)
import Data.FrameEnv (FrameEnv (..))
import qualified Data.FrameEnv as FrameEnv
import Data.SrcFile (SrcFile(..))
import qualified Data.SrcFile as SrcFile (frame)
import Data.Stx
import Data.Symbol
import Utils (split)


data RenamerState =
    RenamerState { frameEnv :: FrameEnv
                 , currentFrame :: Int
                 , srcfileFrames :: Map String Int
                 , currentCount :: Int
                 , currentNamespace :: String
                 , nslevel :: Bool }


initialRenamerState :: RenamerState
initialRenamerState =
    let frameEnv = FrameEnv.empty in
    RenamerState { frameEnv = frameEnv
                 , currentFrame = FrameEnv.rootId frameEnv
                 , srcfileFrames = Map.empty
                 , currentCount = 0
                 , currentNamespace = ""
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
-- genNameM name = (name ++) . show <$> genNumM


withNslevel :: Bool -> RenamerM a -> RenamerM a
withNslevel l m =
    do l' <- nslevel <$> get
       modify $ \state -> state { nslevel = l }
       val <- m
       modify $ \state -> state { nslevel = l' }
       return val


withNamespace :: String -> RenamerM a -> RenamerM a
withNamespace ns m =
    do ns' <- currentNamespace <$> get
       modify $ \state -> state { currentNamespace = ns' ++ "." ++ ns }
       val <- m
       modify $ \state -> state { currentNamespace = ns' }
       return val


getSymbolM :: [String] -> RenamerM Symbol
getSymbolM names =
    do frameEnv <- frameEnv <$> get
       currentNamespace <- currentNamespace <$> get
       currentFrame <- getCurrentFrameM
       if null (tail names)
       then getUnprefixedSymbol frameEnv currentNamespace currentFrame (head names)
       else getPrefixedSymbol frameEnv currentNamespace currentFrame (init names) (last names)
    where getSymbol :: FrameEnv -> Frame -> String -> RenamerM Symbol
          getSymbol env fr name =
              case FrameEnv.getSymbol env fr name of
                Nothing -> throwError $ "name " ++ show name ++ " is undefined"
                Just sym -> return sym

          getUnprefixedSymbol :: FrameEnv -> String -> Frame -> String -> RenamerM Symbol
          getUnprefixedSymbol env ns fr name =
              let frs = fr:FrameEnv.getUnprefixedFrames env ns in get frs
              where get [fr] = getSymbol env fr name
                    get (fr:frs) =
                        getSymbol env fr name `catchError` (\_ -> get frs)

          getNamespace :: FrameEnv -> String -> [String] -> RenamerM Frame
          getNamespace _ _ [] = getCurrentFrameM
          getNamespace env ns prefix =
              case FrameEnv.getPrefixedFrame env ns prefix of
                Nothing -> throwError $ "namespace " ++ show (intercalate "." prefix) ++ " is undefined" ++
                                        "\n\n\t in name " ++ show (intercalate "." prefix) ++ "\n\n"
                Just fr -> return fr

          getPrefixedSymbol :: FrameEnv -> String -> Frame -> [String] -> String -> RenamerM Symbol
          getPrefixedSymbol env ns fr prefix name =
              do fr' <- getNamespace env ns prefix
                 getUnprefixedSymbol env ns fr' name


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
       currentNamespace <- currentNamespace <$> get
       checkShadowing frameEnv currentNamespace name
       frame <- FrameEnv.getFrame frameEnv . (Map.! name) . srcfileFrames <$> get
       let frameEnv' | null prefix = FrameEnv.addUnprefixedFrame frameEnv currentNamespace frame
                     | otherwise = FrameEnv.addPrefixedFrame frameEnv currentNamespace prefix frame

       modify $ \state -> state { frameEnv = frameEnv' }
    where checkShadowing env ns name =
              -- edit: eliminate split
              case FrameEnv.getPrefixedFrame env ns (split '.' name) of
                Nothing -> return ()
                _ -> throwError $ "namespace qualification " ++ show name ++ " has already been defined"


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


withPrefixedScopeM :: String -> RenamerM a -> RenamerM a
withPrefixedScopeM prefix m =
    do (val, frame) <- withScopeM $ do
                         val <- withNamespace prefix m
                         frame <- getCurrentFrameM
                         return (val, frame)
       frameEnv <- frameEnv <$> get
       currentNamespace <- currentNamespace <$> get
       let frameEnv' = FrameEnv.addPrefixedFrame frameEnv currentNamespace prefix frame
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

renameM stx@(ModuleStx "" ns) =
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


mkInteractiveFrame :: SrcFile -> RenamerState -> RenamerState
mkInteractiveFrame srcfile@(SrcFile name deps _ _) state =
    case runStateT mkInteractiveFrameM state of
      Left str -> error str
      Right (_, x) -> x
    where mkInteractiveFrameM =
              do frameEnv <- frameEnv <$> get
                 let (frameEnv', interactiveFrame) = FrameEnv.addFrame frameEnv $ FrameEnv.getRootFrame frameEnv
                 modify $ \state -> state { frameEnv = frameEnv'
                                          , currentFrame = frameId interactiveFrame
                                          , currentNamespace = "Interactive" }
                 mapM_ (useNamespaceM "") (name:reverse deps)


-- edit: fix type signature
saveSrcfile name deps frame content =
    do let frameId = Frame.frameId frame
       modify $ \state -> state { srcfileFrames = Map.insert name frameId (srcfileFrames state) }
       return $ SrcFile name deps (Just frameId) content


renameSrcFiles :: [SrcFile] -> RenamerM [SrcFile]
renameSrcFiles = mapM renameSrcFile
    where renameSrcFile (SrcFile name deps Nothing (Left ns)) =
              do (ns', frame) <- withScopeM $ do
                                   modify $ \state -> state { currentNamespace = name }
                                   ns' <- renameNamespaceM ns
                                   frame <- getCurrentFrameM
                                   return (ns', frame)
                 saveSrcfile name deps frame $ Left ns'

          renameSrcFile srcfile@(SrcFile name deps Nothing content@(Right (_, binds))) =
              do frame <- withScopeM $ do
                            modify $ \state -> state { currentNamespace = name }
                            mapM_ (\name -> addFnSymbolM name name) $ Map.keys binds
                            getCurrentFrameM
                 saveSrcfile name deps frame content
                 

rename :: [SrcFile] -> Either String ([SrcFile], RenamerState)
rename srcfiles =
    runStateT (renameSrcFiles srcfiles) initialRenamerState


renameIncremental :: RenamerState -> Stx String -> Either String (Stx String, RenamerState)
renameIncremental state stx =
    runStateT (renameOneM stx) state