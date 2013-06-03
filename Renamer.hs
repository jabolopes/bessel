{-# LANGUAGE BangPatterns, NamedFieldPuns, ParallelListComp #-}
module Renamer where

import Control.Monad.Error (throwError)
import Control.Monad.State
import Data.Functor ((<$>))
import Data.List (intercalate, maximumBy, nub, partition, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)

import Data.FileSystem (FileSystem)
import qualified Data.FileSystem as FileSystem (get)
import Data.Frame (Frame)
import qualified Data.Frame as Frame (fid)
import Data.FrameTree (FrameTree)
import qualified Data.FrameTree as FrameTree
import Data.SrcFile (SrcFileT (..), SrcFile(..))
import qualified Data.SrcFile as SrcFile (name, symbols, addDefinitionSymbols)
import Data.Stx
import Data.Symbol (Symbol (..))
import Data.Tuple (swap)
import Data.Type
import Utils (split)


data RenamerState =
    RenamerState { fs :: FileSystem
                 , srcfile :: SrcFile
                 , frameTree :: FrameTree
                 , nameCount :: Int
                 , typeCount :: Int
                 , currentFrame :: Int
                 , currentNamespace :: String
                 , nslevel :: Bool
                 , unprefixedUses :: [String]
                 , prefixedUses :: Map String String }


initialRenamerState :: FileSystem -> SrcFile -> RenamerState
initialRenamerState fs srcfile =
    let frameTree = FrameTree.empty in
    RenamerState { fs = fs
                 , srcfile = srcfile
                 , frameTree = frameTree
                 , nameCount = 0
                 , typeCount = 0
                 , currentFrame = FrameTree.rootId frameTree
                 , currentNamespace = ""
                 , nslevel = True
                 , unprefixedUses = []
                 , prefixedUses = Map.empty }


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


genTypeIdM :: RenamerM Int
genTypeIdM =
    do count <- typeCount <$> get
       modify $ \s -> s { typeCount = count + 1 }
       return count


getModuleFrame :: FrameTree -> Frame -> [String] -> Maybe Frame
getModuleFrame tree currentFrame longName =
    loop (FrameTree.getLexicalSymbol tree) currentFrame longName
    where loop _ frame [] = Just frame
          loop fn frame (name:longName) =
            case fn frame name of
              Nothing -> Nothing
              Just (ModuleSymbol fid) -> let Just frame = FrameTree.getFrame tree fid in
                                         loop FrameTree.getFrameSymbol frame longName
              _ -> Nothing


getModuleSymbol :: FrameTree -> Frame -> [String] -> String -> Maybe Symbol
getModuleSymbol tree currentFrame prefix name =
    do frame <- getModuleFrame tree currentFrame prefix
       FrameTree.getFrameSymbol frame name


lookupLocalSymbol :: FrameTree -> Frame -> String -> Maybe Symbol
lookupLocalSymbol tree currentFrame name =
    case split '.' name of
      [name] -> FrameTree.getLexicalSymbol tree currentFrame name
      names -> getModuleSymbol tree currentFrame (init names) (last names)


lookupUseSymbol :: FileSystem -> [String] -> Map String String -> String -> [Symbol]
lookupUseSymbol fs unprefixedUses prefixedUses name =
    let
        unprefixedSyms = map (SrcFile.symbols . FileSystem.get fs) unprefixedUses
        prefixedSyms = map (\(name, prefix) -> Map.mapKeys ((prefix ++ ".") ++) $ SrcFile.symbols $ FileSystem.get fs name) (Map.toList prefixedUses)
    in
      catMaybes [ Map.lookup name syms | syms <- unprefixedSyms ++ prefixedSyms ]


lookupSymbol
    :: FileSystem
    -> FrameTree
    -> Frame
    -> [String]
    -> Map String String
    -> String
    -> Either String Symbol
lookupSymbol fs tree currentFrame unprefixedUses prefixedUses name =
    case lookupLocalSymbol tree currentFrame name of
      Just x -> Right x
      Nothing -> case lookupUseSymbol fs unprefixedUses prefixedUses name of
                   [] -> Left $ "name " ++ show name ++ " is not defined"
                   [sym] -> Right sym
                   _ -> Left $ "name " ++ show name ++ " is multiply defined"


lookupSymbolM :: String -> RenamerM (Either String Symbol)
lookupSymbolM name =
    do fs <- fs <$> get
       tree <- frameTree <$> get
       currentFrame <- getCurrentFrameM
       unprefixedUses <- unprefixedUses <$> get
       prefixedUses <- prefixedUses <$> get
       return $ lookupSymbol fs tree currentFrame unprefixedUses prefixedUses name


getSymbolM :: String -> RenamerM Symbol
getSymbolM name =
    do msym <- lookupSymbolM name
       case msym of
         Left str -> throwError str
         Right t -> return t


getCotypeSymbolM :: String -> RenamerM (String, Int, [Observation])
getCotypeSymbolM name =
    do sym <- getSymbolM name
       case sym of
         CotypeSymbol ns tid obs -> return (ns, tid, obs)
         _ -> throwError $ "name " ++ show name ++ " is not a coindunctive type"


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
                   Left _ -> return ()
                   Right _ -> throwError $ "name " ++ show name ++ " is already defined"

          addSymbol name sym =
              do tree <- frameTree <$> get
                 currentFrame <- getCurrentFrameM
                 let tree' = FrameTree.addSymbol tree currentFrame name sym
                 modify $ \s -> s { frameTree = tree' }


addCotypeSymbolM :: String -> String -> Int -> [Observation] -> RenamerM ()
addCotypeSymbolM name ns tid obs = addSymbolM name (CotypeSymbol ns tid obs)


addFnSymbolM :: String -> String -> RenamerM ()
addFnSymbolM name = addSymbolM name . FnSymbol


addModuleSymbolM :: String -> Int -> RenamerM ()
addModuleSymbolM name = addSymbolM name . ModuleSymbol


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


withPrefixedScopeM :: [String] -> RenamerM a -> RenamerM a
withPrefixedScopeM longName m =
    addLongModuleSymbolM longName
    where addLongModuleSymbolM [] = withNamespace longName m
          addLongModuleSymbolM (name:longName) =
              do (val, frame) <- withScopeM $ do
                                   frame <- getCurrentFrameM
                                   val <- addLongModuleSymbolM longName
                                   return (val, frame)
                 addModuleSymbolM name (Frame.fid frame)
                 return val


withNamespace :: [String] -> RenamerM a -> RenamerM a
withNamespace ns m =
    do ns' <- currentNamespace <$> get
       let longName | null ns' = intercalate "." ns
                    | otherwise = ns' ++ "." ++ intercalate "." ns
       modify $ \s -> s { currentNamespace = longName }
       val <- m
       modify $ \s -> s { currentNamespace = ns' }
       return val


withNslevel :: Bool -> RenamerM a -> RenamerM a
withNslevel l m =
    do l' <- nslevel <$> get
       modify $ \s -> s { nslevel = l }
       val <- m
       modify $ \s -> s { nslevel = l' }
       return val


withUsesM :: [String] -> Map String String -> RenamerM a -> RenamerM a
withUsesM unprefixed prefixed m =
    do unprefixed' <- unprefixedUses <$> get
       prefixed' <- prefixedUses <$> get
       modify $ \s -> s { unprefixedUses = unprefixedUses s ++ unprefixed
                        , prefixedUses = prefixedUses s `Map.union` prefixed }
       val <- m
       modify $ \s -> s { unprefixedUses = unprefixed'
                        , prefixedUses = prefixed' }
       return val


-- lambda macros

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
renameAnnotatedLambdaM arg ann body = undefined
-- edit: remove undefined
-- renameAnnotatedLambdaM arg ann body =
--     do argT <- getTypeSymbolM ann
--        renameLambdaM arg (Just argT) body


-- /lambda macros


-- cotype macros

cotypeObservation :: Int -> Observation -> Int -> Stx String
cotypeObservation tid (name, obT) i =
    let
        t = ArrowT DynT obT
        isStx = appStx "is#" (IntStx tid)
        argPat = namePat "arg" (mkPredPat isStx)
        condBody = foldAppStx (IdStx "arg") [appStx "index" (IntStx i), IdStx "un#"]
        body = CondMacro [([argPat], condBody)] name
    in
      DefnStx (Just t) NrDef name body


cotypeObservations :: Int -> [Observation] -> [Stx String]
cotypeObservations tid obs = zipWith (cotypeObservation tid) obs [0..]

-- /cotype macros


renameNamespaceM :: Namespace String -> RenamerM (Namespace String)
renameNamespaceM (Namespace uses stxs) =
    do let (unprefixed, prefixed) = partition (null . snd) uses
       checkUniqueImports unprefixed prefixed
       checkUniqueQualifiers prefixed
       withNslevel True $
         withUsesM (map fst unprefixed) (Map.fromList (map swap prefixed)) $
           Namespace uses . concat <$> mapM renameM stxs

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

renameM (CondMacro ms blame) =
    (:[]) <$> expandLambda ms blame

renameM (CondStx ms blame) =
    (:[]) . (`CondStx` blame) <$> mapM renameMatch ms
    where renameMatch (stx1, stx2) =
              do stx1' <- renameOneM stx1
                 stx2' <- renameOneM stx2
                 return (stx1', stx2')

renameM (CotypeStx name obs) =
    do srcfile <- srcfile <$> get
       tid <- genTypeIdM
       addCotypeSymbolM name (SrcFile.name srcfile) tid obs
       concat <$> mapM renameM (cotypeObservations tid obs)

renameM (DefnStx ann Def name body) =
    do let fvars = freeVars body
       if name `elem` fvars
       then do
         name' <- genNameM name
         addFnSymbolM name name'
         (:[]) . DefnStx ann Def name' <$>
           withNslevel False (renameOneM body)
       else
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

renameM (MergeStx name vals) =
    do (ns, tid, obs) <- getCotypeSymbolM name
       _ <- checkObservations (map fst obs) (map fst vals)
       moduleId <- IdStx <$> getFnSymbolM (ns ++ ".moduleId")
       stxs <- mapM (renameOneM . snd) vals
       return $ (:[]) $ AppStx (AppStx (appStx "mk#" moduleId) (IntStx tid)) (SeqStx stxs)
    where checkObservations obs vals
              | obs == vals = return ()
              | otherwise = throwError $ "coinductive type " ++ show name ++
                                         " observations " ++ show obs ++
                                         " and initialization " ++ show vals ++
                                         " do not match"

renameM stx@(ModuleStx [] ns) =
    do Namespace _ stxs <- renameNamespaceM ns
       return stxs

renameM stx@(ModuleStx prefix ns) =
    do Namespace _ stxs <- withPrefixedScopeM prefix (renameNamespaceM ns)
       return stxs

renameM (WhereStx stx stxs) =
    withScopeM $ do
      stxs' <- concat <$> mapM renameM stxs
      stx' <- withScopeM (renameOneM stx)
      return [WhereStx stx' stxs']


extendNamespace :: String -> Namespace a -> Namespace a
extendNamespace name (Namespace uses stxs) =
    let
        defn = DefnStx (Just IntT) NrDef "moduleId" (IntStx 0)
        mdl = ModuleStx [name] (Namespace [] [defn])
    in
      Namespace uses (mdl:stxs)


renameSrcFile :: FileSystem -> SrcFile -> Namespace String -> Either String (Namespace String, Map String Symbol)
renameSrcFile fs srcfile ns =
  do (ns', state') <- runStateT (renameNamespaceM (extendNamespace (SrcFile.name srcfile) ns)) (initialRenamerState fs srcfile)
     return (ns', FrameTree.getSymbols (frameTree state'))


rename :: FileSystem -> SrcFile -> Either String SrcFile
rename _ srcfile@SrcFile { t = CoreT } =
    do let moduleIdName = SrcFile.name srcfile ++ ".moduleId"
           syms = Map.fromList [(moduleIdName, FnSymbol moduleIdName)]
       return $ SrcFile.addDefinitionSymbols srcfile syms

rename fs srcfile@SrcFile { srcNs = Just ns } =
    do (ns', syms) <- renameSrcFile fs srcfile ns
       return (SrcFile.addDefinitionSymbols srcfile syms) { renNs = Just ns' }
