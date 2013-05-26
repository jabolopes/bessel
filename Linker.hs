module Linker where

import Control.Monad.State
import Data.Functor ((<$>))
import Data.Map (Map)
import qualified Data.Map as Map

import Data.SrcFile
import qualified Data.SrcFile as SrcFile
import Data.Stx
import Data.Symbol


data LinkerState =
    LinkerState { typeIds :: Map String Int }


emptyLinkerState :: LinkerState
emptyLinkerState = LinkerState { typeIds = Map.empty }


type LinkerM a = State LinkerState a


genTypeId :: String -> LinkerM Int
genTypeId name =
    do typeIds <- typeIds <$> get
       let n = Map.size typeIds
           typeIds' = Map.insert name n typeIds
       modify $ \s -> s { typeIds = typeIds' }
       return n


ensureTypeIdM :: String -> LinkerM Int
ensureTypeIdM name =
    do typeIds <- typeIds <$> get
       case Map.lookup name typeIds of
         Nothing -> genTypeId name
         Just id -> return id


linkM :: Stx a -> LinkerM (Stx a)
linkM stx@(CharStx _) = return stx
linkM stx@(IntStx _) = return stx
linkM stx@(DoubleStx _) = return stx
linkM stx@(SeqStx _) = return stx
linkM stx@(IdStx _) = return stx

linkM (AppStx stx1 stx2) =
    do stx1' <- linkM stx1
       AppStx stx1' <$> linkM stx2

linkM (CondMacro _ _) =
    error "Linker.link(CondMacro): cond macros must be eliminated by the renamer"

linkM (CondStx stxs blame) =
    do stxs' <- mapM linkPair stxs
       return $ CondStx stxs' blame
    where linkPair (stx1, stx2) =
              do stx1' <- linkM stx1
                 stx2' <- linkM stx2
                 return (stx1', stx2')

linkM (DefnStx ann kw name body) =
    DefnStx ann kw name <$> linkM body

linkM (LambdaMacro _ _) =
    error "Linker.link(LambdaMacro): lambda macros must be eliminated by the renamer"

linkM (LambdaStx name ann body) =
    LambdaStx name ann <$> linkM body

linkM (ModuleStx name ns) =
    ModuleStx name <$> linkNamespaceM ns

linkM (CotypeStx _ _) =
    error "Linker.link(CotypeStx): types must be eliminated by the renamer"

linkM (CotypeMkStx name) =
    CotypeMkOp <$> ensureTypeIdM name

linkM stx@CotypeUnStx = return stx

linkM (CotypeIsStx name) =
    CotypeIsOp <$> ensureTypeIdM name

linkM (WhereStx stx stxs) =
    do stx' <- linkM stx
       WhereStx stx' <$> mapM linkM stxs


linkNamespaceM :: Namespace a -> LinkerM (Namespace a)
linkNamespaceM (Namespace uses stxs) =
    Namespace uses <$> mapM linkM stxs


linkSrcFileM :: SrcFile -> LinkerM SrcFile
linkSrcFileM srcfile@SrcFile { t = CoreT } =
    do let typeSyms = [ name | (name, TypeSymbol _) <- Map.toList (SrcFile.symbols srcfile) ]
       mapM ensureTypeIdM typeSyms
       return srcfile

linkSrcFileM srcfile@SrcFile { t = SrcT, renNs = Just renNs } =
    do lnkNs <- linkNamespaceM renNs
       return srcfile { lnkNs = Just lnkNs }

linkSrcFileM srcfile@SrcFile { t = InteractiveT, renNs = Just renNs } =
    do lnkNs <- linkNamespaceM renNs
       return srcfile { lnkNs = Just lnkNs }


linkSrcFiles :: [SrcFile] -> [SrcFile]
linkSrcFiles srcfiles =
    fst $ runState (mapM linkSrcFileM srcfiles) emptyLinkerState