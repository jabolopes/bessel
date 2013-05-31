module Linker where

import Control.Monad.State
import Data.Functor ((<$>))
import Data.Map (Map)
import qualified Data.Map as Map

import Data.SrcFile
import qualified Data.SrcFile as SrcFile
import Data.Stx
import Data.Symbol
import Monad.InterpreterM


data LinkerState = LinkerState { moduleCount :: Int }


emptyLinkerState :: LinkerState
emptyLinkerState = LinkerState { moduleCount = 0 }


type LinkerM a = State LinkerState a


genModuleIdM :: LinkerM Int
genModuleIdM =
    do mid <- moduleCount <$> get
       modify $ \s -> s { moduleCount = moduleCount s + 1 }
       return mid


linkNamespaceM :: Namespace a -> LinkerM (Namespace a)
linkNamespaceM (Namespace uses (stx:stxs)) =
    do mid <- genModuleIdM
       let DefnStx t kw name _ = stx
           stx' = DefnStx t kw name (IntStx mid)
       return $ Namespace uses (stx':stxs)


linkSrcFileM :: SrcFile -> LinkerM SrcFile
linkSrcFileM srcfile@SrcFile { t = CoreT } =
    do mid <- genModuleIdM
       let exprs = Map.fromList [(SrcFile.name srcfile ++ ".moduleId", IntExpr mid)]
       return $ SrcFile.addDefinitionExprs srcfile exprs

linkSrcFileM srcfile@SrcFile { renNs = Just renNs } =
    do lnkNs <- linkNamespaceM renNs
       return srcfile { lnkNs = Just lnkNs }


linkSrcFiles :: [SrcFile] -> [SrcFile]
linkSrcFiles srcfiles =
    fst $ runState (mapM linkSrcFileM srcfiles) emptyLinkerState
