module Data.SrcFile where

import Data.Map (Map)
import qualified Data.Map as Map (empty)

import qualified Data.Env as Env
import Data.FrameEnv
import Data.Stx
import Data.Symbol
import Data.Type
import Monad.InterpreterM


data SrcFile
    = SrcFile { name :: String
              , deps :: [String]
              , frame :: Maybe FrameEnv
              , symbols :: Map String Symbol
              , ts :: Map String Type
              , env :: ExprEnv
              , srcNs :: (Either (Namespace String) ([String], Map String (Type, Expr)))
              , renNs :: Maybe (Namespace String) }
      deriving (Show)


initial name srcNs =
    SrcFile { name = name
            , deps = []
            , frame = Nothing
            , symbols = Map.empty
            , ts = Map.empty
            , env = Env.empty
            , srcNs = srcNs
            , renNs = Nothing }


mkParsedSrcFile :: String -> Namespace String -> SrcFile
mkParsedSrcFile name ns =
    (initial name (Left ns))


mkInteractiveSrcFile :: [String] -> FrameEnv -> SrcFile
mkInteractiveSrcFile deps frame =
    let ns = (Left (Namespace [] [])) in
    (initial "Interactive" ns) { deps = deps
                               , frame = Just frame }


addImplicitDeps uses srcfile@SrcFile { srcNs = Left (Namespace uses' stxs) } =
    srcfile { srcNs = Left $ Namespace (uses ++ uses') stxs }