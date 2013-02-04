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
              , symbols :: Map String Symbol
              , ts :: Map String Type
              , exprs :: Map String Expr
              , srcNs :: (Either (Namespace String) ([String], Map String (Type, Expr)))
              , renNs :: Maybe (Namespace String) }
      deriving (Show)


initial name srcNs =
    SrcFile { name = name
            , deps = []
            , symbols = Map.empty
            , ts = Map.empty
            , exprs = Map.empty
            , srcNs = srcNs
            , renNs = Nothing }


mkParsedSrcFile :: String -> Namespace String -> SrcFile
mkParsedSrcFile name ns =
    (initial name (Left ns))


mkInteractiveSrcFile :: [String] -> SrcFile
mkInteractiveSrcFile deps =
    let
        uses = [ (x, "") | x <- deps ]
        ns = (Left (Namespace uses []))
    in
      (initial "Interactive" ns) { deps = deps }


addImplicitDeps :: [(String, String)] -> SrcFile -> SrcFile
addImplicitDeps uses srcfile@SrcFile { srcNs = Left (Namespace uses' stxs) } =
    srcfile { srcNs = Left $ Namespace (uses ++ uses') stxs }