module Data.SrcFile where

import Data.Map (Map)
import qualified Data.Map as Map (empty, fromList)

import qualified Data.Env as Env
import Data.FrameEnv
import Data.Stx
import Data.Symbol (Symbol (..))
import Data.Type
import Monad.InterpreterM


data SrcFileT = CoreT
              | SrcT
              | InteractiveT
              deriving (Show)

data SrcFile
    = SrcFile { t :: SrcFileT
              , name :: String
              , deps :: [String]
              , symbols :: Map String Symbol
              , ts :: Map String Type
              , exprs :: Map String Expr
              , srcNs :: Maybe (Namespace String)
              , renNs :: Maybe (Namespace String)
              , lnkNs :: Maybe (Namespace String) }
      deriving (Show)


initial :: SrcFileT -> String -> [String] -> SrcFile
initial t name deps =
    SrcFile { t = t
            , name = name
            , deps = deps
            , symbols = Map.empty
            , ts = Map.empty
            , exprs = Map.empty
            , srcNs = Nothing
            , renNs = Nothing
            , lnkNs = Nothing }


type TypeDesc = [(String, Type)]
type FnDesc = [(String, Type, Expr)]


mkCoreSrcFile :: String -> [String] -> TypeDesc -> FnDesc -> SrcFile
mkCoreSrcFile name deps typeDesc fnDesc =
    (initial CoreT name deps) { symbols = symbols
                              , ts = ts
                              , exprs = exprs }
    where symbols =
              Map.fromList (typeSymbols ++ fnSymbols)
              where typeSymbols = [ (x, TypeSymbol x) | (x, _) <- typeDesc ]
                    fnSymbols = [ (x, FnSymbol x) | (x, _, _) <- fnDesc ]

          ts =
              Map.fromList (typeDesc ++ fnTs)
              where fnTs = [ (x, y) | (x, y, _) <- fnDesc ]

          exprs =
              Map.fromList [ (x, y) | (x, _, y) <- fnDesc ]


mkInteractiveSrcFile :: [String] -> SrcFile
mkInteractiveSrcFile deps =
    let
        uses = [ (x, "") | x <- deps ]
        srcNs = Namespace uses []
    in
      (initial InteractiveT "Interactive" deps) { srcNs = Just srcNs }


mkParsedSrcFile :: String -> Namespace String -> SrcFile
mkParsedSrcFile name ns =
    (initial SrcT name []) { srcNs = Just ns }


addImplicitDeps :: [(String, String)] -> SrcFile -> SrcFile
addImplicitDeps uses srcfile@SrcFile { srcNs = Just (Namespace uses' stxs) } =
    srcfile { srcNs = Just $ Namespace (uses ++ uses') stxs }