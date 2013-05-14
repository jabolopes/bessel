module Data.SrcFile where

import Data.Map (Map)
import qualified Data.Map as Map ((!), empty, fromList, insert, mapMaybe, toList)
import Data.Maybe (catMaybes)

import Data.Definition (Definition (symbol, typ, expr))
import qualified Data.Definition as Definition
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
              , exprs :: Map String Expr
              , srcNs :: Maybe (Namespace String)
              , renNs :: Maybe (Namespace String)
              , lnkNs :: Maybe (Namespace String)
              , defs :: Map String Definition }


initial :: SrcFileT -> String -> [String] -> SrcFile
initial t name deps =
    SrcFile { t = t
            , name = name
            , deps = deps
            , symbols = Map.empty
            , exprs = Map.empty
            , srcNs = Nothing
            , renNs = Nothing
            , lnkNs = Nothing
            , defs = Map.empty }


types :: SrcFile -> Map String Type
types srcfile = Map.mapMaybe Definition.typ (defs srcfile)


type TypeDesc = [(String, Type)]
type FnDesc = [(String, Type, Expr)]


mkCoreSrcFile :: String -> [String] -> TypeDesc -> FnDesc -> SrcFile
mkCoreSrcFile name deps typeDesc fnDesc =
    (initial CoreT name deps) { symbols = symbols
                              , exprs = exprs
                              , defs = defs }
    where symbols =
              Map.fromList (typeSymbols ++ fnSymbols)
              where typeSymbols = [ (x, TypeSymbol x) | (x, _) <- typeDesc ]
                    fnSymbols = [ (x, FnSymbol x) | (x, _, _) <- fnDesc ]

          exprs =
              Map.fromList [ (x, y) | (x, _, y) <- fnDesc ]
              
          defs =
            let
                typs = [ (x, def) | (x, y) <- typeDesc, let def = (Definition.initial x) { symbol = Just (TypeSymbol x), typ = Just y } ]
                fns = [ (x, def) | (x, y, z) <- fnDesc, let def = (Definition.initial x) { symbol = Just (FnSymbol x), typ = Just y, expr = Just z } ]
            in
              Map.fromList (typs ++ fns)


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


addDefinitionSymbols :: SrcFile -> Map String Symbol -> SrcFile
addDefinitionSymbols srcfile syms =
    srcfile { defs = loop (defs srcfile) (Map.toList syms) }
    where loop defs [] = defs
          loop defs ((name, sym):syms) =
            let
                def = (Definition.initial name) { symbol = Just sym }
                defs' = Map.insert name def defs
            in
              loop defs' syms


addDefinitionTypes :: SrcFile -> Map String Type -> SrcFile
addDefinitionTypes srcfile ts =
    srcfile { defs = loop (defs srcfile) (Map.toList ts) }
    where loop defs [] = defs
          loop defs ((name, t):ts) =
              let
                  def = (defs Map.! name) { typ = Just t }
                  defs' = Map.insert name def defs
              in
                loop defs' ts


addDefinitionExprs :: SrcFile -> Map String Expr -> SrcFile
addDefinitionExprs srcfile exprs =
    srcfile { defs = loop (defs srcfile) (Map.toList exprs) }
    where loop defs [] = defs
          loop defs ((name, expr):ts) =
              let
                  def = (defs Map.! name) { expr = Just expr }
                  defs' = Map.insert name def defs
              in
                loop defs' ts