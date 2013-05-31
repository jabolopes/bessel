{-# LANGUAGE BangPatterns, ParallelListComp, TupleSections #-}
module Data.SrcFile where

import Data.Map (Map)
import qualified Data.Map as Map ((!), empty, fromList, insert, lookup, mapMaybe, toList)

import Data.Definition (Definition (symbol, typ, expr))
import qualified Data.Definition as Definition (initial, expr, typ, symbol)
import Data.Stx (Namespace (..), Stx)
import Data.Symbol (Symbol (..))
import Data.Type (Type)
import Monad.InterpreterM (Expr)


data SrcFileT = CoreT
              | SrcT
              | InteractiveT
              deriving (Show)

data SrcFile
    = SrcFile { t :: SrcFileT
              , name :: String
              , deps :: [String]
              , srcNs :: Maybe (Namespace String)
              , renNs :: Maybe (Namespace String)
              , lnkNs :: Maybe (Namespace String)
              , defs :: Map String Definition }


initial :: SrcFileT -> String -> [String] -> SrcFile
initial t name deps =
    SrcFile { t = t
            , name = name
            , deps = deps
            , srcNs = Nothing
            , renNs = Nothing
            , lnkNs = Nothing
            , defs = Map.empty }


symbols :: SrcFile -> Map String Symbol
symbols = Map.mapMaybe Definition.symbol . defs


types :: SrcFile -> Map String Type
types = Map.mapMaybe Definition.typ . defs


exprs :: SrcFile -> Map String Expr
exprs = Map.mapMaybe Definition.expr . defs


type TypeDesc = [(String, Type)]
type FnDesc = [(String, Type, Expr)]


mkCoreSrcFile :: String -> [String] -> TypeDesc -> FnDesc -> SrcFile
mkCoreSrcFile name deps typeDesc fnDesc =
    (initial CoreT name deps) { defs = defs }
    where defs =
            let
                typs = [ (x, (Definition.initial x) { symbol = Just (TypeSymbol i), typ = Just y }) | (x, y) <- typeDesc | i <- [0..] ]
                fns = [ (x, def) | (x, y, z) <- fnDesc, let def = (Definition.initial x) { symbol = Just (FnSymbol x), typ = Just y, expr = Just z } ]
            in
              Map.fromList (typs ++ fns)


interactiveName :: String
interactiveName = "Interactive"


mkInteractiveSrcFile :: [SrcFile] -> [Stx String] -> SrcFile
mkInteractiveSrcFile srcfiles stxs =
    let
        deps = map name srcfiles
        uses = map (,"") deps
    in
      (initial InteractiveT interactiveName deps) { srcNs = Just (Namespace uses stxs) }


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
                  def = case Map.lookup name defs of
                          Nothing -> error $ "addDefinitionExprs: definition " ++ show name ++ " is not defined"
                          Just x -> x
                  def' = def { expr = Just expr }
                  defs' = Map.insert name def' defs
              in
                loop defs' ts
