{-# LANGUAGE ParallelListComp, TupleSections #-}
module Data.SrcFile where

import Data.Map (Map)
import qualified Data.Map as Map ((!), empty, fromList, insert, lookup, mapKeys, mapMaybe, union, toList)
import Data.Maybe (fromMaybe)

import Data.Definition (Definition (symbol, typ, val))
import qualified Data.Definition as Definition (name, initial, val, typ, symbol)
import Data.Stx (Namespace (..), Stx)
import Data.Symbol (Symbol (..))
import Data.Type (Type)
import Monad.InterpreterM (Val)


data SrcFileT = CoreT
              | SrcT
              | InteractiveT
              deriving (Show)


data SrcFile
    = SrcFile { t :: SrcFileT
              , name :: String
              , deps :: [String]
              , srcNs :: Maybe (Namespace String)
              , defs :: Map String Definition
              , defOrd :: [String] }


initial :: SrcFileT -> String -> [String] -> SrcFile
initial t name deps =
    SrcFile { t = t
            , name = name
            , deps = deps
            , srcNs = Nothing
            , defs = Map.empty
            , defOrd = [] }


defsAsc :: SrcFile -> [Definition]
defsAsc srcfile = map def (defOrd srcfile)
    where def name =
            fromMaybe
              (error $ "defsAsc: definition " ++ show name ++ " is not defined")
              (Map.lookup name (defs srcfile))


symbols :: SrcFile -> Map String Symbol
symbols = Map.mapMaybe Definition.symbol . defs


type TypeDesc = [(String, Type)]
type FnDesc = [(String, Type, Val)]


mkCoreSrcFile :: String -> [String] -> TypeDesc -> FnDesc -> SrcFile
mkCoreSrcFile srcfileName deps typeDesc fnDesc =
    addDefinitions (initial CoreT srcfileName deps) defs
    where defName name = srcfileName ++ "." ++ name
          fnSymbol = FnSymbol . defName
          
          defs =
            let
                typs = [ (Definition.initial (defName x)) { symbol = Just (TypeSymbol i), typ = Just y } | (x, y) <- typeDesc | i <- [0..] ]
                fns = [ (Definition.initial (defName x)) { symbol = Just (fnSymbol x), typ = Just y, val = Just z } | (x, y, z) <- fnDesc ]
            in
              typs ++ fns


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


addDefinitions :: SrcFile -> [Definition] -> SrcFile
addDefinitions srcfile definitions =
    srcfile { defs = defsMp `Map.union` defs srcfile
            , defOrd = defOrd srcfile ++ map Definition.name definitions }
    where defsMp =
              Map.fromList [ (Definition.name def, def) | def <- definitions ]


updateDefinitions :: SrcFile -> [Definition] -> SrcFile
updateDefinitions srcfile definitions =
    srcfile { defs = defsMp `Map.union` defs srcfile }
    where defsMp =
              Map.fromList [ (Definition.name def, def) | def <- definitions ]


addDefinitionSymbols :: SrcFile -> Map String Symbol -> SrcFile
addDefinitionSymbols srcfile syms =
    srcfile { defs = loop (defs srcfile) (Map.toList syms) }
    where loop defs [] = defs
          loop defs ((name, sym):syms) =
            let
                def = case Map.lookup name defs of
                        Nothing -> error $ "SrcFile.addDefinitionSymbols: definition " ++ show name ++ " is not defined"
                        Just def -> def { symbol = Just sym }
                defs' = Map.insert name def defs
            in
              loop defs' syms


addDefinitionTypes :: SrcFile -> Map String Type -> SrcFile
addDefinitionTypes srcfile ts =
    srcfile { defs = loop (defs srcfile) (Map.toList ts) }
    where loop defs [] = defs
          loop defs ((name, t):ts) =
              let
                  def = case Map.lookup name defs of
                          Nothing -> error $ "SrcFile.addDefinitionTypes: definition " ++ show name ++ " is not defined"
                          Just def -> def { typ = Just t }
                  defs' = Map.insert name def defs
              in
                loop defs' ts


addDefinitionExprs :: SrcFile -> Map String Val -> SrcFile
addDefinitionExprs srcfile exprs =
    srcfile { defs = loop (defs srcfile) (Map.toList exprs) }
    where loop defs [] = defs
          loop defs ((name, expr):ts) =
              let
                  def = case Map.lookup name defs of
                          Nothing -> error $ "SrcFile.addDefinitionExprs: definition " ++ show name ++ " is not defined"
                          Just def -> def { val = Just val }
                  defs' = Map.insert name def defs
              in
                loop defs' ts


setDefinitionOrder :: SrcFile -> [String] -> SrcFile
setDefinitionOrder srcfile defOrd =
    srcfile { defOrd = defOrd }