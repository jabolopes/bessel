{-# LANGUAGE ParallelListComp, TupleSections #-}
module Data.SrcFile where

import Data.List (partition)
import Data.Map (Map)
import qualified Data.Map as Map ((!), empty, fromList, insert, lookup, mapKeys, mapMaybe, union, toList)
import Data.Maybe (fromMaybe)

import Data.Definition (Definition (symbol, val))
import qualified Data.Definition as Definition (name, initial, val, symbol)
import Data.Expr (Expr)
import Data.Symbol (Symbol (..))
import Monad.InterpreterM (Val)


data SrcFileT = CoreT
              | SrcT
              | InteractiveT
              deriving (Show)


data SrcFile
    = SrcFile { t :: SrcFileT
              , name :: String
              , deps :: [String]
              , unprefixedUses :: [String]
              , prefixedUses :: [(String, String)]
              , decls :: [Expr]
              , defs :: Map String Definition
              , defOrd :: [String] }


initial :: SrcFileT -> String -> [String] -> SrcFile
initial t name deps =
    SrcFile { t = t
            , name = name
            , deps = deps
            , unprefixedUses = []
            , prefixedUses = []
            , decls = []
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


type FnDesc = [(String, Val)]


mkCoreSrcFile :: String -> [String] -> FnDesc -> SrcFile
mkCoreSrcFile srcfileName deps fnDesc =
    addDefinitions (initial CoreT srcfileName deps) defs
    where defName name = srcfileName ++ "." ++ name
          fnSymbol = FnSymbol . defName
          
          defs = [ (Definition.initial (defName x)) { symbol = Just (fnSymbol x), val = Just z } | (x, z) <- fnDesc ]


interactiveName :: String
interactiveName = "Interactive"


mkInteractiveSrcFile :: [SrcFile] -> [Expr] -> SrcFile
mkInteractiveSrcFile srcfiles exprs =
    let deps = map name srcfiles in
    (initial InteractiveT interactiveName deps) { unprefixedUses = deps
                                                , decls = exprs }


mkParsedSrcFile :: String -> [(String, String)] -> [Expr] -> SrcFile
mkParsedSrcFile name uses exprs =
  let (unprefixed, prefixed) = partition (null . snd) uses in
  (initial SrcT name []) { unprefixedUses = map fst unprefixed
                         , prefixedUses = prefixed
                         , decls = exprs }


addImplicitUnprefixedUses :: [String] -> SrcFile -> SrcFile
addImplicitUnprefixedUses uses srcfile =
  srcfile { unprefixedUses = uses ++ unprefixedUses srcfile }


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


addDefinitionVals :: SrcFile -> Map String Val -> SrcFile
addDefinitionVals srcfile vals =
    srcfile { defs = loop (defs srcfile) (Map.toList vals) }
    where loop defs [] = defs
          loop defs ((name, val):ts) =
              let
                  def = case Map.lookup name defs of
                          Nothing -> error $ "SrcFile.addDefinitionVals: definition " ++ show name ++ " is not defined"
                          Just def -> def { val = Just val }
                  defs' = Map.insert name def defs
              in
                loop defs' ts


setDefinitionOrder :: SrcFile -> [String] -> SrcFile
setDefinitionOrder srcfile defOrd =
    srcfile { defOrd = defOrd }