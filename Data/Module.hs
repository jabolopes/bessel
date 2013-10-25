module Data.Module where

import Prelude hiding (mod)

import qualified Data.List as List (partition)
import Data.Map (Map)
import qualified Data.Map as Map (empty, fromList, insert, lookup, mapMaybe, union, toList)
import Data.Maybe (fromMaybe)

import Data.Definition (Definition(..))
import qualified Data.Definition as Definition
import Data.Macro
import Data.Symbol (Symbol (..))
import Monad.InterpreterM (Val)

data ModuleT
  = CoreT
  | SrcT
  | InteractiveT
    deriving (Show)

data Module
  = Module { modType :: ModuleT
           , modName :: String
           , modDeps :: [String]
           , modUnprefixedUses :: [String]
           , modPrefixedUses :: [(String, String)]
           , modDecls :: [Macro]
           , modDefs :: Map String Definition
           , modDefOrd :: [String] }

initial :: ModuleT -> String -> [(String, String)] -> Module
initial t name uses =
  let (unprefixed, prefixed) = List.partition (null . snd) uses in
  Module { modType = t
         , modName = name
         , modDeps = map fst uses
         , modUnprefixedUses = map fst unprefixed
         , modPrefixedUses = prefixed
         , modDecls = []
         , modDefs = Map.empty
         , modDefOrd = [] }

defsAsc :: Module -> [Definition]
defsAsc mod = map def (modDefOrd mod)
    where def name =
            fromMaybe
              (error $ "Data.Module.defsAsc: definition " ++ show name ++ " is not defined")
              (Map.lookup name (modDefs mod))

symbols :: Module -> Map String Symbol
symbols = Map.mapMaybe Definition.defSym . modDefs

type FnDesc = [(String, Val)]

mkCoreModule :: String -> [String] -> FnDesc -> Module
mkCoreModule name deps fnDesc =
    let uses = [ (dep, "") | dep <- deps ] in
    addDefinitions (initial CoreT name uses) defs
    where qualName descName = name ++ "." ++ descName

          defs = [ (Definition.initial (qualName x)) { defSym = Just (FnSymbol (qualName x)),
                                                       defVal = Right z } | (x, z) <- fnDesc ]

interactiveName :: String
interactiveName = "Interactive"

mkInteractiveModule :: [Module] -> [Macro] -> Module
mkInteractiveModule mods macros =
    let uses = zip (map modName mods) (repeat "") in
    (initial InteractiveT interactiveName uses) { modDecls = macros }

mkParsedModule :: String -> [(String, String)] -> [Macro] -> Module
mkParsedModule name uses macros =
  let (unprefixed, prefixed) = List.partition (null . snd) uses in
  (initial SrcT name []) { modUnprefixedUses = map fst unprefixed
                         , modPrefixedUses = prefixed
                         , modDecls = macros }

addImplicitUnprefixedUses :: [String] -> Module -> Module
addImplicitUnprefixedUses uses mod =
  mod { modUnprefixedUses = uses ++ modUnprefixedUses mod }

addDefinitions :: Module -> [Definition] -> Module
addDefinitions mod defs =
    mod { modDefs = defsMp `Map.union` modDefs mod
        , modDefOrd = modDefOrd mod ++ map Definition.defName defs }
    where defsMp =
              Map.fromList [ (Definition.defName def, def) | def <- defs ]

updateDefinitions :: Module -> [Definition] -> Module
updateDefinitions mod definitions =
    mod { modDefs = defsMp `Map.union` modDefs mod }
    where defsMp =
              Map.fromList [ (Definition.defName def, def) | def <- definitions ]

addDefinitionSymbols :: Module -> Map String Symbol -> Module
addDefinitionSymbols mod syms =
    mod { modDefs = loop (modDefs mod) (Map.toList syms) }
    where loop defs [] = defs
          loop defs ((name, sym):syms) =
            let
                def = case Map.lookup name defs of
                        Nothing -> error $ "Module.addDefinitionSymbols: definition " ++ show name ++ " is not defined"
                        Just def -> def { defSym = Just sym }
                defs' = Map.insert name def defs
            in
              loop defs' syms

addDefinitionVals :: Module -> Map String Val -> Module
addDefinitionVals mod vals =
    mod { modDefs = loop (modDefs mod) (Map.toList vals) }
    where loop defs [] = defs
          loop defs ((name, val):ts) =
              let
                  def = case Map.lookup name defs of
                          Nothing -> error $ "Module.addDefinitionVals: definition " ++ show name ++ " is not defined"
                          Just x -> x { defVal = Right val }
                  defs' = Map.insert name def defs
              in
                loop defs' ts

setDefinitionOrder :: Module -> [String] -> Module
setDefinitionOrder mod defOrd =
    mod { modDefOrd = defOrd }
