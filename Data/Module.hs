module Data.Module where

import Prelude hiding (mod)

import qualified Data.List as List (elem, nub, partition)
import Data.Map (Map)
import qualified Data.Map as Map (empty, fromList, insert, lookup, union, toList)
import Data.Maybe (fromMaybe)

import qualified Config
import Data.Definition (Definition(..))
import qualified Data.Definition as Definition
import Data.Source
import Data.Symbol (Symbol (..))
import Monad.InterpreterM (Val)

data ModuleT
  = CoreT
  | SrcT
  | InteractiveT
    deriving (Eq, Show)

data Module
  = Module { modType :: ModuleT
           , modName :: String
           , modDeps :: [String]
           , modUnprefixedUses :: [String]
           , modPrefixedUses :: [(String, String)]
           , modDecls :: [Source]
           , modDefs :: Map String Definition
           , modDefOrd :: [String] }

ensureImplicitUses :: Module -> Module
ensureImplicitUses mod
  | modType mod /= SrcT || modName mod == Config.preludeName = mod
  | otherwise =
    mod { modDeps = ensureElem Config.coreName .
                      ensureElem Config.preludeName .
                        modDeps $ mod
        , modUnprefixedUses = ensureElem Config.coreName .
                                ensureElem Config.preludeName .
                                  modUnprefixedUses $ mod }
  where ensureElem x xs
          | List.elem x xs = xs
          | otherwise = x:xs

initial :: ModuleT -> String -> [(String, String)] -> Module
initial t name uses =
  let (unprefixed, prefixed) = List.partition (null . snd) uses in
  ensureImplicitUses
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

mkInteractiveModule :: [Module] -> [Source] -> Module
mkInteractiveModule mods srcs =
  let
    modNames = map modName mods
    uses = zip (init modNames) modNames ++ [(last modNames, "")]
  in
    ensureImplicitUses (initial InteractiveT interactiveName uses) { modDecls = srcs }

mkParsedModule :: String -> [(String, String)] -> [Source] -> Module
mkParsedModule name uses srcs =
  let (unprefixed, prefixed) = List.partition (null . snd) uses in
  (initial SrcT name []) { modUnprefixedUses = map fst unprefixed
                         , modPrefixedUses = prefixed
                         , modDecls = srcs }

addImplicitUnprefixedUses :: [String] -> Module -> Module
addImplicitUnprefixedUses uses mod =
  mod { modUnprefixedUses = uses ++ modUnprefixedUses mod }

addDefinitions :: Module -> [Definition] -> Module
addDefinitions mod defs =
  mod { modDefs = defsMp `Map.union` modDefs mod
      , modDefOrd = List.nub $ modDefOrd mod ++ map Definition.defName defs }
  where defsMp =
          Map.fromList [ (Definition.defName def, def) | def <- defs ]

updateDefinitions :: Module -> [Definition] -> Module
updateDefinitions mod definitions =
  mod { modDefs = defsMp `Map.union` modDefs mod
      , modDefOrd = List.nub $ modDefOrd mod ++ map Definition.defName definitions }
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
    mod { modDefOrd = List.nub defOrd }