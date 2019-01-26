{-# LANGUAGE TupleSections #-}
module Data.Module where

import Prelude hiding (mod)

import Control.Applicative ((<$>))
import Data.IORef
import qualified Data.List as List (elem, nub, partition)
import Data.Map (Map)
import qualified Data.Map as Map (empty, fromList, insert, lookup, union, toList)
import Data.Maybe (fromMaybe)
import Data.Name (Name)

import qualified Config
import Data.Definition (Definition(..))
import qualified Data.Definition as Definition
import qualified Data.Name as Name
import Data.Source
import Monad.InterpreterM (Val)
import qualified Utils

data ModuleT
  = CoreT
  | SrcT
  | InteractiveT
    deriving (Eq, Show)

data Module
  = Module { modType :: ModuleT
           , modName :: Name
           , modDecls :: [Source]
           , modDefs :: Map String Definition
           , modDefOrd :: [String]
           , modUses :: [(Name, Name)]
           }

ensureImplicitUses :: Module -> Module
ensureImplicitUses mod
  | modType mod == CoreT || modName mod == Config.preludeName = mod
  | otherwise =
    mod { modUses = ensureElem (Config.coreName, Name.empty) .
                      ensureElem (Config.preludeName, Name.empty) .
                        modUses $ mod }
  where
    ensureElem x xs
      | List.elem x xs = xs
      | otherwise = x:xs

-- | Expands unqualified uses into all their component combinations.
--
-- For example,
--
--   use Data.List
--
-- expands to
--
--   use Data.List
--   use List
expandUnqualifiedUses :: Module -> Module
expandUnqualifiedUses mod =
  mod { modUses = List.nub . concatMap expandUnprefixed $ modUses mod }
  where
    dropFirstComponent :: Name -> Name
    dropFirstComponent =
      Name.untyped . Utils.flattenId . tail . Utils.splitId . Name.nameStr

    expandUse :: Name -> [Name]
    expandUse use
      | Name.isEmptyName use = []
      | otherwise = (use:) . expandUse $ dropFirstComponent use

    expandUnprefixed :: (Name, Name) -> [(Name, Name)]
    expandUnprefixed x@(use, asName)
      | Name.isEmptyName asName =
        ((use, Name.empty):) . map (use,) $ expandUse use
      | otherwise =
        [x]

initial :: ModuleT -> Name -> [(Name, Name)] -> Module
initial t name uses =
  expandUnqualifiedUses $
  ensureImplicitUses
    Module { modType = t
           , modName = name
           , modDecls = []
           , modDefs = Map.empty
           , modDefOrd = []
           , modUses = uses }

defsAsc :: Module -> [Definition]
defsAsc mod = map def (modDefOrd mod)
    where
      def name =
        fromMaybe
          (error $ "Data.Module.defsAsc: definition " ++ show name ++ " is not defined")
          (Map.lookup name (modDefs mod))

dependencies :: Module -> [Name]
dependencies = List.nub . map fst . modUses

prefixedUses :: Module -> [(Name, Name)]
prefixedUses = snd . List.partition (Name.isEmptyName . snd) . modUses

unprefixedUses :: Module -> [Name]
unprefixedUses =
  map fst . fst . List.partition (Name.isEmptyName . snd) . modUses

type FnDesc = [(String, Val)]

mkCoreModule :: Name -> [Name] -> FnDesc -> IO Module
mkCoreModule moduleName deps fnDesc =
  let uses = [ (dep, Name.empty) | dep <- deps ] in
  ensureDefinitions (initial CoreT moduleName uses) <$> defs
  where
    defs =
      sequence
      [ do ref <- newIORef val
           return (Definition.initial (moduleName `Name.joinNames` (Name.untyped name))) { defVal = Right ref }
        | (name, val) <- fnDesc ]

interactiveName :: Name
interactiveName = Name.untyped "Interactive"

mkInteractiveModule :: [Module] -> [Source] -> Module
mkInteractiveModule mods srcs =
  let
    modNames = map modName mods
    uses = zip (init modNames) modNames ++ [(last modNames, Name.empty)]
  in
    ensureImplicitUses (initial InteractiveT interactiveName uses) { modDecls = srcs }

ensureDefinitions :: Module -> [Definition] -> Module
ensureDefinitions mod defs =
  mod { modDefs = defsMp `Map.union` modDefs mod
      , modDefOrd = List.nub $ modDefOrd mod ++ map (Name.nameStr . Definition.defName) defs }
  where
    defsMp =
      Map.fromList [ (Name.nameStr $ Definition.defName def, def) | def <- defs ]

addDefinitionVals :: Module -> Map String (IORef Val) -> Module
addDefinitionVals mod vals =
  mod { modDefs = loop (modDefs mod) (Map.toList vals) }
  where
    loop defs [] = defs
    loop defs ((name, val):xs) =
      let
        def = case Map.lookup name defs of
                Nothing -> error $ "Module.addDefinitionVals: definition " ++ show name ++ " is not defined"
                Just x -> x { defVal = Right val }
        defs' = Map.insert name def defs
      in
       loop defs' xs

setDefinitionOrder :: Module -> [String] -> Module
setDefinitionOrder mod defOrd =
  mod { modDefOrd = List.nub defOrd }
