module Data.Module where

import Prelude hiding (mod)

import Control.Applicative ((<$>))
import Data.IORef
import qualified Data.List as List (elem, nub, partition)
import Data.Map (Map)
import qualified Data.Map as Map (empty, fromList, insert, lookup, union, toList)
import Data.Maybe (fromMaybe)

import qualified Config
import Data.Definition (Definition(..))
import qualified Data.Definition as Definition
import qualified Data.QualName as QualName
import Data.Source
import Monad.InterpreterM (Val)

data ModuleT
  = CoreT
  | SrcT
  | InteractiveT
    deriving (Eq, Show)

data Module
  = Module { modType :: ModuleT
           , modName :: String
           , modDecls :: [Source]
           , modDefs :: Map String Definition
           , modDefOrd :: [String]
           , modUses :: [(String, String)]
           }

ensureImplicitUses :: Module -> Module
ensureImplicitUses mod
  | modType mod == CoreT || modName mod == Config.preludeName = mod
  | otherwise =
    mod { modUses = ensureElem (Config.coreName, "") .
                      ensureElem (Config.preludeName, "") .
                        modUses $ mod }
  where
    ensureElem x xs
      | List.elem x xs = xs
      | otherwise = x:xs

initial :: ModuleT -> String -> [(String, String)] -> Module
initial t name uses =
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

dependencies :: Module -> [String]
dependencies = List.nub . map fst . modUses

prefixedUses :: Module -> [(String, String)]
prefixedUses = snd . List.partition (null . snd) . modUses

unprefixedUses :: Module -> [String]
unprefixedUses = map fst . fst . List.partition (null . snd) . modUses

type FnDesc = [(String, Val)]

mkCoreModule :: String -> [String] -> FnDesc -> IO Module
mkCoreModule name deps fnDesc =
  let uses = [ (dep, "") | dep <- deps ] in
  ensureDefinitions (initial CoreT name uses) <$> defs
  where
    defs =
      sequence
      [ do ref <- newIORef val
           return (Definition.initial (QualName.mkQualName [name, defName])) { defVal = Right ref } | (defName, val) <- fnDesc ]

interactiveName :: String
interactiveName = "Interactive"

mkInteractiveModule :: [Module] -> [Source] -> Module
mkInteractiveModule mods srcs =
  let
    modNames = map modName mods
    uses = zip (init modNames) modNames ++ [(last modNames, "")]
  in
    ensureImplicitUses (initial InteractiveT interactiveName uses) { modDecls = srcs }

ensureDefinitions :: Module -> [Definition] -> Module
ensureDefinitions mod defs =
  mod { modDefs = defsMp `Map.union` modDefs mod
      , modDefOrd = List.nub $ modDefOrd mod ++ map (QualName.fromQualName . Definition.defName) defs }
  where
    defsMp =
      Map.fromList [ (QualName.fromQualName $ Definition.defName def, def) | def <- defs ]

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
