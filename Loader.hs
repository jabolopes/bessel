{-# LANGUAGE ParallelListComp #-}
module Loader where

import Prelude hiding (lex, mod)

import Control.Monad (when)
import qualified Data.List as List (nub)
import Data.Map (Map)
import qualified Data.Map as Map ((!), fromList)
import qualified Data.Set as Set (empty, member, insert)

import Data.Exception (throwLoaderException)
import Data.FileSystem (FileSystem)
import qualified Data.FileSystem as FileSystem (get, member)
import Data.GraphUtils (acyclicTopSort)
import Data.Module (ModuleT(..), Module(modDecls))
import qualified Data.Module as Module
import Data.PrettyString (PrettyString)
import qualified Data.PrettyString as PrettyString
import Data.Source
import Parser (parseFile)
import qualified Pretty.Stage.Loader as Pretty

readFileM :: String -> IO String
readFileM modName = readFile $ map tr modName ++ ".bsl"
  where tr '.' = '/'
        tr c = c

parseModule :: String -> String -> Either PrettyString Source
parseModule modName filetext =
  case parseFile modName filetext of
    Left err -> Left . PrettyString.text $ err
    Right x -> Right x

loadModule :: String -> IO Module
loadModule filename =
  do str <- readFileM filename
     let ModuleS me uses body =
           case parseModule filename str of
             Left err -> throwLoaderException err
             Right x -> x
     when (filename /= me) $
       throwLoaderException $ Pretty.moduleMeMismatch me filename
     when (length (map fst uses) /= length (List.nub (map fst uses))) $
       throwLoaderException $ Pretty.moduleContainsDuplicateUses me uses
     let unprefixed = filter (not . null . snd) uses
     when (length (map snd unprefixed) /= length (List.nub (map snd unprefixed))) $
       throwLoaderException $ Pretty.moduleContainsDuplicateQualifiers me uses
     return (Module.initial SrcT me uses) { modDecls = body }

preloadModule :: FileSystem -> String -> IO [Module]
preloadModule fs = preloadModule' [] Set.empty . (:[])
  where
    preloadModule' mods _ [] = return mods
    preloadModule' mods loaded (filename:filenames)
      | filename `Set.member` loaded =
        preloadModule' mods loaded filenames
      | fs `FileSystem.member` filename =
        do let mod = FileSystem.get fs filename
           preloadModule' (mod:mods) (Set.insert filename loaded) (Module.dependencies mod ++ filenames)
      | otherwise =
        do mod <- loadModule filename
           preloadModule' (mod:mods) (Set.insert filename loaded) (Module.dependencies mod ++ filenames)

buildNodes :: [Module] -> Map String Int
buildNodes mods =
  Map.fromList [ (Module.modName mod, i) | mod <- mods | i <- [1..] ]

buildEdges :: Map String Int -> [Module] -> [(Int, Int)]
buildEdges nodes =
  concatMap (\mod -> zip (repeat (nodes Map.! Module.modName mod)) (map (nodes Map.!) (Module.dependencies mod)))

buildGraph :: [Module] -> Either [Module] [Module]
buildGraph mods =
  let edges = buildEdges (buildNodes mods) mods in
  acyclicTopSort mods edges

preload :: FileSystem -> String -> IO [Module]
preload fs filename =
  do mods <- preloadModule fs filename
     case buildGraph mods of
       Left mods' -> error $ "Loader.preload: module cycle in " ++ show (map Module.modName mods')
       Right mods' -> return mods'
