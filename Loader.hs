{-# LANGUAGE ParallelListComp #-}
module Loader where

import Prelude hiding (lex, mod)

import Data.Map (Map)
import qualified Data.Map as Map ((!), fromList)
import qualified Data.Set as Set (empty, member, insert)

import Config (isPrelude)
import Data.Exception (throwLoaderException, throwParserException)
import Data.FileSystem (FileSystem)
import qualified Data.FileSystem as FileSystem (get, member)
import Data.GraphUtils (acyclicTopSort)
import Data.Module (Module(..))
import qualified Data.Module as Module (modName, modDeps)
import Parser (parsePrelude, parseFile)

readFileM :: String -> IO String
readFileM filename = readFile $ toFilename filename ++ ".bsl"
    where toFilename = map f
              where f '.' = '/'
                    f c = c

loadModuleM :: String -> IO Module
loadModuleM filename =
    do str <- readFileM filename
       let parseFn | isPrelude filename = parsePrelude
                   | otherwise = parseFile
           mod = case parseFn filename str of
                       Left err -> throwParserException err
                       Right x -> x
       if Module.modName mod /= filename
       then throwLoaderException $ "me " ++ show (Module.modName mod) ++ " and filename " ++ show filename ++ " mismatch"
       else return mod

preloadModule :: FileSystem -> String -> IO [Module]
preloadModule fs filename = preloadModule' [] Set.empty [filename]
    where preloadModule' mods _ [] = return mods
          preloadModule' mods loaded (filename:filenames)
              | filename `Set.member` loaded =
                  preloadModule' mods loaded filenames
              | fs `FileSystem.member` filename =
                  do let mod = FileSystem.get fs filename
                     preloadModule' (mod:mods) (Set.insert filename loaded) (Module.modDeps mod ++ filenames)
              | otherwise =
                  do mod <- loadModuleM filename
                     preloadModule' (mod:mods) (Set.insert filename loaded) (Module.modDeps mod ++ filenames)

buildNodes :: [Module] -> Map String Int
buildNodes mods =
    Map.fromList [ (Module.modName mod, i) | mod <- mods | i <- [1..] ]

buildEdges :: Map String Int -> [Module] -> [(Int, Int)]
buildEdges nodes =
    concatMap (\mod -> zip (repeat (nodes Map.! Module.modName mod)) (map (nodes Map.!) (Module.modDeps mod)))

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
