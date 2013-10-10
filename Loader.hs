{-# LANGUAGE ParallelListComp #-}
module Loader where

import Prelude hiding (lex, mod)

import Data.Functor ((<$>))
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map ((!), fromList)
import qualified Data.Set as Set (empty, member, insert)

import Config
import Data.Exception (throwLoaderException, throwParserException)
import Data.FileSystem (FileSystem)
import qualified Data.FileSystem as FileSystem (get, member)
import Data.GraphUtils (acyclicTopSort)
import Data.Macro
import Data.Module (Module(..))
import qualified Data.Module as Module (modName, modDeps)
import Data.PrettyString (PrettyString)
import qualified Data.PrettyString as PrettyString
import Parser (parseFile)
import qualified Pretty.Loader as Pretty
import qualified Stage.Expander as Stage

readFileM :: String -> IO String
readFileM filename = readFile $ toFilename filename ++ ".bsl"
    where toFilename = map f
              where f '.' = '/'
                    f c = c

parseModule :: String -> String -> Either PrettyString Macro
parseModule filename filetext =
  case parseFile filename filetext of
    Left str -> Left (PrettyString.text str)
    Right mod -> Right mod

loadFile :: String -> String -> Either PrettyString Macro
loadFile filename filetext
  | isPrelude filename =
    parseModule filename filetext
  | otherwise =
    do ModuleM me uses body <- parseModule filename filetext
       return $ ModuleM me ((coreName, ""):(preludeName, ""):uses) body

loadModuleM :: String -> IO Module
loadModuleM filename =
  do str <- readFileM filename
     let mod@(ModuleM me _ _) = case loadFile filename str of
                                  Left err -> throwParserException err
                                  Right x -> x
     if filename /= me
     then throwLoaderException (Pretty.moduleMeMismatch me filename)
     else do
       case Stage.expandModule mod of
         Left err -> throwLoaderException err
         Right x -> return x

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
