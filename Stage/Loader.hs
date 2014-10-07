{-# LANGUAGE LambdaCase, ParallelListComp #-}
module Stage.Loader (preload) where

import Prelude hiding (lex, mod)

import Control.Monad.Except
import qualified Data.List as List (nub)
import Data.Map (Map)
import qualified Data.Map as Map ((!), fromList)
import qualified Data.Set as Set (empty, member, insert)
import System.IO.Error (tryIOError)

import Data.FileSystem (FileSystem)
import qualified Data.FileSystem as FileSystem (lookup, member)
import Data.GraphUtils (acyclicTopSort)
import Data.Module (ModuleT(..), Module(modDecls))
import qualified Data.Module as Module
import Data.PrettyString
import qualified Data.PrettyString as PrettyString
import Data.Result
import Data.Source
import Parser (parseFile)
import qualified Pretty.Stage.Loader as Pretty

type LoaderM a = ResultT IO a

readFileM :: String -> LoaderM String
readFileM modName =
  do res <- liftIO . tryIOError . readFile $ map tr modName ++ ".bsl"
     case res of
       Left err -> throwError . Pretty.readFileFail modName $ show err
       Right x -> return x
  where
    tr '.' = '/'
    tr c = c

parseModule :: String -> String -> LoaderM Source
parseModule modName filetext =
  case parseFile modName filetext of
    Left err -> throwError $ PrettyString.text $ err
    Right x -> return x

loadModule :: String -> LoaderM Module
loadModule filename =
  do str <- readFileM filename
     ModuleS me uses body <- parseModule filename str
     when (filename /= me) $
       throwError $ Pretty.moduleMeMismatch me filename
     when (length (map fst uses) /= length (List.nub (map fst uses))) $
       throwError $ Pretty.moduleContainsDuplicateUses me uses
     let unprefixed = filter (not . null . snd) uses
     when (length (map snd unprefixed) /= length (List.nub (map snd unprefixed))) $
       throwError $ Pretty.moduleContainsDuplicateQualifiers me uses
     return (Module.initial SrcT me uses) { modDecls = body }

preloadModule :: FileSystem -> String -> LoaderM [Module]
preloadModule fs =
  preloadModule' [] Set.empty . (:[])
  where
    preloadModule' mods _ [] = return mods
    preloadModule' mods loaded (filename:filenames)
      | filename `Set.member` loaded =
        preloadModule' mods loaded filenames
      | fs `FileSystem.member` filename =
        case FileSystem.lookup fs filename of
          Nothing -> throwError $ Pretty.moduleNotFound filename
          Just mod -> preloadModule' (mod:mods) (Set.insert filename loaded) (Module.dependencies mod ++ filenames)
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

preload :: FileSystem -> String -> IO (Either PrettyString [Module])
preload fs filename =
  runResultT (preloadModule fs filename) >>=
    \case
      Bad err -> return $ Left err
      Ok mods ->
         case buildGraph mods of
           Left mods' -> return . Left . Pretty.moduleCycle $ map Module.modName mods'
           Right mods' -> return $ Right mods'
