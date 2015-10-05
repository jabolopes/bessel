{-# LANGUAGE LambdaCase, ParallelListComp #-}
module Stage.Loader (preload) where

import Prelude hiding (lex, mod)

import Control.Monad.Except
import qualified Data.List as List (nub)
import Data.Map (Map)
import qualified Data.Map as Map ((!), fromList)
import Data.Set (Set)
import qualified Data.Set as Set (empty, member, insert)
import System.IO.Error (tryIOError)

import Data.FileSystem (FileSystem)
import qualified Data.FileSystem as FileSystem (lookup, member)
import Data.GraphUtils (acyclicTopSort)
import Data.Module (ModuleT(..), Module(modDecls))
import qualified Data.Module as Module
import Data.Name (Name)
import qualified Data.Name as Name
import Data.PrettyString
import qualified Data.PrettyString as PrettyString
import Data.Result
import Data.Source
import Parser (parseFile)
import qualified Pretty.Stage.Loader as Pretty

type LoaderM a = ResultT IO a

readFileM :: Name -> LoaderM String
readFileM modName =
  do res <- liftIO . tryIOError . readFile $ map tr (Name.nameStr modName) ++ ".bsl"
     case res of
       Left err -> throwError . Pretty.readFileFail modName $ show err
       Right x -> return x
  where
    tr '.' = '/'
    tr c = c

parseModule :: Name -> String -> LoaderM Source
parseModule modName filetext =
  case parseFile modName filetext of
    Left err -> throwError . PrettyString.text $ err
    Right x -> return x

loadModule :: Name -> LoaderM Module
loadModule modName =
  do str <- readFileM modName
     ModuleS me uses body <- parseModule modName str
     when (modName /= me) $
       throwError $ Pretty.moduleMeMismatch me modName
     when (length (map fst uses) /= length (List.nub (map fst uses))) $
       throwError $ Pretty.moduleContainsDuplicateUses me uses
     let unprefixed = filter (not . Name.isEmptyName . snd) uses
     when (length (map snd unprefixed) /= length (List.nub (map snd unprefixed))) $
       throwError $ Pretty.moduleContainsDuplicateQualifiers me uses
     return (Module.initial SrcT me uses) { modDecls = body }

preloadModule :: FileSystem -> Name -> LoaderM [Module]
preloadModule fs =
  preloadModule' [] Set.empty . (:[])
  where
    preloadModule' :: [Module] -> Set Name -> [Name] -> LoaderM [Module]
    preloadModule' mods _ [] = return mods
    preloadModule' mods loaded (modName:modNames)
      | modName `Set.member` loaded =
        preloadModule' mods loaded modNames
      | fs `FileSystem.member` modName =
        case FileSystem.lookup fs modName of
          Nothing -> throwError $ Pretty.moduleNotFound modName
          Just mod -> preloadModule' (mod:mods) (Set.insert modName loaded) (Module.dependencies mod ++ modNames)
      | otherwise =
        do mod <- loadModule modName
           preloadModule' (mod:mods) (Set.insert modName loaded) (Module.dependencies mod ++ modNames)

buildNodes :: [Module] -> Map Name Int
buildNodes mods =
  Map.fromList [ (Module.modName mod, i) | mod <- mods | i <- [1..] ]

buildEdges :: Map Name Int -> [Module] -> [(Int, Int)]
buildEdges nodes =
  concatMap (\mod -> zip (repeat (nodes Map.! Module.modName mod)) (map (nodes Map.!) (Module.dependencies mod)))

buildGraph :: [Module] -> Either [Module] [Module]
buildGraph mods =
  let edges = buildEdges (buildNodes mods) mods in
  acyclicTopSort mods edges

preload :: FileSystem -> Name -> IO (Either PrettyString [Module])
preload fs modName =
  runResultT (preloadModule fs modName) >>=
    \case
      Bad err -> return $ Left err
      Ok mods ->
         case buildGraph mods of
           Left mods' -> return . Left . Pretty.moduleCycle $ map Module.modName mods'
           Right mods' -> return $ Right mods'
