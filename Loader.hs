{-# LANGUAGE NamedFieldPuns, ParallelListComp,
             TupleSections #-}
module Loader where

import Prelude hiding (lex)

import Control.Monad.State
import Data.Functor ((<$>))
import Data.Graph (buildG, topSort, scc)
import Data.List (intercalate, nub, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree

import Config
import Data.SrcFile
import qualified Data.SrcFile as SrcFile
import Data.Stx
import Data.Pat
import Lexer (lex)
import Parser (parsePrelude, parseFile)
import Utils


type LoaderM a = StateT [String] IO a


dependenciesNsM :: Namespace String -> LoaderM ()
dependenciesNsM (Namespace uses stxs) =
    modify (map fst uses ++) >> mapM_ dependenciesM stxs


dependenciesM :: Stx String -> LoaderM ()
dependenciesM (CharStx _) = return ()
dependenciesM (IntStx _) = return ()
dependenciesM (DoubleStx _) = return ()
dependenciesM (SeqStx stxs) = mapM_ dependenciesM stxs
dependenciesM (IdStx _) = return ()
dependenciesM (AppStx stx1 stx2) = dependenciesM stx1 >> dependenciesM stx2
dependenciesM (DefnStx _ _ body) = dependenciesM body
dependenciesM (LambdaStx _ body) = dependenciesM body
dependenciesM (ModuleStx _ ns) = dependenciesNsM ns
dependenciesM (TypeStx _ stxs) = mapM_ dependenciesM stxs
dependenciesM (TypeMkStx _) = return ()
dependenciesM (TypeUnStx) = return ()
dependenciesM (TypeIsStx _) = return ()
dependenciesM (WhereStx _ stxs) = mapM_ dependenciesM stxs


readFileM :: String -> IO String
readFileM filename = readFile $ (toFilename filename) ++ ".fl"
    where toFilename = map f
              where f '.' = '/'
                    f c = c


dependenciesFileM :: String -> IO SrcFile
dependenciesFileM filename =
    do str <- readFileM filename
       let tks = lex str
           parseFn | isPrelude filename = parsePrelude
                   | otherwise = parseFile
           srcfile@SrcFile { name, srcNs = Left ns } = parseFn tks
       (_, deps) <- runStateT (dependenciesNsM ns) []
       let deps' = nub $ sort deps
       if name /= filename
       then error $ "Loader.dependenciesFileM: me " ++ show name ++ " and filename " ++ show filename ++ " mismatch"
       else return $ srcfile { deps = deps' }


preloadSrcFile :: Map String SrcFile -> String -> IO [SrcFile]
preloadSrcFile corefiles filename = preloadSrcFile' [] Set.empty [filename]
    where preloadSrcFile' srcfiles _ [] = return srcfiles
          preloadSrcFile' srcfiles loaded (filename:filenames)
              | filename `Set.member` loaded =
                  preloadSrcFile' srcfiles loaded filenames
              | filename `Map.member` corefiles =
                  do let srcfile = corefiles Map.! filename
                     preloadSrcFile' (srcfile:srcfiles) (Set.insert filename loaded) (deps srcfile ++ filenames)
              | otherwise =
                  do srcfile <- dependenciesFileM filename
                     preloadSrcFile' (srcfile:srcfiles) (Set.insert filename loaded) (deps srcfile ++ filenames)


buildNodes :: [SrcFile] -> Map String Int
buildNodes srcfiles =
    Map.fromList [ (SrcFile.name srcfile, i) | srcfile <- srcfiles | i <- [1..] ]


buildEdges :: Map String Int -> [SrcFile] -> [(Int, Int)]
buildEdges nodes srcfiles =
    concatMap (\srcfile -> zip (repeat (nodes Map.! SrcFile.name srcfile)) (map (nodes Map.!) (SrcFile.deps srcfile))) srcfiles


replaceIndexes :: [SrcFile] -> [Int] -> [SrcFile]
replaceIndexes srcfiles is = [ srcfiles !! (i - 1) | i <- is ]


hasCycle :: [[[Int]]] -> Maybe [Int]
hasCycle [] = Nothing
hasCycle (is:iss)
    | singleton is = hasCycle iss
    | otherwise = Just $ concat is


buildGraph :: [SrcFile] -> Either [SrcFile] [SrcFile]
buildGraph srcfiles =
    let
        nodes = buildNodes srcfiles
        edges = buildEdges nodes srcfiles
        graph = buildG (1, length srcfiles) edges
        srcfiles' = replaceIndexes srcfiles $ topSort graph
    in
      case hasCycle $ map levels $ scc graph of
        Nothing -> Right $ reverse srcfiles'
        Just is -> Left $ replaceIndexes srcfiles is


preload :: Map String SrcFile -> String -> IO [SrcFile]
preload fs filename =
    do srcfiles <- preloadSrcFile fs filename
       case buildGraph srcfiles of
         Left srcfiles' -> error $ "Loader.preload: module cycle in " ++ show (map SrcFile.name srcfiles')
         Right srcfiles' -> return srcfiles'