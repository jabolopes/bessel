{-# LANGUAGE NamedFieldPuns, ParallelListComp,
             TupleSections #-}
module Loader where

import Prelude hiding (lex)

import Data.Map (Map)
import qualified Data.Map as Map ((!), fromList)
import qualified Data.Set as Set (empty, member, insert)

import Config (isPrelude)
import Data.Exception (throwLoaderException, throwParserException)
import Data.FileSystem (FileSystem)
import qualified Data.FileSystem as FileSystem (get, member)
import Data.GraphUtils (acyclicTopSort)
import Data.SrcFile
import qualified Data.SrcFile as SrcFile (name, deps)
import Parser (parsePrelude, parseFile)
import Utils (singleton)


readFileM :: String -> IO String
readFileM filename = readFile $ toFilename filename ++ ".bsl"
    where toFilename = map f
              where f '.' = '/'
                    f c = c


loadSrcFileM :: String -> IO SrcFile
loadSrcFileM filename =
    do str <- readFileM filename
       let parseFn | isPrelude filename = parsePrelude
                   | otherwise = parseFile
           srcfile = case parseFn filename str of
                       Left str -> throwParserException str
                       Right x -> x
       if SrcFile.name srcfile /= filename
       then throwLoaderException $ "me " ++ show (SrcFile.name srcfile) ++ " and filename " ++ show filename ++ " mismatch"
       else return srcfile


preloadSrcFile :: FileSystem -> String -> IO [SrcFile]
preloadSrcFile fs filename = preloadSrcFile' [] Set.empty [filename]
    where preloadSrcFile' srcfiles _ [] = return srcfiles
          preloadSrcFile' srcfiles loaded (filename:filenames)
              | filename `Set.member` loaded =
                  preloadSrcFile' srcfiles loaded filenames
              | fs `FileSystem.member` filename =
                  do let srcfile = FileSystem.get fs filename
                     preloadSrcFile' (srcfile:srcfiles) (Set.insert filename loaded) (deps srcfile ++ filenames)
              | otherwise =
                  do srcfile <- loadSrcFileM filename
                     preloadSrcFile' (srcfile:srcfiles) (Set.insert filename loaded) (deps srcfile ++ filenames)


buildNodes :: [SrcFile] -> Map String Int
buildNodes srcfiles =
    Map.fromList [ (SrcFile.name srcfile, i) | srcfile <- srcfiles | i <- [1..] ]


buildEdges :: Map String Int -> [SrcFile] -> [(Int, Int)]
buildEdges nodes =
    concatMap (\srcfile -> zip (repeat (nodes Map.! SrcFile.name srcfile)) (map (nodes Map.!) (SrcFile.deps srcfile)))


buildGraph :: [SrcFile] -> Either [SrcFile] [SrcFile]
buildGraph srcfiles =
    let edges = buildEdges (buildNodes srcfiles) srcfiles in
    acyclicTopSort srcfiles edges


preload :: FileSystem -> String -> IO [SrcFile]
preload fs filename =
    do srcfiles <- preloadSrcFile fs filename
       case buildGraph srcfiles of
         Left srcfiles' -> error $ "Loader.preload: module cycle in " ++ show (map SrcFile.name srcfiles')
         Right srcfiles' -> return srcfiles'