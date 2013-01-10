{-# LANGUAGE ParallelListComp, TupleSections #-}
module Loader (preload) where

import Control.Monad.State
import Data.Functor ((<$>))
import Data.Graph (buildG, topSort, scc)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree

import Data.SrcFile
import Data.Stx
import Data.Pat
import qualified Lexer (lex)
import Parser (parsePrelude, parseFile)
import Utils


type LoaderM a = StateT [String] IO a


dependenciesNsM (Namespace uses stxs) =
    modify (uses ++) >> mapM_ dependenciesM stxs


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
dependenciesM (TypeMkStx _ _) = return ()
dependenciesM (TypeUnStx _ _) = return ()
dependenciesM (TypeIsStx _ _) = return ()
dependenciesM (WhereStx _ stxs) = mapM_ dependenciesM stxs


dependenciesFileM :: String -> IO SrcFile
dependenciesFileM filename =
    do str <- readFile $ (toFilename filename) ++ ".fl"
       let tks = Lexer.lex str
           parseFn | filename == "Prelude" = parsePrelude
                   | otherwise = parseFile
           (SrcFile name [] Nothing content@(Left ns)) = parseFn tks
       (_, deps) <- runStateT (dependenciesNsM ns) []
       if name /= filename
       then error $ "Loader.dependenciesFileM: me " ++ show name ++ " and filename " ++ show filename ++ " mismatch"
       else return $ SrcFile name deps Nothing content
    where toFilename = map f
              where f '.' = '/'
                    f c = c


preloadSrcFile :: Map String SrcFile -> String -> IO [SrcFile]
preloadSrcFile corefiles filename = preloadSrcFile' [] Set.empty [filename]
    where preloadSrcFile' srcfiles _ [] = return srcfiles
          preloadSrcFile' srcfiles loaded (filename:filenames)
              | filename `Set.member` loaded =
                  preloadSrcFile' srcfiles loaded filenames
              | filename `Map.member` corefiles =
                  do let srcfile@(SrcFile _ deps _ _) = corefiles Map.! filename
                     preloadSrcFile' (srcfile:srcfiles) (Set.insert filename loaded) (deps ++ filenames)
              | otherwise =
                  do srcfile@(SrcFile _ deps _ _) <- dependenciesFileM filename
                     preloadSrcFile' (srcfile:srcfiles) (Set.insert filename loaded) (deps ++ filenames)


buildNodes :: [SrcFile] -> Map String Int
buildNodes srcfiles =
    Map.fromList [ (name, i) | SrcFile name _ _ _ <- srcfiles | i <- [1..] ]


buildEdges :: Map String Int -> [SrcFile] -> [(Int, Int)]
buildEdges nodes srcfiles =
    concatMap (\(SrcFile name deps _ _) -> zip (repeat (nodes Map.! name)) (map (nodes Map.!) deps)) srcfiles


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


preload :: [SrcFile] -> String -> IO [SrcFile]
preload corefiles filename =
    do srcfiles <- preloadSrcFile corefilesMp filename
       case buildGraph srcfiles of
         Left srcfiles' -> error $ "Loader.preload: module cycle in " ++ show srcfiles'
         Right srcfiles' -> return srcfiles'
    where corefilesMp = Map.fromList [ (name, srcfile) | srcfile@(SrcFile name _ _ _) <- corefiles ]