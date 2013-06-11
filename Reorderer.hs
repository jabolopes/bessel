{-# LANGUAGE ParallelListComp #-}
module Reorderer where

import Data.List (partition)
import Data.GraphUtils (acyclicTopSort)
import Data.Map (Map)
import qualified Data.Map as Map (fromList, lookup)
import Data.Maybe (catMaybes)

import Data.SrcFile
import Data.Stx


reorderStx :: Stx String -> (String, [String])
reorderStx stx@(DefnStx _ _ name body) = (name, freeVars stx)
reorderStx _ = error "Reoderer.reorderStx: unhandled case"


buildEdges :: Map String Int -> [(String, [String])] -> [(Int, Int)]
buildEdges _ [] = []
buildEdges nodes ((name, vars):fvars) =
    let edges = buildEdges nodes fvars in
    case Map.lookup name nodes of
      Nothing -> edges
      Just i -> catMaybes (map (buildEdge i) vars) ++ edges
    where buildEdge i var =
              case Map.lookup var nodes of
                Nothing -> Nothing
                Just j -> Just (i, j)


reorderNamespace :: Namespace String -> Namespace String
reorderNamespace (Namespace uses stxs) =
    let
        (defns, stxs') = partition isDefnStx stxs
        fvars = map reorderStx defns
        nodes = Map.fromList [ (name, i) | i <- [1..] | (name, _) <- fvars ]
        edges = buildEdges nodes fvars
    in
      case acyclicTopSort defns edges of
        Left _ -> error "Reorderer: mutually recursive functions are not implemented"
        Right defns' -> Namespace uses (stxs' ++ defns')


reorder :: SrcFile -> SrcFile
reorder srcfile@SrcFile { t = CoreT } =
    srcfile

reorder srcfile =
    let Just ns = srcNs srcfile in
    srcfile { srcNs = Just (reorderNamespace ns) }