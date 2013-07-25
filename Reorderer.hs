{-# LANGUAGE ParallelListComp #-}
module Reorderer where

import Data.List (partition)
import Data.GraphUtils (acyclicTopSort)
import Data.Map (Map)
import qualified Data.Map as Map (fromList, lookup)
import Data.Maybe (catMaybes)

import Data.Definition (Definition(..))
import qualified Data.Definition as Definition (initial)
import Data.SrcFile (SrcFileT(..), SrcFile(..))
import qualified Data.SrcFile as SrcFile (addDefinitions, name)
import Data.Stx
import Utils (flattenId)


-- reorderStx :: Stx String -> (String, [String])
-- reorderStx stx@(DefnStx _ _ name body) = (name, freeVars stx)
-- reorderStx _ = error "Reoderer.reorderStx: unhandled case"


-- buildEdges :: Map String Int -> [(String, [String])] -> [(Int, Int)]
-- buildEdges _ [] = []
-- buildEdges nodes ((name, vars):fvars) =
--     let edges = buildEdges nodes fvars in
--     case Map.lookup name nodes of
--       Nothing -> edges
--       Just i -> catMaybes (map (buildEdge i) vars) ++ edges
--     where buildEdge i var =
--               case Map.lookup var nodes of
--                 Nothing -> Nothing
--                 Just j -> Just (i, j)


-- reorderNamespace :: Namespace String -> Namespace String
-- reorderNamespace (Namespace uses stxs) =
--     let
--         (defns, stxs') = partition isDefnStx stxs
--         fvars = map reorderStx defns
--         nodes = Map.fromList [ (name, i) | i <- [1..] | (name, _) <- fvars ]
--         edges = buildEdges nodes fvars
--     in
--       case acyclicTopSort defns edges of
--         Left _ -> error "Reorderer: mutually recursive functions are not implemented"
--         Right defns' -> Namespace uses (stxs' ++ defns')


splitDefnOrModule :: SrcFile -> Stx String -> Definition
splitDefnOrModule srcfile@SrcFile { srcNs = Just (Namespace uses ns) } stx@(DefnStx _ _ name _) =
    let
        srcfileName = SrcFile.name srcfile
        defName = srcfileName ++ "." ++ name
        (unprefixed, prefixed) = partition (null . snd) uses
        unprefixed' = map fst unprefixed
    in
      (Definition.initial defName) { unprefixedUses = srcfileName:unprefixed'
                                   , prefixedUses = prefixed
                                   , srcStx = Just stx }

-- splitDefnOrModule stx@(ModuleStx [] ns) =
--     error "Reorderer.splitDefnOrModule(ModuleStx): not implemented for unnamed modules"

-- splitDefnOrModule stx@(ModuleStx prefix ns) =
--     (Definition.initial (flattenId prefix)) { srcStx = Just stx }


splitNamespace :: SrcFile -> Namespace String -> [Definition]
splitNamespace srcfile ns@(Namespace _ stxs) =
    map (splitDefnOrModule srcfile) stxs


reorder :: SrcFile -> SrcFile
reorder srcfile@SrcFile { t = CoreT } = srcfile

reorder srcfile@SrcFile { srcNs = Just ns } =
    SrcFile.addDefinitions srcfile (splitNamespace srcfile ns)