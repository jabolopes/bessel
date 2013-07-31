{-# LANGUAGE ParallelListComp #-}
module Reorderer where

import Data.List (partition)
-- import Data.GraphUtils (acyclicTopSort)
-- import Data.Map (Map)
-- import qualified Data.Map as Map (fromList, lookup)
-- import Data.Maybe (catMaybes)

import Data.Definition (Definition(..))
import qualified Data.Definition as Definition (initial, prefixedUses, unprefixedUses)
import Data.SrcFile (SrcFileT(..), SrcFile(..))
import qualified Data.SrcFile as SrcFile (addDefinitions, name, prefixedUses, decls, unprefixedUses)
import Data.Expr
import Data.QualName (QualName)
import qualified Data.QualName as QualName (fromQualName)
import Data.Type
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


-- |
-- @
-- (f, Int)
-- @
-- 
-- @
-- sig f : {f:Int} -> Int
-- def f x@ = index 0 (un# x)
-- @
splitDefn :: SrcFile -> Expr -> [Definition]
splitDefn srcfile (CotypeDecl coT@(CoT obs)) =
  concat [ splitDefn srcfile (defn ob i) | ob <- obs | i <- [0..] ]
  where var = "x"

        pat = namePat var (mkPredPat constTrueE)

        body i =
          AppE
          (appE "index" (IntE i))
          (appE "un#" (idE var))

        lambda name i =
          LambdaMacro [] (CondMacro [([pat], body i)] name)

        defn (name, t) i =
          let str = QualName.fromQualName name in
          FnDecl (Just (ArrowT coT t)) NrDef str (lambda str i)

splitDefn srcfile expr@(FnDecl _ _ name _) =
    let
        srcfileName = SrcFile.name srcfile
        defName = srcfileName ++ "." ++ name
        unprefixed = SrcFile.unprefixedUses srcfile
        prefixed = SrcFile.prefixedUses srcfile
    in
      (:[]) $ (Definition.initial defName) { Definition.unprefixedUses = srcfileName:unprefixed
                                           , Definition.prefixedUses = prefixed
                                           , srcExpr = Just expr }

-- splitDefn stx@(ModuleStx [] ns) =
--     error "Reorderer.splitDefn(ModuleStx): not implemented for unnamed modules"

-- splitDefn stx@(ModuleStx prefix ns) =
--     (Definition.initial (flattenId prefix)) { srcStx = Just stx }


reorder :: SrcFile -> SrcFile
reorder srcfile@SrcFile { t = CoreT } = srcfile

reorder srcfile =
  SrcFile.addDefinitions srcfile $ concatMap (splitDefn srcfile) (SrcFile.decls srcfile)