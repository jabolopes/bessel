{-# LANGUAGE ParallelListComp #-}
module Reorderer where

import Data.Definition (Definition(..))
import qualified Data.Definition as Definition
import Data.SrcFile (SrcFileT(..), SrcFile(..))
import qualified Data.SrcFile as SrcFile (addDefinitions, name, prefixedUses, decls, unprefixedUses)
import Data.Expr

splitDefn :: SrcFile -> Expr -> [Definition]
splitDefn srcfile expr@(FnDecl _ name _) =
    let
        srcfileName = SrcFile.name srcfile
        qualName = srcfileName ++ "." ++ name
        unprefixed = SrcFile.unprefixedUses srcfile
        prefixed = SrcFile.prefixedUses srcfile
    in
      (:[]) $ (Definition.initial qualName) { Definition.defUnprefixedUses = srcfileName:unprefixed
                                           , Definition.defPrefixedUses = prefixed
                                           , defSrc = Just expr }

reorder :: SrcFile -> SrcFile
reorder srcfile@SrcFile { t = CoreT } = srcfile
reorder srcfile =
  SrcFile.addDefinitions srcfile $ concatMap (splitDefn srcfile) (SrcFile.decls srcfile)