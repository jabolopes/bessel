{-# LANGUAGE ParallelListComp #-}
module Reorderer where

import Prelude hiding (mod)

import Data.Definition (Definition(..))
import qualified Data.Definition as Definition
import Data.Module (ModuleT(..), Module(Module, modType))
import qualified Data.Module as Module
import Data.Expr

splitDefn :: Module -> Expr -> [Definition]
splitDefn mod expr@(FnDecl _ name _) =
  let
    modName = Module.modName mod
    qualName = modName ++ "." ++ name
    unprefixed = Module.modUnprefixedUses mod
    prefixed = Module.modPrefixedUses mod
  in
    (:[]) $ (Definition.initial qualName) { Definition.defUnprefixedUses = modName:unprefixed
                                          , Definition.defPrefixedUses = prefixed
                                          , defSrc = Just expr }

reorder :: Module -> Module
reorder mod@Module { modType = CoreT } = mod
reorder mod =
  Module.addDefinitions mod $ concatMap (splitDefn mod) (Module.modDecls mod)
