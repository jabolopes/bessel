module Reorderer where

import Prelude hiding (mod)

import Data.Definition (Definition(..))
import qualified Data.Definition as Definition
import Data.Macro (Macro(..))
import Data.Module (ModuleT(..), Module(Module, modType))
import qualified Data.Module as Module

splitDefn :: Module -> Macro -> [Definition]
splitDefn mod macro@(FnDeclM name _) =
  let
    modName = Module.modName mod
    qualName = modName ++ "." ++ name
    unprefixed = modName:Module.modUnprefixedUses mod
    prefixed = Module.modPrefixedUses mod
  in
    (:[]) $ (Definition.initial qualName) { Definition.defUnprefixedUses = unprefixed
                                          , Definition.defPrefixedUses = prefixed
                                          , defMac = Right macro }

reorder :: Module -> Module
reorder mod@Module { modType = CoreT } = mod
reorder mod =
  Module.addDefinitions mod $ concatMap (splitDefn mod) (Module.modDecls mod)
