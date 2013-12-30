module Pretty.Repl where

import qualified Data.Map as Map

import Data.Module (ModuleT, Module)
import qualified Data.Module as Module
import Data.PrettyString (PrettyString, (<+>), ($+$))
import qualified Data.PrettyString as PrettyString
import qualified Pretty.Data.Definition as Pretty

docModuleNames :: [String] -> PrettyString
docModuleNames names =
  PrettyString.text "Staged modules"
  $+$
  (PrettyString.nest $
    PrettyString.vcat $
      map PrettyString.text names)
