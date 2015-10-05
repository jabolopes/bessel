module Pretty.Repl where

import Data.Name (Name)
import Data.PrettyString (PrettyString, ($+$))
import qualified Data.PrettyString as PrettyString

docModuleNames :: [Name] -> PrettyString
docModuleNames names =
  PrettyString.text "Staged modules"
  $+$
  (PrettyString.nest $
    PrettyString.vcat $
      map (PrettyString.text . show) names)
