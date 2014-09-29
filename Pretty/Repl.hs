module Pretty.Repl where

import Data.PrettyString (PrettyString, ($+$))
import qualified Data.PrettyString as PrettyString

docModuleNames :: [String] -> PrettyString
docModuleNames names =
  PrettyString.text "Staged modules"
  $+$
  (PrettyString.nest $
    PrettyString.vcat $
      map PrettyString.text names)
