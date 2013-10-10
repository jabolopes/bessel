module Pretty.Loader where

import Data.PrettyString (PrettyString, (<+>), ($+$))
import qualified Data.PrettyString as PrettyString

moduleMeMismatch :: String -> String -> PrettyString
moduleMeMismatch me filename =
  (PrettyString.text "module" <+> PrettyString.text me <+>
   PrettyString.text "and filename mismatch")
  $+$
  PrettyString.nest (PrettyString.text "module me is" <+>
                     PrettyString.text me
                     $+$
                     PrettyString.text "module filename is" <+>
                     PrettyString.text filename)
