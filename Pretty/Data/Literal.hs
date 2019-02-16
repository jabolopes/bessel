module Pretty.Data.Literal where

import Data.Literal (Literal(..))
import Data.PrettyString (PrettyString)
import qualified Data.PrettyString as PrettyString

docLiteral :: Literal -> PrettyString
docLiteral (CharL c) = PrettyString.quotes $ PrettyString.char c
docLiteral (IntL i) = PrettyString.int i
docLiteral (RealL d) =  PrettyString.double d
docLiteral (StringL s) = PrettyString.string s
