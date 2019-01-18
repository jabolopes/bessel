module Typechecker.TypeName where

import Data.Char (chr, ord)

newtype TypeName = TypeName Int
  deriving (Eq, Ord)

instance Show TypeName where
  show (TypeName i)
    | i > 25 =
      "a" ++ show (i - 25)
    | otherwise =
      [chr $ ord 'a' + i]

typeName :: Char -> TypeName
typeName c = TypeName $ ord c - ord 'a'
