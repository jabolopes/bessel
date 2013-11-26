module Data.TypeName where

import Utils (flattenId)

newtype TypeName
  = TypeName { fromTypeName :: String }
    deriving (Eq, Ord)

instance Show TypeName where
  show TypeName { fromTypeName = x } = x

mkTypeName :: [String] -> TypeName
mkTypeName name =
  TypeName { fromTypeName = flattenId name }
