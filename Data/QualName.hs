module Data.QualName where

import Utils (flattenId)

newtype QualName
  = QualName { fromQualName :: String }
    deriving (Eq, Ord)

instance Show QualName where
  show QualName { fromQualName = x } = x

mkQualName :: [String] -> QualName
mkQualName name =
  QualName { fromQualName = flattenId name }