module Data.QualName where

import qualified Data.Char as Char (isUpper)

import Utils (flattenId, splitId)

newtype QualName
  = QualName { fromQualName :: String }
    deriving (Eq, Ord)

instance Show QualName where
  show QualName { fromQualName = x } = x

mkQualName :: [String] -> QualName
mkQualName name =
  QualName { fromQualName = flattenId name }

isTypeName :: QualName -> Bool
isTypeName = isTypeName' . last . splitId . fromQualName
  where isTypeName' (x:_) = Char.isUpper x
        isTypeName' _ = False
