module Data.QualName where

import qualified Data.Char as Char (isUpper)

import Utils (flattenId, splitId)

newtype QualName
  = QualName { fromQualName :: String }
  deriving (Show)

mkQualName :: [String] -> QualName
mkQualName name =
  QualName { fromQualName = flattenId name }

isTypeName :: QualName -> Bool
isTypeName = isTypeName' . last . splitId . fromQualName
  where isTypeName' (x:_) = Char.isUpper x
        isTypeName' _ = False
