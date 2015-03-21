module Data.QualName where

import qualified Data.Char as Char (isUpper)

import Utils (flattenId, splitId)

newtype QualName
  = QualName { fromQualName :: String }
  deriving (Show)

mkQualName :: [String] -> QualName
mkQualName name =
  QualName { fromQualName = flattenId name }

isEmptyName :: QualName -> Bool
isEmptyName QualName { fromQualName = "" } = True
isEmptyName _ = False

isTypeName :: QualName -> Bool
isTypeName name
  | isEmptyName name = False
  | otherwise = isTypeName' . last . splitId $ fromQualName name
  where
    isTypeName' (x:_) = Char.isUpper x
    isTypeName' _ = False

moduleName :: QualName -> String
moduleName = flattenId . init . splitId . fromQualName

definitionName :: QualName -> String
definitionName = last . splitId . fromQualName
