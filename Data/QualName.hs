module Data.QualName (QualName,
                      mkQualName,
                      fromQualName,
                      isTypeName,
                      moduleName,
                      definitionName) where

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
  where
    isTypeName' (x:_) = Char.isUpper x
    isTypeName' _ = False

moduleName :: QualName -> String
moduleName = flattenId . init . splitId . fromQualName

definitionName :: QualName -> String
definitionName = last . splitId . fromQualName
