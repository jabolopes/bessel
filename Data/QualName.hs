module Data.QualName where

import qualified Data.Char as Char (isUpper)

import Utils (flattenId, splitId)

newtype QualName
  = QualName { fromQualName :: String }
  deriving (Eq, Show)

mkQualName :: [String] -> QualName
mkQualName name =
  QualName { fromQualName = flattenId name }

-- | Returns a 'QualName' from a qualified name.
-- @
-- qualified "Data.Map" = QualName "Data.Map"
-- @
qualified :: String -> QualName
qualified = mkQualName . Utils.splitId

-- Returns a 'QualName' from an unqualified name, from which occurrences of the
-- period character (i.e., '.')  are stripped.
-- @
-- unqualified "Map" = QualName "Map"
-- unqualified "Data.Map" = QualName "DataMap"
-- @
unqualified :: String -> QualName
unqualified = QualName . filter (/= '.')

joinNames :: QualName -> QualName -> QualName
joinNames QualName { fromQualName = name1 } QualName { fromQualName = name2 } =
  QualName { fromQualName = flattenId [name1, name2] }

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
