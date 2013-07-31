module Data.QualName where


newtype QualName
  = QualName { fromQualName :: String }
    deriving (Eq, Ord, Show)