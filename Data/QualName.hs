module Data.QualName where


newtype QualName
  = QualName { fromQualName :: String }
    deriving (Eq, Ord)

instance Show QualName where
  show QualName { fromQualName = x } = x