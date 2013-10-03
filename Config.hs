module Config (isPrelude, preludeName) where

preludeNames :: [String]
preludeNames = ["Prelude"]

isPrelude :: String -> Bool
isPrelude = (`elem` preludeNames)

preludeName :: String
preludeName = head preludeNames
