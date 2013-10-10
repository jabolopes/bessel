module Config where

coreName :: String
coreName = "Core"

isCore :: String -> Bool
isCore = (== coreName)

preludeName :: String
preludeName = "Prelude"

isPrelude :: String -> Bool
isPrelude = (== preludeName)
