module Config where

import Data.Name (Name)
import qualified Data.Name as Name

coreName :: Name
coreName = Name.untyped "Core"

isCore :: Name -> Bool
isCore = (== coreName)

preludeName :: Name
preludeName = Name.untyped "Prelude"

isPrelude :: Name -> Bool
isPrelude = (== preludeName)
