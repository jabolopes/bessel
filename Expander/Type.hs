module Expander.Type where

import Data.Name (Name)
import qualified Data.Name as Name
import Data.Source (Source(..))
import qualified Data.Source as Source

-- Runtime.

isTypeName :: Name
isTypeName = Name.untyped "isType#"

-- Types.

-- | Returns a name to test a variant type
--
-- For example, for the type @Fruit@
-- @
-- type Fruit = ...
-- @
-- returns @isFruit@
genIsTypeName :: Name -> Name
genIsTypeName = Name.untyped . ("is" ++) . Name.nameStr

genTypePredicate :: Name -> Source
genTypePredicate typeName =
  FnDefS (PatS (genIsTypeName typeName) Nothing) Nothing body []
  where
    body = IdS isTypeName `AppS` Source.stringS (Name.nameStr typeName)
