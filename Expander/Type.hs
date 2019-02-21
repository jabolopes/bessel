module Expander.Type where

import Data.Name (Name)
import qualified Data.Name as Name
import Data.Source (Source(..))
import qualified Data.Source as Source
import Typechecker.Type (Type(..))
import qualified Typechecker.Type as Type

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
genIsTypeName name =
  Name.untyped ("is" ++ Name.nameStr name)

genTypePredicate :: Name -> Source
genTypePredicate typeName =
  FnDefS (PatS fnName Nothing) Nothing body []
  where
    typ = PrimitiveT (Name.nameStr typeName) `Arrow` Type.boolT

    fnName = Name.annotate (genIsTypeName typeName) typ

    body = IdS isTypeName `AppS` Source.stringS (Name.nameStr typeName)
