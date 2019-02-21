module Expander.Variant where

import Data.Name (Name)
import qualified Data.Name as Name
import Data.Source (Source(..))
import qualified Data.Source as Source
import Expander.Type

-- Runtime.

isVariantName :: Name
isVariantName = Name.untyped "isVariant#"

mkVariantName :: Name
mkVariantName = Name.untyped "mkVariant#"

unVariantName :: Name
unVariantName = Name.untyped "unVariant#"

-- Variant tags.

genIsTagName :: Name -> Name
genIsTagName = Name.untyped . ("is" ++) . Name.nameStr

genMkTagName :: Name -> Name
genMkTagName = Name.untyped . ("mk" ++) . Name.nameStr

genUnTagName :: Name -> Name
genUnTagName = Name.untyped . ("un" ++) . Name.nameStr

genCondResult :: Name -> Maybe Source -> Source
genCondResult resultName guard =
  CondS [([PatS resultName guard], IdS resultName)]

-- | Generates a predicate for a variant tag.
-- @
-- type Fruit
--   = Apple
--   | Banana @isInt
--   | Fig x@(@isInt, @isReal)
-- @
--
-- @
-- isApple = isVariant "Fruit" 0
-- isBanana = isVariant "Fruit" 1
-- isFig = isVariant "Fruit" 2
-- @
genTagPredicate :: Name -> Name -> Int -> Source
genTagPredicate typeName tagName tagNum =
  FnDefS (PatS (genIsTagName tagName) Nothing) Nothing body []
  where
    fnName = Name.untyped "fn"
    argName = Name.untyped "arg"

    condBody =
      Source.listToApp [IdS isVariantName, Source.stringS (Name.nameStr typeName), Source.intS tagNum, IdS fnName, IdS argName]

    body =
      CondS [([PatS fnName Nothing, PatS argName (Just . IdS $ genIsTypeName typeName)], condBody)]

-- | Generates a constructor for a variant tag.
-- @
-- type Fruit
--   = Apple
--   | Banana @isInt
--   | Fig x@(@isInt, @isReal)
-- @
--
-- @
-- mkApple = mkVariant "Fruit" 0 ()
-- mkBanana x@isInt = mkVariant "Fruit" 1 x
-- mkFig x@(@isInt, @isReal) = mkVariant "Fruit" 2 x
-- @
genTagConstructor :: Name -> Name -> Int -> Source -> Source
genTagConstructor typeName tagName tagNum pat =
  FnDefS (PatS (genMkTagName tagName) Nothing) Nothing (body pat) []
  where
    argName = Name.untyped "arg"
    resultName = Name.untyped "r"

    condBody binder =
      genCondResult resultName (Just . IdS $ genIsTypeName typeName) `AppS`
        Source.listToApp [IdS mkVariantName, Source.stringS (Name.nameStr typeName), Source.intS tagNum, binder]

    body src@(TupleS []) =
      condBody src
    body src@(PatS binder _) =
      CondS [([src], condBody (IdS binder))]
    body src =
      body . PatS argName $ Just src

-- | Generates a deconstructor for a variant tag.
-- @
-- type Fruit =
--   = Apple
--   | Banana @isInt
--   | Fig (@isInt, @isReal)
-- @
--
-- @
-- unApple x@isFruit = (r@() -> r) $ unVariant x
-- unBanana x@isFruit = (r@isInt -> r) $ unVariant x
-- unFig x@isFruit = (r@(@isInt, @isReal) -> r) $ unVariant x
-- @
genTagDeconstructor :: Name -> Name -> Source -> Source
genTagDeconstructor typeName tagName pat =
  FnDefS (PatS (genUnTagName tagName) Nothing) Nothing body []
  where
    argName = Name.untyped "arg"
    resultName = Name.untyped "r"

    guard (PatS _ (Just src)) = Just src
    guard (PatS _ Nothing) = Nothing
    guard src = Just src

    condBody =
      genCondResult resultName (guard pat) `AppS` (IdS unVariantName `AppS` IdS argName)

    body =
      CondS [([PatS argName (Just . IdS $ genIsTypeName typeName)], condBody)]
