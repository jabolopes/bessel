module Expander.Variant where

import Data.Name (Name)
import qualified Data.Name as Name
import Data.Source (Source(..))
import qualified Data.Source as Source
import Expander.Type
import Monad.NameM (NameM)
import qualified Monad.NameM as NameM

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

genCondResult :: Monad m => Maybe Source -> NameM m Source
genCondResult guard =
  do resultName <- NameM.genNameM $ Name.untyped "r"
     return $ CondS [([PatS resultName guard], IdS resultName)]

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
genTagPredicate :: Monad m => Name -> Name -> Int -> NameM m Source
genTagPredicate typeName tagName tagNum =
  do fnName <- NameM.genNameM $ Name.untyped "fn"
     argName <- NameM.genNameM $ Name.untyped "arg"
     return $ FnDefS (PatS (genIsTagName tagName) Nothing) Nothing (body fnName argName) []
  where
    condBody fnName argName =
      Source.listToApp [IdS isVariantName, Source.stringS (Name.nameStr typeName), Source.intS tagNum, IdS fnName, IdS argName]

    body fnName argName =
      CondS [([PatS fnName Nothing, PatS argName (Just . IdS $ genIsTypeName typeName)], condBody fnName argName)]

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
genTagConstructor :: Monad m => Name -> Name -> Int -> Source -> NameM m Source
genTagConstructor typeName tagName tagNum pat =
  do body <- genBody pat
     return $ FnDefS (PatS (genMkTagName tagName) Nothing) Nothing body []
  where
    genCondBody binder =
      do condResult <- genCondResult . Just . IdS $ genIsTypeName typeName
         return $ condResult `AppS`
           Source.listToApp [IdS mkVariantName, Source.stringS (Name.nameStr typeName), Source.intS tagNum, binder]

    genBody src@(TupleS []) =
      genCondBody src
    genBody src@(PatS binder _) =
      do condBody <- genCondBody $ IdS binder
         return $ CondS [([src], condBody)]
    genBody src =
      do argName <- NameM.genNameM $ Name.untyped "arg"
         genBody . PatS argName $ Just src

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
genTagDeconstructor :: Monad m => Name -> Name -> Source -> NameM m Source
genTagDeconstructor typeName tagName pat =
  do body <- genBody
     return $ FnDefS (PatS (genUnTagName tagName) Nothing) Nothing body []
  where
    guard (PatS _ (Just src)) = Just src
    guard (PatS _ Nothing) = Nothing
    guard src = Just src

    genCondBody argName =
      do condResult <- genCondResult (guard pat)
         return $ condResult `AppS` (IdS unVariantName `AppS` IdS argName)

    genBody =
      do argName <- NameM.genNameM $ Name.untyped "arg"
         condBody <- genCondBody argName
         return $ CondS [([PatS argName (Just . IdS $ genIsTypeName typeName)], condBody)]
