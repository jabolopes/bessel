-- | Expansions of variants.
--
-- Variants can have a pattern. Variants without a pattern are handled
-- in a special way, namely, they use isVariant0# and mkVariant0# and
-- they don't have a destructor. If the variant has a pattern that is
-- a predicate, that predicate is used in the expansions. If that
-- pattern is a type predicate, then the type predicate is first
-- expanded to a type predicate.
--
-- TODO: Finish type patterns.
--
-- @
-- type Fruit
--   = Apple
--   | Banana @isInt
--   | Fig x@(@isInt, @isReal)
--   | Orange @isFruit
--
-- let isApple = (arg@isFruit = isFruit arg && isVariant0# "Fruit" 0 arg)
-- let isBanana fn = (arg@isFruit = isFruit arg && isVariant# "Fruit" 1 fn arg)
-- let isFig    fn = (arg@isFruit = isFruit arg && isVariant# "Fruit" 2 fn arg)
-- let isOrange fn = (arg@isFruit = isFruit arg && isVariant# "Fruit" 3 fn arg)
--
-- let mkApple = (r@isFruit = r) $ mkVariant0# "Fruit" 0
-- let mkBanana (arg@isInt = (r@isFruit = r) $ mkVariant# "Fruit" 1 arg)
-- let mkFig    (x@(@isInt, @isReal) = (r@isFruit = r) $ mkVariant# "Fruit" 2 x)
-- let mkOrange (arg@isFruit = (r@isFruit = r) $ mkVariant# "Fruit" 3 arg)
--
-- let unBanana (arg@isFruit = (r@isInt = r) $ unVariant arg)
-- let unFig    (arg@isFruit = (x@(@isInt, @isReal) = x) $ unVariant arg)
-- let unOrange (arg@isFruit = (r@isFruit = x) $ unVariant arg)
--
-- Note that unApple does not exist.
-- @
module Expander.Variant where

import Data.Name (Name)
import qualified Data.Name as Name
import Data.Source (Source(..))
import qualified Data.Source as Source
import qualified Expander.Cast as Cast
import Expander.Type
import Monad.NameM (MonadName)
import qualified Monad.NameM as NameM

-- Runtime.

isVariant0Name :: Name
isVariant0Name = Name.untyped "isVariant0#"

isVariantName :: Name
isVariantName = Name.untyped "isVariant#"

mkVariant0Name :: Name
mkVariant0Name = Name.untyped "mkVariant0#"

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

-- | Generates a predicate for a variant tag.
genTagPredicate :: MonadName m => Name -> Name -> Int -> Maybe Source -> m Source
genTagPredicate typeName tagName tagNum pat =
  do body <- genBody pat
     return $ FnDefS (PatS (genIsTagName tagName) Nothing) Nothing body []
  where
    genBody Nothing =
      Cast.castArgument (IdS $ genIsTypeName typeName) $ \argName ->
        return $ Source.listToApp [IdS isVariant0Name, Source.stringS (Name.nameStr typeName), Source.intS tagNum, IdS argName]
    genBody (Just _) =
      do fnName <- NameM.genName $ Name.untyped "fn"
         body <- Cast.castArgument (IdS $ genIsTypeName typeName) $ \argName ->
           return $ Source.listToApp [IdS isVariantName, Source.stringS (Name.nameStr typeName), Source.intS tagNum, IdS fnName, IdS argName]
         return $ CondS [([PatS fnName Nothing], body)]

-- | Generates a constructor for a variant tag.
genTagConstructor :: MonadName m => Name -> Name -> Int -> Maybe Source -> m Source
genTagConstructor typeName tagName tagNum pat =
  do body <- genBody pat
     return $ FnDefS (PatS (genMkTagName tagName) Nothing) Nothing body []
  where
    genBody Nothing =
      do condResult <- Cast.castResult . IdS $ genIsTypeName typeName
         return $ condResult `AppS`
           Source.listToApp [IdS mkVariant0Name, Source.stringS (Name.nameStr typeName), Source.intS tagNum]
    genBody (Just src) =
      Cast.castFunction src (IdS $ genIsTypeName typeName) $ \argName ->
        return $ Source.listToApp [IdS mkVariantName, Source.stringS (Name.nameStr typeName), Source.intS tagNum, IdS argName]

-- | Generates a deconstructor for a variant tag.
genTagDeconstructor :: MonadName m => Name -> Name -> Source -> m Source
genTagDeconstructor typeName tagName pat =
  do body <- genBody
     return $ FnDefS (PatS (genUnTagName tagName) Nothing) Nothing body []
  where
    genBody =
      Cast.castFunction (IdS $ genIsTypeName typeName) pat $ \argName ->
        return (IdS unVariantName `AppS` IdS argName)
