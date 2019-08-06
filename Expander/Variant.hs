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
-- isApple arg@isFruit     = isFruit arg && isVariant0# "Fruit" 0 arg
-- isBanana fn arg@isFruit = isFruit arg && isVariant# "Fruit" 1 fn arg
-- isFig fn arg@isFruit    = isFruit arg && isVariant# "Fruit" 2 fn arg
-- isOrange fn arg@isFruit = isFruit arg && isVariant# "Fruit" 3 fn arg
--
-- mkApple                   = (r@isFruit = r) $ mkVariant0# "Fruit" 0
-- mkBanana arg@isInt        = (r@isFruit = r) $ mkVariant# "Fruit" 1 arg
-- mkFig x@(@isInt, @isReal) = (r@isFruit = r) $ mkVariant# "Fruit" 2 x
-- mkOrange isFruit          = (r@isFruit = r) $ mkVariant# "Fruit" 3 arg
--
-- unBanana arg@isFruit = (r@isInt = r) $ unVariant arg
-- unFig arg@isFruit    = (x@(@isInt, @isReal) = x) $ unVariant arg
-- unOrange arg@isFruit = (r@isFruit = x) $ unVariant arg
--
-- Note that unApple does not exist.
-- @
module Expander.Variant where

import Data.Name (Name)
import qualified Data.Name as Name
import Data.Source (Source(..))
import qualified Data.Source as Source
import Expander.Type
import Monad.NameM (NameM)
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

genCondResult :: Monad m => Source -> NameM m Source
genCondResult src@(PatS binder _) =
  return $ CondS [([src], IdS binder)]
genCondResult src =
  do resultName <- NameM.genNameM $ Name.untyped "r"
     genCondResult . PatS resultName $ Just src

-- | Generates a predicate for a variant tag.
genTagPredicate :: Monad m => Name -> Name -> Int -> Maybe Source -> NameM m Source
genTagPredicate typeName tagName tagNum pat =
  do body <- genBody pat
     return $ FnDefS (PatS (genIsTagName tagName) Nothing) Nothing body []
  where
    condBody Nothing argName =
      Source.listToApp [IdS isVariant0Name, Source.stringS (Name.nameStr typeName), Source.intS tagNum, IdS argName]
    condBody (Just fnName) argName =
      Source.listToApp [IdS isVariantName, Source.stringS (Name.nameStr typeName), Source.intS tagNum, IdS fnName, IdS argName]

    genBody Nothing =
      do argName <- NameM.genNameM $ Name.untyped "arg"
         return $ CondS [([PatS argName (Just . IdS $ genIsTypeName typeName)], condBody Nothing argName)]
    genBody (Just _) =
      do fnName <- NameM.genNameM $ Name.untyped "fn"
         argName <- NameM.genNameM $ Name.untyped "arg"
         return $ CondS [([PatS fnName Nothing, PatS argName (Just . IdS $ genIsTypeName typeName)], condBody (Just fnName) argName)]

-- | Generates a constructor for a variant tag.
genTagConstructor :: Monad m => Name -> Name -> Int -> Maybe Source -> NameM m Source
genTagConstructor typeName tagName tagNum pat =
  do body <- genBody pat
     return $ FnDefS (PatS (genMkTagName tagName) Nothing) Nothing body []
  where
    genCondBody Nothing =
      do condResult <- genCondResult . IdS $ genIsTypeName typeName
         return $ condResult `AppS`
           Source.listToApp [IdS mkVariant0Name, Source.stringS (Name.nameStr typeName), Source.intS tagNum]
    genCondBody (Just binder) =
      do condResult <- genCondResult . IdS $ genIsTypeName typeName
         return $ condResult `AppS`
           Source.listToApp [IdS mkVariantName, Source.stringS (Name.nameStr typeName), Source.intS tagNum, binder]

    genBody Nothing =
      genCondBody Nothing
    genBody (Just src@(PatS binder _)) =
      do condBody <- genCondBody . Just $ IdS binder
         return $ CondS [([src], condBody)]
    genBody src =
      do argName <- NameM.genNameM $ Name.untyped "arg"
         genBody . Just $ PatS argName src

-- | Generates a deconstructor for a variant tag.
genTagDeconstructor :: Monad m => Name -> Name -> Source -> NameM m Source
genTagDeconstructor typeName tagName pat =
  do body <- genBody
     return $ FnDefS (PatS (genUnTagName tagName) Nothing) Nothing body []
  where
    genCondBody argName =
      do condResult <- genCondResult pat
         return $ condResult `AppS` (IdS unVariantName `AppS` IdS argName)

    genBody =
      do argName <- NameM.genNameM $ Name.untyped "arg"
         condBody <- genCondBody argName
         return $ CondS [([PatS argName (Just . IdS $ genIsTypeName typeName)], condBody)]
