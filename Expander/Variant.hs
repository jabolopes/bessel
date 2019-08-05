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

genCondResult :: Monad m => Source -> NameM m Source
genCondResult src@(PatS binder _) =
  return $ CondS [([src], IdS binder)]
genCondResult src =
  do resultName <- NameM.genNameM $ Name.untyped "r"
     genCondResult . PatS resultName $ Just src

-- | Generates a predicate for a variant tag.
-- @
-- type Fruit
--   = Apple
--   | Banana @isInt
--   | Fig x@(@isInt, @isReal)
--   | Orange @isFruit
-- @
--
-- @
-- isApple fn arg@isFruit  = isFruit arg && isVariant "Fruit" 0 fn arg
-- isBanana fn arg@isFruit = isFruit arg && isVariant "Fruit" 1 fn arg
-- isFig fn arg@isFruit    = isFruit arg && isVariant "Fruit" 2 fn arg
-- isOrange fn arg@isFruit = isFruit arg && isVariant "Fruit" 3 fn arg
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
--   | Orange @isFruit
-- @
--
-- @
-- mkApple                   = (r@isFruit = r) $ mkVariant "Fruit" 0 ()
-- mkBanana arg@isInt        = (r@isFruit = r) $ mkVariant "Fruit" 1 arg
-- mkFig x@(@isInt, @isReal) = (r@isFruit = r) $ mkVariant "Fruit" 2 x
-- mkOrange isFruit          = (r@isFruit = r) $ mkVariant "Fruit" 3 arg
-- @
genTagConstructor :: Monad m => Name -> Name -> Int -> Source -> NameM m Source
genTagConstructor typeName tagName tagNum pat =
  do body <- genBody pat
     return $ FnDefS (PatS (genMkTagName tagName) Nothing) Nothing body []
  where
    genCondBody binder =
      do condResult <- genCondResult . IdS $ genIsTypeName typeName
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
--   | Fig x@(@isInt, @isReal)
--   | Orange @isFruit
-- @
--
-- @
-- unApple arg@isFruit  = (r@() = r) $ unVariant arg
-- unBanana arg@isFruit = (r@isInt = r) $ unVariant arg
-- unFig arg@isFruit    = (x@(@isInt, @isReal) = x) $ unVariant arg
-- unOrange arg@isFruit = (r@isFruit = x) $ unVariant arg
-- @
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
