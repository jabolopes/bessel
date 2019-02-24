{-# LANGUAGE FlexibleContexts, ParallelListComp #-}
module Expander.Pattern where

import Prelude hiding (pred, mod)

import Control.Monad.Except (MonadError, throwError)

import qualified Data.List as List

import Data.Literal (Literal(..))
import Data.Name (Name)
import qualified Data.Name as Name
import Data.PrettyString (PrettyString)
import Data.Source (Source(..))
import qualified Data.Source as Source
import qualified Expander.Variant as Variant
import Monad.NameM (NameM)
import qualified Monad.NameM as NameM
import qualified Pretty.Data.Source as Pretty
import qualified Pretty.Stage.Expander as Pretty

-- Runtime.

headName :: Name
headName = Name.untyped "head#"

tailName :: Name
tailName = Name.untyped "tail#"

tupleRefName :: Int -> Int -> Name
tupleRefName len index = Name.untyped $ "tuple" ++ show len ++ "Ref" ++ show index ++ "#"

isHeadTailName :: Name
isHeadTailName = Name.untyped "isHeadTail#"

isListName :: Name
isListName = Name.untyped "isList#"

isTupleName :: Int -> Name
isTupleName 1 = error "isTupleName undefined for length 1"
isTupleName len = Name.untyped $ "isTuple" ++ show len

-- Pattern names.

genPatternName :: Monad m => String -> Source -> NameM m Name
genPatternName _ (PatS binder Nothing)
  | Name.isEmptyName binder = return $ Name.untyped "_"
genPatternName _ (PatS binder _) = return binder
genPatternName hint _ = NameM.genNameM $ Name.untyped hint

-- | Generates names for the given patterns.
-- @
-- genPatNames x       = "x"
-- genPatNames x@isInt = "x"
-- genPatNames  @      = "_"
-- genPatNames  @isInt = "arg#0"
-- @
genPatternNames :: Monad m => [Source] -> NameM m [Name]
genPatternNames = mapM (genPatternName "arg")

-- Pattern definitions.

-- | Generates the function that gets the value of a single pattern.
--
-- binder: name of the pattern, used as the definition name.
-- mods: folded into applications to form the function body.
-- val: applied at the end of the applications that form the definition body.
--
-- @
-- genPatternGetter "x" ["hd#", "tl#"] y
-- @
--
-- @
-- let x = hd# (tl# y)
-- @
genPatternGetter :: Name -> [Source] -> Source -> Source
genPatternGetter binder mods val =
  FnDefS (PatS binder Nothing) Nothing (foldr AppS val mods) []

-- | Generates all functions that get all the value of a complex pattern.
--
-- val: name of the outermost pattern to which inner transformations apply,
-- which is necessary because not all patterns have a name.
--
-- Example (list):
-- @
-- xs@[x, y@isInt]
-- @
-- generates:
-- @
-- let x = head# xs
-- let y = head# (tail# xs)
-- @
--
-- Example (tuple):
-- @
-- xs@(x, y@isInt)
-- @
-- generates:
-- @
-- let x = tuple2Ref0 xs
-- let y = tuple2Ref1 xs
-- @
genPatternGetters :: Source -> Source -> [Source]
genPatternGetters val = genGetters []
  where
    genVariantGetter mods src =
      let (IdS typeName:srcs) = Source.appToList src in
      concatMap (genGetters (IdS (Variant.genUnTagName typeName):mods)) srcs

    genPatGetter mods binder guard =
      binderGetter ++ guardGetter guard
      where
        binderGetter
          | Name.isEmptyName binder = []
          | List.null mods = []
          | otherwise = (:[]) $ genPatternGetter binder mods val

        guardGetter Nothing = []
        guardGetter (Just pat) = genGetters mods pat

    genListGetters mods pats =
      concat [ genGetters (mod ++ mods) pat
             | mod <- patMods
             | pat <- pats ]
      where
        listRef 1 = [IdS headName]
        listRef i = IdS tailName:listRef (i - 1)

        patMods = map (reverse . listRef) [1..length pats]

    genTupleGetters _ [] = []
    genTupleGetters mods [src] =
      genGetters mods src
    genTupleGetters mods srcs =
      concat [ genGetters (patMod index:mods) src
             | index <- [0..]
             | src <- srcs ]
      where
        patMod index = IdS $ tupleRefName (length srcs) index

    genGetters mods src
      | Source.isTypePat src = genVariantGetter mods src
    genGetters mods (BinOpS "+>" pat1 pat2) =
      genGetters (IdS headName:mods) pat1 ++ genGetters (IdS tailName:mods) pat2
    genGetters mods (PatS binder guard) =
      genPatGetter mods binder guard
    genGetters mods (SeqS pats) =
      genListGetters mods pats
    genGetters mods (TupleS pats) =
      genTupleGetters mods pats
    genGetters _ _ = []

-- Pattern predicates.

-- | Returns the predicate and equality function names for literals.
genLiteralPredicate :: Literal -> (Name, Name)
genLiteralPredicate CharL {} = (Name.untyped "isChar#", Name.untyped "eqChar#")
genLiteralPredicate IntL {} = (Name.untyped "isInt#", Name.untyped "eqInt#")
genLiteralPredicate RealL {} = (Name.untyped "isReal#", Name.untyped "eqReal#")
genLiteralPredicate StringL {} = (Name.untyped "isString#", Name.untyped "eqString#")

-- | Generates a predicate for a pattern.
--
-- @
-- x
-- x@isInt
-- (x@ +> xs@)
-- x@[@, @]
-- x@1
-- x@"ola"
-- ()
-- (x@isInt, y@isReal)
-- @
--
-- @
-- const true
-- isInt
-- isHeadTail (const true) (const true)
-- isList [const true, const true]
-- \arg -> isInt arg && eqInt arg 1
-- isString
-- isTuple0
-- isTuple2 (isInt, isReal)
-- @
genPatternPredicate :: MonadError PrettyString m => Source -> NameM m Source
genPatternPredicate = genPredicate
  where
    genTagPredicate [IdS typeName] =
      AppS (IdS $ Variant.genIsTagName typeName) <$> genPredicate (TupleS [])
    genTagPredicate (IdS typeName:srcs) =
      do args <- mapM genPredicate srcs
         return . Source.listToApp $ (IdS $ Variant.genIsTagName typeName):args
    genTagPredicate srcs =
      throwError . Pretty.devTypePat . Pretty.docSource $ Source.listToApp srcs

    genBinOpPredicate "+>" pat1 pat2 =
      do pred1 <- genPredicate pat1
         pred2 <- genPredicate pat2
         return $ Source.listToApp [IdS isHeadTailName, pred1, pred2]
    genBinOpPredicate op pat1 pat2 =
      throwError . Pretty.devUnhandled "Stage.Expander.patPred.genBinOpPredicate" . Pretty.docSource $
        BinOpS op pat1 pat2

    genLiteral literal =
      do let (isFn, eqFn) = genLiteralPredicate literal
         argName <- NameM.genNameM $ Name.untyped "arg"
         return $ CondS [([PatS argName Nothing],
                          (IdS isFn `AppS` IdS argName) `AndS`
                           (Source.listToApp [IdS eqFn, LiteralS literal, IdS argName]))]

    genPatPredicate Nothing =
      return $ CondS [([Source.allPat], Source.idS "true#")]
    genPatPredicate (Just src) =
      genPredicate src

    genListPredicate pats =
      do srcs <- mapM genPredicate pats
         return $ Source.listToApp [IdS isListName, SeqS srcs]

    genTuplePredicate [] =
      return . IdS $ isTupleName 0
    genTuplePredicate [pat] =
      genPredicate pat
    genTuplePredicate pats =
      do srcs <- mapM genPredicate pats
         return $ Source.listToApp [IdS . isTupleName $ length pats, TupleS srcs]

    genPredicate src
      | Source.isTypePat src = genTagPredicate (Source.appToList src)
    genPredicate src@AppS {} =
      case Source.toSource src of
        Just x -> return x
        _ -> throwError . Pretty.devSourceApp $ Pretty.docSource src
    genPredicate (BinOpS op pat1 pat2) =
      genBinOpPredicate op pat1 pat2
    genPredicate pred@IdS {} =
      return pred
    genPredicate (LiteralS literal) =
      genLiteral literal
    genPredicate (PatS _ pat) =
      genPatPredicate pat
    genPredicate (SeqS pats) =
      genListPredicate pats
    genPredicate (TupleS pats) =
      genTuplePredicate pats
    genPredicate src =
      throwError . Pretty.devUnhandled "Expander.Pattern.genPatternPredicate.genPredicate" $ Pretty.docSource src
