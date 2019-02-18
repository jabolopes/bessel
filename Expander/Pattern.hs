{-# LANGUAGE ParallelListComp #-}
module Expander.Pattern where

import Prelude hiding (mod)

import Data.Literal (Literal(..))
import Data.Name (Name)
import qualified Data.Name as Name
import Data.Source (Source(..))
import qualified Data.Source as Source
import qualified Expander.Variant as Variant
import Monad.NameM (NameM)
import qualified Monad.NameM as NameM

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

-- | Generates names for the given patterns.
-- @
-- genPatNames x       = "x#0"
-- genPatNames x@isInt = "x#0"
-- genPatNames  @      = "arg#0"
-- genPatNames  @isInt = "arg#0"
-- @
genPatNames :: Monad m => [Source] -> NameM m [Name]
genPatNames = mapM genPatName
  where
    genName name
      | Name.isEmptyName name = NameM.genNameM $ Name.untyped "arg"
      | otherwise = NameM.genNameM name

    genPatName (PatS binder _) = genName binder
    genPatName _ = genName Name.empty

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
-- xs@[x, y]
-- @
-- generates:
-- @
-- let xs = x1#
-- let x = head# x1#
-- let y = head# (tail# x1#)
-- @
--
-- Example (tuple):
-- @
-- xs@(x, y)
-- @
-- generates:
-- @
-- let xs = x1#
-- let x = tuple2Ref0 x1#
-- let y = tuple2Ref1 x1#
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
