{-# LANGUAGE LambdaCase, ParallelListComp, TupleSections #-}
module Stage.Expander where

import Prelude hiding (mod, pred)

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))
import Control.Monad.Error.Class (throwError)

import Data.Expr (DefnKw(..), Expr(..))
import qualified Data.Expr as Expr
import Data.Name (Name)
import qualified Data.Name as Name
import Data.PrettyString (PrettyString)
import qualified Data.PrettyString as PrettyString
import Data.Source (Source(..))
import qualified Data.Source as Source
import qualified Expander.Variant as Variant
import qualified Expander.Type as Type
import Monad.NameM (NameM)
import qualified Monad.NameM as NameM
import qualified Monad.Utils as Utils (returnOne)
import qualified Pretty.Data.Source as Pretty
import qualified Pretty.Stage.Expander as Pretty
import Typechecker.Type (Type)

type ExpanderM a = NameM (Either PrettyString) a

-- | Generates names for the given patterns.
-- @
-- genPatName x       = "x#0"
-- genPatName x@isInt = "x#0"
-- genPatName  @      = "arg#0"
-- genPatName  @isInt = "arg#0"
-- @
genPatNames :: [Source] -> ExpanderM [Name]
genPatNames = mapM genPatName
  where
    genName name
      | Name.isEmptyName name = NameM.genNameM $ Name.untyped "arg"
      | otherwise = NameM.genNameM name

    genPatName (PatS binder _) = genName binder
    genPatName _ = genName Name.empty

-- Expand patterns.

-- | Generates a function definition for a pattern.
--
-- binder: name of the pattern, used as the definition name.
-- mods: folded into applications to form the definition body.
-- val: applied at the end of the applications that form the definition body.
--
-- @
-- patFnDef "x" [hd, tl] y
-- @
--
-- @
-- let x = hd (tl y)
-- @
patFnDef :: Name -> [Source] -> Source -> Source
patFnDef binder mods val =
  FnDefS (PatS binder Nothing) Nothing (Source.foldAppS val mods) []

-- | Generates function definitions for a pattern's bindings
--
-- val: name of the outermost pattern to which inner transformations apply,
-- which is necessary because not all patterns have a name.
--
-- @
-- xs@[x, y]
-- @
--
-- @
-- let xs = x1#
-- let x = hd x1#
-- let y = hd (tl x1#)
-- @
patDefinitions :: Source -> Source -> [Source]
patDefinitions val = sourceDefns []
  where
    hdName = Source.idS "hd"
    tlName = Source.idS "tl"

    tupleRefName len index = Source.idS $ "tuple" ++ show len ++ "Ref" ++ show index

    sourceDefns mods src
      | Source.isTypePat src =
        let (IdS typeName:srcs) = Source.appToList src in
        concatMap (sourceDefns (IdS (Variant.genUnTagName typeName):mods)) srcs
    sourceDefns mods (BinOpS "+>" hdPat tlPat) =
      sourceDefns (hdName:mods) hdPat ++
      sourceDefns (tlName:mods) tlPat
    sourceDefns mods (PatS binder guard) =
      hdDefn ++ tlDefn
      where
        hdDefn
          | Name.isEmptyName binder = []
          | otherwise = (:[]) $ patFnDef binder mods val

        tlDefn =
          case guard of
            Nothing -> []
            Just src -> sourceDefns mods src
    sourceDefns mods (SeqS pats) =
      concat [ sourceDefns (mod ++ mods) pat | mod <- patMods | pat <- pats ]
      where
        listRef 1 = [hdName]
        listRef i = tlName:listRef (i - 1)

        patMods = map (reverse . listRef) [1..length pats]
    sourceDefns mods (TupleS [pat]) =
      sourceDefns mods pat
    sourceDefns mods (TupleS pats) =
      concat [ sourceDefns (patMod index:mods) pat | index <- [0..length pats - 1] | pat <- pats ]
      where
        patMod index = tupleRefName (length pats) index
    sourceDefns _ _ = []

-- | Returns the predicate and equality function names for primitive types.
patConstantPred :: Source -> (Name, Name)
patConstantPred CharS {} = (Name.untyped "isChar#", Name.untyped "eqChar#")
patConstantPred IntS {} = (Name.untyped "isInt#", Name.untyped "eqInt#")
patConstantPred RealS {} = (Name.untyped "isReal#", Name.untyped "eqReal#")
patConstantPred src =
  PrettyString.error "Expander.patConstantPred: invalid argument" $
  (PrettyString.text "src =" PrettyString.<+> Pretty.docSource src)

-- | Generates a predicate for a pattern, according the predicate guard.
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
-- isInt && eqInt 1
-- isList [isChar, isChar, isChar]
-- isTuple ()
-- isTuple (isInt, isReal)
-- @
patPred :: Source -> ExpanderM Expr
patPred = sourcePred
  where
    isHeadTailName = Name.untyped "isHeadTail"
    isListName = Name.untyped "isList"

    isTupleName :: Int -> Name
    isTupleName 1 = error "isTupleName undefined for length 1"
    isTupleName len = Name.untyped $ "isTuple" ++ show len

    variantConsPred [IdS typeName] =
      AppE (IdE $ Variant.genIsTagName typeName) <$> sourcePred (TupleS [])
    variantConsPred (IdS typeName:srcs) =
      Expr.foldAppE (IdE $ Variant.genIsTagName typeName) <$> mapM sourcePred srcs
    variantConsPred srcs =
      throwError . Pretty.devTypePat . Pretty.docSource $ Source.listToApp srcs

    sourcePred :: Source -> ExpanderM Expr
    sourcePred src
      | Source.isTypePat src =
        variantConsPred (Source.appToList src)
    sourcePred src@AppS {} =
      case Source.toSource src of
        Just x -> expandOne x
        _ -> throwError . Pretty.devSourceApp $ Pretty.docSource src
    sourcePred (BinOpS "+>" hdPat tlPat) =
      do hdPred <- sourcePred hdPat
         tlPred <- sourcePred tlPat
         return $ Expr.appE isHeadTailName hdPred `AppE` tlPred
    sourcePred pred@IdS {} =
      expandOne pred
    sourcePred (PatS _ Nothing) =
      return Expr.constTrueE
    sourcePred (PatS _ (Just src)) =
      sourcePred src
    sourcePred (SeqS pats) =
      Expr.appE isListName . Expr.seqE <$> mapM sourcePred pats
    sourcePred (TupleS []) =
      return . IdE $ isTupleName 0
    sourcePred (TupleS [pat]) =
      sourcePred pat
    sourcePred (TupleS pats) =
      Expr.appE (isTupleName $ length pats) . Expr.tupleE <$> mapM sourcePred pats
    sourcePred (StringS cs) =
      sourcePred . SeqS . map CharS $ cs
    sourcePred m =
      do let (isFn, eqFn) = patConstantPred m
         arg <- NameM.genNameM $ Name.untyped "arg"
         let argId = IdS arg
             pred = AndS (Source.appS isFn argId) ((Source.appS eqFn m) `AppS` argId)
         LambdaE arg <$> expandOne pred

-- Expand conds.

-- | Generates an expression by expanding patterns to their respective definitions.
--
-- @
-- x y = body
-- @
--
-- @
-- let x = ... in
-- let y = ... in
-- body
-- @
expandMatchBody :: [Name] -> [Source] -> Source -> ExpanderM Expr
expandMatchBody args pats body =
  do defns <- concat <$> (mapM expandSource $ concat [ patDefinitions (IdS arg) pat | arg <- args | pat <- pats ])
     case defns of
       [] -> expandOne body
       _ -> Expr.letE defns <$> expandOne body

-- | Expands a match of a cond, returning the expanded body and patterns.
--
-- @
-- x@pat1 y@pat1' -> body1
-- x@pat2 y@pat2' -> body2
-- @
--
-- @
-- cond
--  pred1 x# && pred2 y# ->
--    let x = ... in
--    let y = ... in
--    body1
--  pred1 x# && pred2 y# ->
--    let x = ... in
--    let y = ... in
--    body2
--  _ _ -> blame "..."
-- @
--
-- The above code is a simplification because '&&' is encoded through 'CondE'.
expandMatch :: [Name] -> [Source] -> Source -> ExpanderM (Expr, Expr)
expandMatch args pats body =
  (,) <$> andPred <*> expandMatchBody args pats body
  where
    applyPred arg pat =
      do pred <- patPred pat
         return . AppE pred $ IdE arg

    andPred =
      foldl1 Expr.andE <$> sequence [ applyPred arg pat | arg <- args | pat <- pats ]

expandMatches :: [Name] -> [([Source], Source)] -> ExpanderM [(Expr, Expr)]
expandMatches names = loop
  where
    loop [] = return []
    loop ((pats, body):ms) =
      (:) <$> expandMatch names pats body <*> loop ms

-- | Expands a cond, returning the expanded lambdas and cond.
-- Example:
-- @
-- x@pat1 y@pat1' -> body1
-- x@pat2 y@pat2' -> body2
-- @
-- expands into:
-- @
-- \x# ->
--   \y# ->
--     cond
--       pred1 x# && pred2 y# ->
--         let x = ... in
--         let y = ... in
--         body1
--       pred1 x# && pred2 y# ->
--         let x = ... in
--         let y = ... in
--         body2
--       _ _ -> blame "..."
-- @
--
-- The above code is a simplification because '&&' is encoded through 'CondE'.
expandCond :: Source -> Name -> ExpanderM Expr
expandCond (CondS matches) blame =
  do let args = head . fst . unzip $ matches
     case checkMatches (length args) matches of
       Left err ->
         throwError err
       Right () ->
         do argNames <- genPatNames args
            matches' <- expandMatches argNames matches
            return . lambdas argNames . CondE matches' $ Name.nameStr blame
  where
    checkMatches _ [] = Right ()
    checkMatches n ((args, _):matches')
      | length args == n = checkMatches n matches'
      | otherwise = Left . Pretty.condMatchPatternsMismatch . Pretty.docCond $ matches

    lambdas [] body = body
    lambdas (arg:args) body =
      LambdaE arg (lambdas args body)
expandCond src _ =
  PrettyString.error "Expander.expandCond: invalid argument" $
  PrettyString.text "src =" PrettyString.<+> Pretty.docSource src

-- Expand function definitions.

-- | Builds a function definition expression. It finds whether the function is
-- recursive or not, and handles also optional type annotations.
expandFnDecl :: Name -> Maybe Type -> Expr -> Expr
expandFnDecl name typ body =
  let
    kw | name `elem` Expr.freeVars body = Def
       | otherwise = NrDef
  in
    case typ of
      Nothing -> FnDecl kw name body
      Just x -> FnDecl kw name (AnnotationE body x)

-- | Expands a pattern on the left side of a '=' sign.
-- Example:
-- @
-- [x, y] = [1, 2]
-- @
-- expands into:
-- @
-- res#0 = [1, 2]
-- x = hd res#0
-- y = hd (tl res#0)
-- @
expandResultPattern :: Source -> Maybe Type -> Source -> [Source] -> ExpanderM [Source]
expandResultPattern pat typ body whereClause =
  do result <- NameM.genNameM $ Name.untyped "res"
     return $ FnDefS (PatS result Nothing) typ body whereClause:patDefinitions (IdS result) pat

-- | Expands where clauses into 'LetS' and applies them in each match of a
-- 'CondS'.
-- Example:
-- @
-- let x = y
-- where
--   let y = 0
-- @
-- expand into:
-- @
-- let x =
--   let y = 0 in
--   y
-- @
expandWhereClause :: Source -> [Source] -> Source
expandWhereClause src [] =
  src
expandWhereClause (CondS matches) defns =
  CondS $ map (id *** (LetS defns)) matches
expandWhereClause src defns =
  LetS defns src

-- | Expands 'FnDefS'. Expands function definitions together with
-- 'CondS' to get the right blame for incomplete pattern matching.
expandFunctionDefinition :: Source -> ExpanderM [Expr]
expandFunctionDefinition (FnDefS (PatS name _) typ body@CondS {} whereClause) =
  Utils.returnOne $ expandFnDecl name typ <$> expandCond (expandWhereClause body whereClause) name
expandFunctionDefinition (FnDefS (PatS name _) typ body whereClause) =
  Utils.returnOne $ expandFnDecl name typ <$> expandOne (expandWhereClause body whereClause)
expandFunctionDefinition (FnDefS pat typ body whereClause) =
  concat <$> (mapM expandFunctionDefinition =<< expandResultPattern pat typ body whereClause)
expandFunctionDefinition src =
  PrettyString.error "Expander.expandFunctionDefinition: invalid argument" $
  PrettyString.text "src =" PrettyString.<+> Pretty.docSource src

-- Expand sequences.

expandSeq :: [Source] -> ExpanderM Expr
expandSeq ms = Expr.seqE <$> mapM expandOne ms

-- Expand tuples.

-- | Expands a tuple into the corresponding size-dependent functions
-- (e.g., mkTuple2, mkTuple3, etc).
--
-- Example:
-- @
-- ()
-- (1, 2.0)
-- (1, 2.0, "hello")
-- ...
-- @
-- expands to:
-- @
-- mkTuple0
-- mkTuple2 1 2.0
-- mkTuple3 1 2.0 "hello"
-- ...
-- @
expandTuple :: [Source] -> ExpanderM Expr
expandTuple [src] =
  expandOne src
expandTuple srcs =
  do let mkTupleName = "mkTuple" ++ show (length srcs)
     Expr.foldAppE (Expr.idE mkTupleName) <$> mapM expandOne srcs

-- Expand type definitions.

expandTypeDecl :: Name -> [(Name, Source)] -> ExpanderM [Expr]
expandTypeDecl typeName tags =
  do let srcs = typePredicates ++ tagPredicates ++ tagConstructors ++ tagDeconstructors
     exprs <- concat <$> mapM expandSource srcs
     return exprs
  where
    typePredicates =
      [ Type.genTypePredicate typeName ]

    tagPredicates =
      [ Variant.genTagPredicate typeName tagName tagNum |
        (tagName, _) <- tags |
        tagNum <- [0..] ]

    tagConstructors =
      [ Variant.genTagConstructor typeName tagName tagNum pat |
        (tagName, pat) <- tags |
        tagNum <- [0..] ]

    tagDeconstructors =
      [ Variant.genTagDeconstructor typeName tagName pat |
        (tagName, pat) <- tags ]

-- Expand source.

expandOne :: Source -> ExpanderM Expr
expandOne src = head <$> expandSource src

expandSource :: Source -> ExpanderM [Expr]
expandSource (AndS m1 m2) =
  Utils.returnOne $ Expr.andE <$> expandOne m1 <*> expandOne m2
expandSource (AppS m1 m2) =
  Utils.returnOne $ AppE <$> expandOne m1 <*> expandOne m2
expandSource (BinOpS op m1 m2) =
  Utils.returnOne $ Expr.binOpE (Name.untyped op) <$> expandOne m1 <*> expandOne m2
expandSource (CharS c) =
  Utils.returnOne . return $ Expr.charE c
expandSource src@CondS {} =
  Utils.returnOne . expandCond src $ Name.untyped "lambda"
expandSource src@FnDefS {} =
  expandFunctionDefinition src
expandSource (IdS name) =
  Utils.returnOne . return . Expr.IdE $ name
expandSource (IntS n) =
  Utils.returnOne . return . Expr.intE $ n
expandSource (LetS defns src) =
  do defns' <- concat <$> mapM expandSource defns
     Utils.returnOne $ Expr.letE defns' <$> expandOne src
expandSource (ModuleS me _ _) =
  throwError (Pretty.devModuleNested (Name.nameStr me))
expandSource (OrS m1 m2) =
  Utils.returnOne $ Expr.orE <$> expandOne m1 <*> expandOne m2
expandSource pat@PatS {} =
  throwError . Pretty.devPattern . Pretty.docSource $ pat
expandSource (RealS n) =
  Utils.returnOne . return . Expr.realE $ n
expandSource (SeqS ms) =
  Utils.returnOne $ expandSeq ms
expandSource (StringS str) =
  Utils.returnOne . return . Expr.stringE $ str
expandSource (TupleS srcs) =
  Utils.returnOne $ expandTuple srcs
expandSource (TypeDeclS typeName tags) =
  expandTypeDecl typeName tags

expand :: Source -> Either PrettyString [Expr]
expand src = fst <$> NameM.runNameM (expandSource src) NameM.initialNameState
