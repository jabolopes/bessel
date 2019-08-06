{-# LANGUAGE LambdaCase, ParallelListComp, TupleSections #-}
module Stage.Expander where

import Prelude hiding (mod, pred)

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))
import Control.Monad.Error.Class (throwError)
import qualified Data.Maybe as Maybe

import Data.Literal (Literal(..))
import Data.Expr (DefnKw(..), Expr(..))
import qualified Data.Expr as Expr
import Data.Name (Name)
import qualified Data.Name as Name
import Data.PrettyString (PrettyString)
import qualified Data.PrettyString as PrettyString
import Data.Source (Source(..))
import qualified Data.Source as Source
import qualified Expander.Pattern as Pattern
import qualified Expander.Variant as Variant
import qualified Expander.Type as Type
import Monad.NameM (NameM)
import qualified Monad.NameM as NameM
import qualified Monad.Utils as Utils (returnOne)
import qualified Pretty.Data.Source as Pretty
import qualified Pretty.Stage.Expander as Pretty
import Typechecker.Type (Type)

type ExpanderM a = NameM (Either PrettyString) a

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
    genTagPredicate [IdS typeName] =
      AppE (IdE $ Variant.genIsTagName typeName) <$> sourcePred (TupleS [])
    genTagPredicate (IdS typeName:srcs) =
      Expr.foldAppE (IdE $ Variant.genIsTagName typeName) <$> mapM sourcePred srcs
    genTagPredicate srcs =
      throwError . Pretty.devTypePat . Pretty.docSource $ Source.listToApp srcs

    genBinOpPredicate "+>" pat1 pat2 =
      do pred1 <- sourcePred pat1
         pred2 <- sourcePred pat2
         return $ Expr.appE Pattern.isHeadTailName pred1 `AppE` pred2
    genBinOpPredicate op pat1 pat2 =
      throwError . Pretty.devUnhandled "Stage.Expander.patPred.genBinOpPredicate" . Pretty.docSource $
        BinOpS op pat1 pat2

    genLiteralPredicate literal =
      do let (isFn, eqFn) = Pattern.genLiteralPredicate literal
         arg <- NameM.genNameM $ Name.untyped "arg"
         let argId = IdS arg
             pred = AndS (Source.appS isFn argId) ((Source.appS eqFn (LiteralS literal)) `AppS` argId)
         LambdaE arg <$> expandOne pred

    genPatPredicate Nothing =
      return Expr.constTrueE
    genPatPredicate (Just src) =
      sourcePred src

    genTuplePredicate [] =
      return . IdE $ Pattern.isTupleName 0
    genTuplePredicate [pat] =
      sourcePred pat
    genTuplePredicate pats =
      Expr.appE (Pattern.isTupleName $ length pats) . Expr.tupleE <$> mapM sourcePred pats

    sourcePred :: Source -> ExpanderM Expr
    sourcePred src
      | Source.isTypePat src = genTagPredicate (Source.appToList src)
    sourcePred src@AppS {} =
      case Source.toSource src of
        Just x -> expandOne x
        _ -> throwError . Pretty.devSourceApp $ Pretty.docSource src
    sourcePred (BinOpS op pat1 pat2) =
      genBinOpPredicate op pat1 pat2
    sourcePred pred@IdS {} =
      expandOne pred
    sourcePred (LiteralS literal) =
      genLiteralPredicate literal
    sourcePred (PatS _ pat) =
      genPatPredicate pat
    sourcePred (SeqS pats) =
      Expr.appE Pattern.isListName . Expr.seqE <$> mapM sourcePred pats
    sourcePred (TupleS pats) =
      genTuplePredicate pats
    sourcePred src =
      throwError . Pretty.devUnhandled "Stage.Expander.patPred.sourcePred" $ Pretty.docSource src

-- Expand conds.

data MatchPredicate
  -- | Trivial match predicate.
  -- @
  -- x y = ...
  -- @
  -- is equivalent to:
  -- @
  -- (\_ -> true#) x && (\_ -> true#) y
  -- @
  = Trivial
  -- | Non-trivial match predicate.
  -- @
  -- x@pat1 y@pat2 = ...
  -- @
  -- expands into:
  -- @
  -- pred1 x && pred2 y
  -- @
  --
  -- The above code is a simplification because '&&' is encoded through 'CondE'.
  | NonTrivial Expr

genMatchPredicate :: Source -> Name -> ExpanderM MatchPredicate
genMatchPredicate (PatS _ Nothing) _ =
  return Trivial
genMatchPredicate pat arg =
  do pred <- expandOne =<< Pattern.genPatternPredicate pat
     return . NonTrivial $ pred `AppE` IdE arg

-- | Same as 'genMatchPredicate' but predicates for multiple arguments
-- are folded by '&&'.
genMatchPredicates :: [Source] -> [Name] -> ExpanderM MatchPredicate
genMatchPredicates pats args =
  catNonTrivials <$> sequence [ genMatchPredicate pat arg | pat <- pats | arg <- args ] >>= \case
    [] -> return Trivial
    exprs -> return . NonTrivial $ foldl1 Expr.andE exprs
  where
    catNonTrivials [] = []
    catNonTrivials (Trivial:xs) = catNonTrivials xs
    catNonTrivials (NonTrivial expr:xs) = expr:catNonTrivials xs

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
  do defns <- concat <$> (mapM expandSource $ concat [ Pattern.genPatternGetters (IdS arg) pat | arg <- args | pat <- pats ])
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
-- @
--
-- The above code is a simplification because '&&' is encoded through 'CondE'.
expandMatch :: [Name] -> ([Source], Source) -> ExpanderM (MatchPredicate, Expr)
expandMatch args (pats, body) =
  do predicates <- genMatchPredicates pats args
     body' <- expandMatchBody args pats body
     return (predicates, body')

expandMatches :: [Name] -> [([Source], Source)] -> ExpanderM Expr
expandMatches args [match] =
  expandMatch args match >>= \case
    (Trivial, body) -> return body
    (NonTrivial pred, body) -> return $ CondE [(pred, body)]
expandMatches args matches =
  CondE . fillTrivialPredicates <$> mapM (expandMatch args) matches
  where
    fillTrivialPredicates [] =
      []
    fillTrivialPredicates ((Trivial, body):ms) =
      (Expr.trueE, body):fillTrivialPredicates ms
    fillTrivialPredicates ((NonTrivial pred, body):ms) =
      (pred, body):fillTrivialPredicates ms

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
-- @
--
-- The above code is a simplification because '&&' is encoded through 'CondE'.
expandCond :: [([Source], Source)] -> ExpanderM Expr
expandCond matches =
  do let args = head . fst . unzip $ matches
     () <- checkMatches (length args) matches
     argNames <- Pattern.genPatternNames args
     matches' <- expandMatches argNames matches
     return $ lambdas argNames matches'
  where
    checkMatches :: Int -> [([Source], Source)] -> ExpanderM ()
    checkMatches _ [] = return ()
    checkMatches n ((args, _):matches')
      | length args == n = checkMatches n matches'
      | otherwise = throwError . Pretty.condMatchPatternsMismatch . Pretty.docCond $ matches

    lambdas [] body = body
    lambdas (arg:args) body =
      LambdaE arg (lambdas args body)

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
  do resultName <- Pattern.genPatternName "res" pat
     predicate <- Pattern.genPatternPredicate pat
     let body' = Source.listToApp [Source.idS "check#", predicate, body]
     return $ FnDefS (PatS resultName Nothing) typ body' whereClause:Pattern.genPatternGetters (IdS resultName) pat

-- | Expands where clauses.
--
-- Example without cond (or with cond with more than 1 match):
-- @
-- let f
--   x@isInt = ...
--   x@isReal = ...
-- where
--   let g = ...
-- @
-- expands into:
-- @
-- let f =
--   let g = ... in
--   \x -> ...
-- @
--
-- Example with cond with single pattern match.
-- @
-- let f x@ = ...
-- where
--   let g = ...
-- @
-- expands into:
-- @
-- let f =
--   \x ->
--     let g = ... in
--     ...
-- @
expandWhere :: Source -> [Source] -> Source
expandWhere src [] = src
expandWhere (CondS [match]) defns = CondS $ map (id *** (LetS defns)) [match]
expandWhere src defns = LetS defns src

-- | Expands 'FnDefS'. Expands function definitions together with
-- 'CondS' to get the right blame for incomplete pattern matching.
expandFunctionDefinition :: Source -> ExpanderM [Expr]
expandFunctionDefinition (FnDefS pat@(PatS _ Nothing) typ body whereClause) =
  do fnName <- Pattern.genPatternName "fn" pat
     let body' = expandWhere body whereClause
     Utils.returnOne $ expandFnDecl fnName typ <$> expandOne body'
expandFunctionDefinition (FnDefS pat typ body whereClause) =
  concat <$> (mapM expandFunctionDefinition =<< expandResultPattern pat typ body whereClause)
expandFunctionDefinition src =
  PrettyString.error "Expander.expandFunctionDefinition: invalid argument" $
  PrettyString.text "src =" PrettyString.<+> Pretty.docSource src

-- Expand literals.

expandLiteral :: Literal -> Expr
expandLiteral (CharL c) = Expr.charE c
expandLiteral (IntL n) = Expr.intE n
expandLiteral (RealL d) = Expr.realE d
expandLiteral (StringL str) = Expr.stringE str

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

expandTypeDecl :: Name -> [(Name, Maybe Source)] -> ExpanderM [Expr]
expandTypeDecl typeName tags =
  do srcs <- sequence $ typePredicates ++ tagPredicates ++ tagConstructors ++ tagDeconstructors
     concat <$> mapM expandSource srcs
  where
    typePredicates =
      [ return $ Type.genTypePredicate typeName ]

    tagPredicates =
      [ Variant.genTagPredicate typeName tagName tagNum pat |
        (tagName, pat) <- tags |
        tagNum <- [0..] ]

    tagConstructors =
      [ Variant.genTagConstructor typeName tagName tagNum pat |
        (tagName, pat) <- tags |
        tagNum <- [0..] ]

    tagDeconstructors =
      [ Variant.genTagDeconstructor typeName tagName (Maybe.fromJust pat) |
        (tagName, pat) <- tags, Maybe.isJust pat ]

-- Expand source.

expandOne :: Source -> ExpanderM Expr
expandOne src =
  do exprs <- expandSource src
     case exprs of
       [expr] ->
         return expr
       _ ->
         throwError $ PrettyString.text "Stage.Expander.expandOne: expected single expression"

expandSource :: Source -> ExpanderM [Expr]
expandSource (AndS src1 src2) =
  Utils.returnOne $ Expr.andE <$> expandOne src1 <*> expandOne src2
expandSource (AppS src1 src2) =
  Utils.returnOne $ AppE <$> expandOne src1 <*> expandOne src2
expandSource (BinOpS op src1 src2) =
  Utils.returnOne $ Expr.binOpE (Name.untyped op) <$> expandOne src1 <*> expandOne src2
expandSource (CondS matches) =
  Utils.returnOne $ expandCond matches
expandSource src@FnDefS {} =
  expandFunctionDefinition src
expandSource (IdS name) =
  Utils.returnOne . return . Expr.IdE $ name
expandSource (LetS defns src) =
  do defns' <- concat <$> mapM expandSource defns
     Utils.returnOne $ Expr.letE defns' <$> expandOne src
expandSource (LiteralS literal) =
  Utils.returnOne . return $ expandLiteral literal
expandSource (ModuleS me _ _) =
  throwError (Pretty.devModuleNested (Name.nameStr me))
expandSource (OrS src1 src2) =
  Utils.returnOne $ Expr.orE <$> expandOne src1 <*> expandOne src2
expandSource pat@PatS {} =
  throwError . Pretty.devPattern . Pretty.docSource $ pat
expandSource (SeqS ms) =
  Utils.returnOne $ expandSeq ms
expandSource (TupleS srcs) =
  Utils.returnOne $ expandTuple srcs
expandSource (TypeDeclS typeName tags) =
  expandTypeDecl typeName tags

expand :: Source -> Either PrettyString [Expr]
expand src = fst <$> NameM.runNameM (expandSource src) NameM.initialNameState
