{-# LANGUAGE ParallelListComp, TupleSections #-}
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
import Monad.NameM (NameM)
import qualified Monad.Utils as Utils (returnOne)
import qualified Monad.NameM as NameM
import qualified Pretty.Data.Source as Pretty
import qualified Pretty.Stage.Expander as Pretty

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
  FnDefS (PatS binder Nothing) (Source.foldAppS val mods)

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
    unConsName = Source.idS "unCons#"
    hdName = Source.idS "hd"
    tlName = Source.idS "tl"

    sourceDefns mods src
      | Source.isTypePat src =
        concatMap (sourceDefns (unConsName:mods)) . tail . Source.appToList $ src
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
      concat [ sourceDefns (mod ++ mods) pat | pat <- pats | mod <- patMods ]
      where
        listRef 1 = [hdName]
        listRef i = tlName:listRef (i - 1)
        patMods = map (reverse . listRef) [1..length pats]
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
-- @
--
-- @
-- const true
-- isInt
-- isList (const true) (const true)
-- isTuple [const true, const true]
-- isInt && eqInt 1
-- isTuple [isChar, isChar, isChar]
-- @
patPred :: Source -> ExpanderM Expr
patPred = sourcePred
  where
    isListName = Name.untyped "isList"
    isTupleName = Name.untyped "isTuple"

    sourcePred :: Source -> ExpanderM Expr
    sourcePred src
      | Source.isTypePat src =
        let
          qualName =
            case head (Source.appToList src) of
              IdS x -> x
              PatS x Nothing -> x
        in
          return . IdE . consIsName $ qualName
    sourcePred src@AppS {} =
      case Source.toSource src of
        Just x -> expandOne x
        _ -> throwError . Pretty.devSourceApp $ Pretty.docSource src
    sourcePred (BinOpS "+>" hdPat tlPat) =
      do hdPred <- sourcePred hdPat
         tlPred <- sourcePred tlPat
         return $ Expr.appE isListName hdPred `AppE` tlPred
    sourcePred pred@IdS {} =
      expandOne pred
    sourcePred (PatS _ Nothing) =
      return Expr.constTrueE
    sourcePred (PatS _ (Just src)) =
      patPred src
    sourcePred (SeqS pats) =
      Expr.appE isTupleName . Expr.seqE <$> mapM sourcePred pats
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
--
-- @
-- x@pat1 y@pat1' -> body1
-- x@pat2 y@pat2' -> body2
-- @
--
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
expandCond :: [([Source], Source)] -> Name -> ExpanderM Expr
expandCond matches blame =
  do let args = head . fst . unzip $ matches
     case checkMatches (length args) matches of
       Left err -> throwError err
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

-- Expand function definitions.

expandFnDecl :: Name -> Expr -> Expr
expandFnDecl name body =
  let
    kw | name `elem` Expr.freeVars body = Def
       | otherwise = NrDef
  in
   FnDecl kw name body

expandResultPattern :: Source -> Source -> ExpanderM [Source]
expandResultPattern pat body =
  do result <- NameM.genNameM $ Name.untyped "res"
     return $ FnDefS (PatS result Nothing) body:patDefinitions (IdS result) pat

expandSeq :: [Source] -> ExpanderM Expr
expandSeq ms = Expr.seqE <$> mapM expandOne ms

expandString :: String -> Expr
expandString = Expr.stringE

-- Expand type definitions.

consIsName :: Name -> Name
consIsName = Name.untyped . ("is" ++) . Name.nameStr

consMkName :: Name -> Name
consMkName = id

consPredFn :: Name -> Source
consPredFn consName =
  let
    consNameStr = Name.nameStr consName
    body = Source.appS isConsName . Source.appS linkName . StringS $ consNameStr
  in
    FnDefS (PatS (consIsName consName) Nothing) body
  where
    isConsName = Name.untyped "isCons#"
    linkName = Name.untyped "link#"

typePredFn :: Name -> [Name] -> Source
typePredFn typeName consNames =
  let
    predNames = map (Source.IdS . consIsName) consNames
    body = foldl1 OrS predNames
  in
    FnDefS (PatS (consIsName typeName) Nothing) body

consConsFn :: Name -> Name -> Maybe Source -> Source
consConsFn consName binder guard =
  let
    consNameStr = Name.nameStr consName
    body = Source.appS mkConsName . Source.appS linkName . StringS $ consNameStr
  in
    FnDefS (PatS (consMkName consName) Nothing) $
      CondS [([PatS binder guard], body `AppS` IdS binder)]
  where
    mkConsName = Name.untyped "mkCons#"
    linkName = Name.untyped "link#"

expandConstructor :: (Name, Source) -> ExpanderM [Expr]
expandConstructor (consName, pat) =
  do binder <- patBinder pat
     let guard = patGuard pat
         predSource = consPredFn consName
         consSource = consConsFn consName binder guard
     (++) <$> expandSource predSource <*> expandSource consSource
  where
    patBinder (PatS binder _) | not (Name.isEmptyName binder) = return binder
    patBinder _               = NameM.genNameM $ Name.untyped "arg"

    patGuard (PatS _ Nothing) = Nothing
    patGuard (PatS _ (Just src)) = patGuard src
    patGuard src = Just src

expandTypeDecl :: Name -> [(Name, Source)] -> ExpanderM [Expr]
expandTypeDecl typeName cons =
  do consFns <- concat <$> mapM expandConstructor cons
     typeFn <- expandSource . typePredFn typeName $ map fst cons
     return $ consFns ++ typeFn

-- | Expands 'WhereS' into 'LetS'. It is special syntax to be used only with 'CondS'.
--
-- @
-- let x = y
--   where
--     let y = 0
--
-- let x =
--   let y = 0 in
--   y
-- @
expandWhereCondMatches :: Source -> [([Source], Source)]
expandWhereCondMatches (WhereS (CondS matches) defns) =
  map (id *** (LetS defns)) matches
expandWhereCondMatches src =
  PrettyString.error "Expander.expandWhereCondMatches: invalid argument" $
  (PrettyString.text "src =" PrettyString.<+> Pretty.docSource src)

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
  Utils.returnOne . return . CharE $ c
expandSource (CondS ms) =
  Utils.returnOne . expandCond ms $ Name.untyped "lambda"
-- Expand FnDeclS together with CondS to get the correct blame.
expandSource (FnDefS (PatS name _) (CondS ms)) =
  Utils.returnOne $ expandFnDecl name <$> expandCond ms name
-- Expand FnDeclS together with WhereS to get the correct blame.
expandSource (FnDefS (PatS name _) src@(WhereS CondS {} _)) =
  Utils.returnOne $ expandFnDecl name <$> expandCond (expandWhereCondMatches src) name
expandSource (FnDefS (PatS name _) body) =
  Utils.returnOne $ expandFnDecl name <$> expandOne body
-- TODO: Find a way to get the blame correct in this case.
expandSource (FnDefS pat body) =
  concat <$> (mapM expandSource =<< expandResultPattern pat body)
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
  Utils.returnOne . return . expandString $ str
expandSource (TypeDeclS typeName cons) =
  expandTypeDecl typeName cons
expandSource src@(WhereS CondS {} _) =
  expandSource . CondS . expandWhereCondMatches $ src
expandSource src@WhereS {} =
  throwError . Pretty.whereOutsideCond . Pretty.docSource $ src

expand :: Source -> Either PrettyString [Expr]
expand src = fst <$> NameM.runNameM (expandSource src) NameM.initialNameState
