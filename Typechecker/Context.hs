{-# LANGUAGE NamedFieldPuns #-}
module Typechecker.Context where

import Prelude hiding (last, lookup)

import qualified Data.List as List
import qualified Data.Maybe as Maybe

import Data.Expr
import Data.Name (Name)
import qualified Data.Name as Name
import Typechecker.Type (Type(..))
import qualified Typechecker.Type as Type
import Typechecker.TypeName (TypeName)

data ContextVar
  = ContextType Type
  | ContextTermVar Name
  | ContextMarker TypeName
    deriving (Eq)

instance Show ContextVar where
  show (ContextType typ) = show typ
  show (ContextTermVar x) = show x
  show (ContextMarker x) = "<|" ++ show x

assigned :: Name -> Type -> (ContextVar, Maybe Type)
assigned binding typ = (ContextTermVar binding, Just typ)

-- | Context is implemented as a reversed list.
data Context = Context {
  -- | Scope implemented as a stack on a reverse list.
  scope :: [(ContextVar, Maybe Type)],
  -- | Term variables accumulated when the context is sliced (i.e.,
  -- names discarded when they go out of scope) to be used later for
  -- full substitution.
  vars :: [(ContextVar, Maybe Type)] }
  deriving (Show)

-- |
lookupContext :: Context -> ContextVar -> Maybe (ContextVar, Maybe Type)
lookupContext Context { scope } var =
  List.find ((==) var . fst) scope

-- | Looks up the type of 'var' in the context. The type is returned
-- as it appears on the scope, without further substitutions.
lookupType :: Context -> ContextVar -> Maybe Type
lookupType context var = snd =<< lookupContext context var

-- | Specialization of 'lookupType' for term variables.
lookupTerm :: Context -> Name -> Maybe Type
lookupTerm context = lookupType context . ContextTermVar

-- | resolveTerm returns a 'name' possibly annotated by a fully
-- substituted type.
--
-- Implementation details: substitution uses both the scope and the
-- vars from the 'Context'. See 'vars' for more information.
resolveTerm :: Context -> Name -> Name
resolveTerm context@Context { scope, vars } name =
  let context' = context { scope = vars ++ scope, vars = [] } in
  case lookupTerm context' name of
    Nothing ->
      name
    Just typ ->
      let typ' = Type.rebuildForall $ substitute context typ in
      Name.annotate name typ'

containsScope :: Eq a => [(a, b)] -> a -> Bool
containsScope xs x = List.elem x $ map fst xs

-- | Determines whether the context variable appears in the 'Context's
-- scope.
contains :: Context -> ContextVar -> Bool
contains context = Maybe.isJust . lookupContext context

-- | Returns true iff the context variable appears in the 'Context's
-- scope and is assigned.
containsAssigned :: Context -> ContextVar -> Bool
containsAssigned context var =
  case lookupContext context var of
    Just (_, Just _) -> True
    _ -> False

-- | Returns true iff the context variable appears in the 'Context's
-- scope and is not assigned.
containsUnassigned :: Context -> ContextVar -> Bool
containsUnassigned context var =
  case lookupContext context var of
    Just (_, Nothing) -> True
    _ -> False

containsType :: Context -> Type -> Bool
containsType context =
  contains context . ContextType

containsTypeUnassigned :: Context -> Type -> Bool
containsTypeUnassigned context =
  containsUnassigned context . ContextType

containsUnassignedTypes :: Context -> [Type] -> Bool
containsUnassignedTypes context =
  all ((==) True) . map (containsTypeUnassigned context)

containsTermAssigned :: Context -> Name -> Bool
containsTermAssigned context =
  containsAssigned context . ContextTermVar

append :: Context -> (ContextVar, Maybe Type) -> Context
append context var =
  context { scope = var:scope context }

appendType :: Context -> Type -> Context
appendType context typ =
  context `append` (ContextType typ, Nothing)

appendWithMarker :: Context -> Type -> Context
appendWithMarker context typ@(ExistVar name) =
  context `append` (ContextMarker name, Nothing) `appendType` typ

appendTermAssign :: Context -> Expr -> Type -> Context
appendTermAssign context (IdE name) typ =
  context `append` (ContextTermVar name, Just typ)

-- | Returns a context in which the scope has been dropped from its
-- deepest part up until and including the element that matches the
-- predicate.
--
-- Implementation details: variables in the part of the scope being
-- discarded are added to the 'Context's vars field to be later used
-- for full substitution. See 'vars' for more information.
sliceAt :: Context -> ((ContextVar, Maybe Type) -> Bool) -> Context
sliceAt context@Context { scope, vars } predicate =
  case predicate `List.findIndex` scope of
    Nothing ->
      context
    Just index ->
      let
        (discard, keep) = splitAt (index + 1) scope
        newVars = filter isVar discard
        allVars = newVars ++ vars
        contextVars = map substituteVar allVars
      in
        context { scope = keep, vars = contextVars }
  where
    isVar (ContextTermVar _, _) = True
    isVar _ = False

    substituteVar var@(_, Nothing) =
      var
    substituteVar (ContextTermVar name, Just typ) =
      let typ' = substitute context typ in
      (ContextTermVar name, Just typ')

sliceAtMarker :: Context -> Type -> Context
sliceAtMarker context (ExistVar name) =
  sliceAt context $ (==) (ContextMarker name, Nothing)

sliceAtType :: Context -> Type -> Context
sliceAtType context typ =
  sliceAt context isType
  where
    isType (ContextType x, _) = x == typ
    isType _ = False

sliceAtTermVar :: Context -> Expr -> Context
sliceAtTermVar context (IdE name) =
  sliceAt context isVar
  where
    isVar (ContextTermVar x, _) = x == name
    isVar _ = False

assign :: Context -> ContextVar -> Type -> Context
assign context@Context { scope } var typ =
  context { scope = loop scope }
  where
    loop [] = []
    loop ((x, y):xs)
      | x == var = ((x, Just typ):xs)
      | otherwise = (x, y):loop xs

assignType :: Context -> Type -> Type -> Context
assignType context typ@ExistVar {} =
  assign context (ContextType typ)

assignWithExistVars :: Context -> ContextVar -> [Type] -> Context
assignWithExistVars context@Context { scope } var types =
  context { scope = loop scope }
  where
    loop [] = []
    loop ((x, y):xs)
      | x == var =
        let (solution:existVars) = List.reverse types in
        [(x, Just solution)] ++
        map (\t -> (ContextType t, Nothing)) existVars ++
        xs
      | otherwise =
        (x, y):loop xs

assignTypeWithExistVars :: Context -> Type -> [Type] -> Context
assignTypeWithExistVars context typ types =
  assignWithExistVars context (ContextType typ) types

isTypeWellFormed :: Context -> Type -> Bool
isTypeWellFormed context typ@(TypeVar _) =
  contains context $ ContextType typ
isTypeWellFormed _ typ
  | Type.isUnit typ = True
isTypeWellFormed context (Arrow type1 type2) =
  isTypeWellFormed context type1 && isTypeWellFormed context type2
isTypeWellFormed context (Forall var typ) =
  isTypeWellFormed (appendType context (TypeVar var)) typ
isTypeWellFormed context typ@(ExistVar _) =
  contains context $ ContextType typ
isTypeWellFormed _ PrimitiveT {} =
  True
-- TODO Revise with respect to theory.
isTypeWellFormed context (ListT typ) =
  isTypeWellFormed context typ
-- TODO Revise with respect to theory.
isTypeWellFormed context (TupleT types) =
  all ((==) True) $ map (isTypeWellFormed context) types

isContextWellFormed :: Context -> Bool
isContextWellFormed context =
  isScopeWellFormed $ scope context
  where
    isScopeWellFormed :: [(ContextVar, Maybe Type)] -> Bool
    isScopeWellFormed [] =
      True
    isScopeWellFormed ((var@(ContextType (TypeVar _)), Nothing):scope) =
      isScopeWellFormed scope &&
      not (containsScope scope var)
    isScopeWellFormed ((var@ContextTermVar {}, Just typ):scope) =
      isScopeWellFormed scope &&
      not (containsScope scope var) &&
      isTypeWellFormed context { scope = scope } typ
    isScopeWellFormed ((var@(ContextType (ExistVar _)), Nothing):scope) =
      isScopeWellFormed scope &&
      not (containsScope scope var)
    isScopeWellFormed ((var@(ContextType (ExistVar _)), (Just typ)):scope) =
      isScopeWellFormed scope &&
      not (containsScope scope var) &&
      Type.isMonotype typ &&
      isTypeWellFormed context { scope = scope } typ
    isScopeWellFormed ((marker@(ContextMarker name), Nothing):scope) =
      isScopeWellFormed scope &&
      not (containsScope scope marker) &&
      not (containsScope scope (ContextType (ExistVar name)))
    isScopeWellFormed _ =
      False

substitute :: Context -> Type -> Type
substitute _ typ@TypeVar {} = typ
substitute _ typ
  | Type.isUnit typ = typ
substitute context typ@ExistVar {}
  | contains context (ContextType typ) =
    case lookupType context (ContextType typ) of
      Just typ' -> substitute context typ'
      Nothing -> typ
  | otherwise =
    error $ "substitute: failed to lookup in context " ++ show typ
substitute context (Arrow type1 type2) =
  Arrow (substitute context type1) (substitute context type2)
substitute context (Forall var typ) =
  Forall var $ substitute context typ
substitute _ typ@PrimitiveT {} = typ
substitute context (ListT typ) =
  ListT $ substitute context typ
substitute context (TupleT types) =
  TupleT $ map (substitute context) types
