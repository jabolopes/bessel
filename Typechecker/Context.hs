module Typechecker.Context where

import Prelude hiding (last, lookup)

import qualified Data.List as List

import Data.Expr
import Data.Name
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
type Context = [(ContextVar, Maybe Type)]

lookup :: Context -> ContextVar -> Maybe Type
lookup [] _ = Nothing
lookup ((x, y):xs) var
  | x == var = y
  | otherwise = lookup xs var

lookupTerm :: Context -> Expr -> Maybe Type
lookupTerm context (IdE name) =
  lookup context $ ContextTermVar name

contains :: Context -> ContextVar -> Bool
contains [] _ = False
contains ((x, _):xs) var =
  x == var || contains xs var

containsAssigned :: Context -> ContextVar -> Bool
containsAssigned [] _ = False
containsAssigned ((x, y):xs) var
  | x == var =
    case y of
      Nothing -> False
      _ -> True
  | otherwise = containsAssigned xs var

containsUnassigned :: Context -> ContextVar -> Bool
containsUnassigned [] _ = False
containsUnassigned ((x, y):xs) var
  | x == var =
    case y of
      Nothing -> True
      _ -> False
  | otherwise = containsUnassigned xs var

containsUnassignedTypes :: Context -> [Type] -> Bool
containsUnassignedTypes _ [] = True
containsUnassignedTypes [] _ = False
containsUnassignedTypes ((x, y):xs) vs@(var:vars)
  | x == ContextType var =
    case y of
      Nothing -> containsUnassignedTypes xs vars
      _ -> False
  | otherwise =
    containsUnassignedTypes xs vs

containsType :: Context -> Type -> Bool
containsType context =
  contains context . ContextType

-- TODO: change TypeName to Term?
containsTermAssigned :: Context -> Name -> Bool
containsTermAssigned context =
  containsAssigned context . ContextTermVar

containsTypeUnassigned :: Context -> Type -> Bool
containsTypeUnassigned context =
  containsUnassigned context . ContextType

append :: Context -> (ContextVar, Maybe Type) -> Context
append = flip (:)

appendType :: Context -> Type -> Context
appendType context typ = (ContextType typ, Nothing):context

appendWithMarker :: Context -> Type -> Context
appendWithMarker context typ@(ExistVar name) =
  appendType ((ContextMarker name, Nothing):context) typ

appendTermAssign :: Context -> Expr -> Type -> Context
appendTermAssign context (IdE name) typ =
  (ContextTermVar name, Just typ):context

sliceAtMarker :: Context -> Type -> Context
sliceAtMarker context typ@(ExistVar name) =
  case (ContextMarker name, Nothing) `List.elemIndex` context of
    Nothing -> context
    Just index -> drop (index + 1) context

sliceAtType :: Context -> Type -> Context
sliceAtType context typ =
  case List.findIndex isType context of
    Just index -> drop (index + 1) context
    Nothing -> context
  where
    isType (ContextType x, _) = x == typ
    isType _ = False

sliceAtTermVar :: Context -> Expr -> Context
sliceAtTermVar context (IdE name) =
  case List.findIndex isVar context of
    Just index -> drop (index + 1) context
    Nothing -> context
  where
    isVar (ContextTermVar x, _) = x == name
    isVar _ = False

assign :: Context -> ContextVar -> Type -> Context
assign [] _ _ = []
assign ((x, y):xs) type1 type2
  | x == type1 = ((x, Just type2):xs)
  | otherwise = (x, y):assign xs type1 type2

assignType :: Context -> Type -> Type -> Context
assignType context typ@ExistVar {} =
  assign context (ContextType typ)

assignWithExistVars :: Context -> ContextVar -> [Type] -> Context
assignWithExistVars [] _ _ = []
assignWithExistVars ((x, y):xs) typ types
  | x == typ =
    let (solution:existVars) = List.reverse types in
    [(x, Just solution)] ++
    map (\t -> (ContextType t, Nothing)) existVars ++
    xs
  | otherwise =
    (x, y):assignWithExistVars xs typ types

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
isContextWellFormed [] =
  True
isContextWellFormed ((var@(ContextType (TypeVar _)), Nothing):context) =
  isContextWellFormed context &&
  not (contains context var)
isContextWellFormed ((var@ContextTermVar {}, Just typ):context) =
  isContextWellFormed context &&
  not (contains context var) &&
  isTypeWellFormed context typ
isContextWellFormed ((var@(ContextType (ExistVar _)), Nothing):context) =
  isContextWellFormed context &&
  not (contains context var)
isContextWellFormed ((var@(ContextType (ExistVar _)), (Just typ)):context) =
  isContextWellFormed context &&
  not (contains context var) &&
  Type.isMonotype typ &&
  isTypeWellFormed context typ
isContextWellFormed ((marker@(ContextMarker name), Nothing):context) =
  isContextWellFormed context &&
  not (contains context marker) &&
  not (contains context (ContextType (ExistVar name)))
isContextWellFormed _ =
  False

substitute :: Context -> Type -> Type
substitute _ typ@TypeVar {} = typ
substitute _ typ
  | Type.isUnit typ = typ
substitute context typ@ExistVar {}
  | contains context (ContextType typ) =
    case lookup context (ContextType typ) of
      Just typ' -> substitute context typ'
      Nothing -> typ
  | otherwise =
    error $ "substitute: failed to lookup in context " ++ show typ
substitute context (Arrow type1 type2) =
  Arrow (substitute context type1)
        (substitute context type2)
substitute context (Forall var typ) =
  Forall var $ substitute context typ
substitute _ typ@PrimitiveT {} = typ
substitute context (ListT typ) =
  ListT (substitute context typ)
substitute context (TupleT types) =
  TupleT $ map (substitute context) types
