module Typechecker.Type where

import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Literal (Literal(..))
import Typechecker.TypeName (TypeName(..))
import qualified Typechecker.TypeName as TypeName

data Type
  = TypeVar TypeName
  | ExistVar TypeName
  | Forall TypeName Type
  | Arrow Type Type
  | PrimitiveT String
  | ListT Type
  | TupleT [Type]
    deriving (Eq, Ord)

instance Show Type where
  show (TypeVar name) = show name
  show (ExistVar name) = "^" ++ show name
  show (Forall name typ) = "forall " ++ show name ++ ". " ++ show typ
  show (Arrow t1@Arrow {} t2) = "(" ++ show t1 ++ ")" ++ " -> " ++ show t2
  show (Arrow t1 t2) = show t1 ++ " -> " ++ show t2
  show (PrimitiveT name) = name
  show (ListT typ) = "List " ++ show typ
  show (TupleT types) = "(" ++ List.intercalate ", " (map show types) ++ ")"

boolT :: Type
boolT = PrimitiveT "Bool"

charT :: Type
charT = PrimitiveT "Char"

intT :: Type
intT = PrimitiveT "Int"

realT :: Type
realT = PrimitiveT "Real"

stringT :: Type
stringT = PrimitiveT "String"

literalT :: Literal -> Type
literalT CharL {} = charT
literalT IntL {} = intT
literalT RealL {} = realT
literalT StringL {} = stringT

unitT :: Type
unitT = TupleT []

isUnit :: Type -> Bool
isUnit (TupleT []) = True
isUnit _ = False

-- TODO: Revise list / tuple with respect to theory.
isMonotype :: Type -> Bool
isMonotype typ | isUnit typ = True
isMonotype TypeVar {} = True
isMonotype ExistVar {} = True
isMonotype (Arrow t1 t2) = isMonotype t1 && isMonotype t2
isMonotype PrimitiveT {} = True
isMonotype _ = False

substituteTypeVar :: Type -> Type -> Type -> Type
substituteTypeVar (TypeVar name) target = sub
  where
    sub typ | isUnit typ = typ
    sub typ@(TypeVar name')
      | name == name' = target
      | otherwise = typ
    sub typ@ExistVar {} =
      typ
    sub typ@(Forall name' body)
      | name == name' = typ
      | otherwise = Forall name' $ sub body
    sub (Arrow type1 type2) =
      Arrow (sub type1) (sub type2)
    sub typ@PrimitiveT {} = typ
    sub (ListT typ) = ListT (sub typ)
    sub (TupleT types) = TupleT $ map sub types
substituteTypeVar _ _ =
  error "Type.substituteTypeVar: expected type variable"

substitute :: Type -> Type -> Type -> Type
substitute source target typ
  | source == typ = target
  | otherwise = sub typ
  where
    sub (Forall name body) =
      Forall name $ substitute source target body
    sub (Arrow type1 type2) =
      Arrow
        (substitute source target type1)
        (substitute source target type2)
    sub (ListT listType) = substitute source target listType
    sub (TupleT types) = TupleT $ map (substitute source target) types
    sub x = x

freeVars :: Type -> Set Type
freeVars = free Set.empty Set.empty
  where
    free vars _ typ | isUnit typ = vars
    free vars scope typ@(TypeVar _)
      | typ `Set.member` scope = vars
      | otherwise = typ `Set.insert` vars
    free vars scope typ@(ExistVar _)
      | typ `Set.member` scope = vars
      | otherwise = typ `Set.insert` vars
    free vars scope (Forall name body) =
      let scope' = TypeVar name `Set.insert` scope in
      free vars scope' body
    free vars scope (Arrow type1 type2) =
      let vars' = free vars scope type1 in
      free vars' scope type2
    free vars _ PrimitiveT {} = vars
    free vars scope (ListT typ) =
      free vars scope typ
    free vars scope (TupleT types) =
      foldl (\vs typ -> free vs scope typ) vars types

rebuildForall :: Type -> Type
rebuildForall typ =
  rebuild (TypeName.typeName 'a') (Set.toList (freeVars typ)) typ
  where
    rebuild _ [] t = t
    rebuild typeName@(TypeName i) (var:vars) t =
      let t' = substitute var (TypeVar typeName) t in
      Forall typeName $ rebuild (TypeName (succ i)) vars t'
