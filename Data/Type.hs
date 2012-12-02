module Data.Type where

import Data.Functor ((<$>))
import Data.List (intercalate)


data Type
    = BoolT
    | IntT
    | DoubleT
    | CharT

    | TupT [Type]
    | SeqT Type

    | DynT
    | ArrowT Type Type
      
    | ExistT String
    | ForallT String Type
    | TvarT String
      deriving (Eq)

instance Show Type where
    show BoolT = "Bool"
    show IntT = "Int"
    show DoubleT = "Double"
    show CharT = "Char"
    show (TupT ts) = "[|" ++ intercalate ", " (map show ts) ++ "|]"
    show (SeqT t) = "[" ++ show t ++ "]"
    show DynT = "Dyn"
    show (ArrowT t1@(ArrowT _ _) t2) = "(" ++ show t1 ++ ") -> " ++ show t2
    show (ArrowT t1 t2) = show t1 ++ " -> " ++ show t2

    show (ExistT str) = '^':str
    show (ForallT var t) = "forall " ++ var ++ ". " ++ show t
    show (TvarT str) = str


isAtomicT :: Type -> Bool
isAtomicT (ArrowT _ _) = False
isAtomicT (ExistT _) = False
isAtomicT _ = True


simpleType :: a -> (a, Maybe b)
simpleType t = (t, Nothing)


unifType :: a -> b -> (a, Maybe b)
unifType t1 t2 = (t1, Just t2)


generalizeT :: Type -> Type -> Maybe Type
generalizeT t1 t2 | t1 == t2 = Just t1
    
generalizeT (SeqT t1) (SeqT t2) =
    SeqT <$> generalizeT t1 t2

generalizeT (ArrowT arg1 res1) (ArrowT arg2 res2) =
    do t1 <- generalizeT arg1 arg2
       ArrowT t1 <$> generalizeT res1 res2

generalizeT t1 t2 = Just DynT


generalizeTs :: [Type] -> Maybe Type
generalizeTs [] = Nothing
generalizeTs [t] = Just t
generalizeTs (t1:t2:ts) =
    generalizeT t1 t2 >>= generalizeTs . (:ts)


-- edit: 'substituteT' should substitute one type for another
-- instead of substituting a type for a string
-- edit: use renaming to avoid variable capture?
substituteT _ _ BoolT = BoolT
substituteT _ _ IntT = IntT
substituteT _ _ DoubleT = DoubleT
substituteT _ _ CharT = CharT
substituteT t var (TupT ts) = TupT $ map (substituteT t var) ts
substituteT t var (SeqT seqT) = SeqT $ substituteT t var seqT
substituteT t var DynT = DynT

substituteT t var (ArrowT fnT argT) =
  ArrowT (substituteT t var fnT) (substituteT t var argT)

substituteT t var1 existT@(ExistT var2)
  | var1 == var2 = t
  | otherwise = existT

substituteT t var (ForallT vars forallT) =
  ForallT vars $ substituteT t var forallT

substituteT t var1 tvarT@(TvarT var2)
  | var1 == var2 = t
  | otherwise = tvarT