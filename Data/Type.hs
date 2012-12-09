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