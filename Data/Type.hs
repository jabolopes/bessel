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


freshForallT :: Int -> Type -> (Int, Type)
freshForallT count t =
    let ((_, count'), t') = freshForallT' (Map.empty, count) t in (count', t')
    where freshForallT' :: (Map String String, Int) -> Type -> ((Map String String, Int), Type)
          freshForallT' state t@BoolT = (state, t)
          freshForallT' state t@IntT = (state, t)
          freshForallT' state t@DoubleT = (state, t)
          freshForallT' state t@CharT = (state, t)

          freshForallT' state (TupT tupTs) = loop state [] tupTs
              where loop state tupTs [] = (state, TupT (reverse tupTs))
                    loop state tupTs (t:ts) =
                        let (state', t') = freshForallT' state t in
                        loop state' (t':tupTs) ts

          freshForallT' state (SeqT seqT) =
              let (state', seqT') = freshForallT' state seqT in
              (state', SeqT seqT')

          freshForallT' state t@DynT = (state, t)

          freshForallT' state (ArrowT t1 t2) =
              let
                  (state', t1') = (freshForallT' state t1)
                  (state'', t2') = (freshForallT' state' t2)
              in
                (state'', ArrowT t1' t2')

          freshForallT' state t@(ExistT _) = (state, t)

          freshForallT' (vars, n) (ForallT var forallT) =
              let
                  var' = var ++ show n
                  vars' = Map.insert var var' vars
                  n' = n + 1
                  (state', forallT') = freshForallT' (vars', n') forallT
              in
                (state', ForallT var' forallT')

          freshForallT' state@(vars, _) (TvarT var) =
              (state, TvarT (vars Map.! var))
