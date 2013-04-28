module Data.Type where

import Data.Functor ((<$>))
import Data.List (intercalate, nub, sort)

import Data.Map (Map)
import qualified Data.Map as Map


data Type
    = BoolT
    | IntT
    | DoubleT
    | CharT

    | TupT [Type]
    | SeqT Type

    | DynT
    | ArrowT Type Type
      
    | EvarT String
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

    show (EvarT str) = str
    
    show (ForallT var t@(ForallT _ _)) = "forall " ++ var ++ showForall t
      where showForall (ForallT var t) = "," ++ var ++ showForall t
            showForall t = ". " ++ show t

    show (ForallT var t) = "forall " ++ var ++ ". " ++ show t
    show (TvarT str) = str


isEvarT :: Type -> Bool
isEvarT (EvarT _) = True
isEvarT _ = False


isForallT :: Type -> Bool
isForallT (ForallT _ _) = True
isForallT _ = False


isAtomicT :: Type -> Bool
isAtomicT (ArrowT _ _) = False
isAtomicT _ = True


simpleType :: Type -> (Type, Maybe Type)
simpleType t = (t, Nothing)


unifType :: a -> b -> (a, Maybe b)
unifType t1 t2 = (t1, Just t2)


freshForallT :: Int -> Type -> (Int, Type)
freshForallT count t =
    let ((_, count'), t') = freshForallT' (Map.empty, count) t in (count', t')
    where freshForallT' state t@BoolT = (state, t)
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
                  (state', t1') = freshForallT' state t1
                  (state'', t2') = freshForallT' state' t2
              in
                (state'', ArrowT t1' t2')

          freshForallT' state t@(EvarT _) = (state, t)

          freshForallT' (vars, n) (ForallT var forallT) =
              let
                  -- edit: toEnum overflows
                  var' = [toEnum n]
                  vars' = Map.insert var var' vars
                  n' = n + 1
                  (state', forallT') = freshForallT' (vars', n') forallT
              in
                (state', ForallT var' forallT')

          freshForallT' state@(vars, _) t@(TvarT var) =
              case Map.lookup var vars of
                Nothing -> (state, t)
                Just var' -> (state, TvarT var')


kickForalls :: Type -> Type
kickForalls t =
    let (vars, t') = kick [] t in
    foldl (\t var -> ForallT var t) t' vars
    where kick vars t@BoolT = (vars, t)
          kick vars t@IntT = (vars, t)
          kick vars t@DoubleT = (vars, t)
          kick vars t@CharT = (vars, t)

          kick vars (TupT ts) = loop [] vars ts
              where loop res vars [] = (vars, TupT (reverse res))
                    loop res vars (t:ts) =
                        let (vars', t') = kick vars t in
                        loop (t':res) vars' ts

          kick vars (SeqT t) =
              let (vars', t') = kick vars t in
              (vars', SeqT t')

          kick vars t@DynT = (vars, t)

          kick vars t@(ArrowT argT rangeT) =
              let
                  (vars', argT') = kick vars argT
                  (vars'', rangeT') = kick vars' rangeT
              in
                (vars'', ArrowT argT' rangeT')

          kick vars t@(EvarT _) = (vars, t)

          kick vars (ForallT var forallT) =
              kick (var:vars) forallT

          kick vars t@(TvarT _) = (vars, t)


freeTvarsT :: Type -> [String]
freeTvarsT t = nub $ sort $ freeTvars [] t
    where freeTvars _ BoolT = []
          freeTvars _ IntT = []
          freeTvars _ DoubleT = []
          freeTvars _ CharT = []

          freeTvars vars (TupT tupTs) =
              concatMap (freeTvars vars) tupTs

          freeTvars vars (SeqT seqT) =
              freeTvars vars seqT

          freeTvars _ DynT = []

          freeTvars vars (ArrowT argT rangeT) =
              freeTvars vars argT ++ freeTvars vars rangeT

          freeTvars vars (EvarT var) = [var]

          freeTvars vars (ForallT var forallT) =
              freeTvars (var:vars) forallT

          freeTvars vars (TvarT var) =
              if var `elem` vars then
                  []
              else
                  [var]


-- 'occursT' @t1 t2@: does @t1@ occur in @t2@?
occursT t1 t2 | t1 == t2 = True
occursT t (TupT tupTs) = any (occursT t) tupTs
occursT t (SeqT seqT) = occursT t seqT
occursT t (ArrowT argT rangeT) = occursT t argT || occursT t rangeT
occursT t (ForallT _ forallT) = occursT t forallT
occursT _ _ = False


-- unifyT :: Type -> Type -> Type
-- unifyT t1 t2
--     | t1 == t2 = t1

-- unifyT (TupT ts1) (TupT ts2) =
--     let ts = zipWith unifyT ts1 ts2 in
--     if all (== (head ts)) ts then
--         SeqT $ head ts
--     else
--         TupT ts

-- unifyT (SeqT t1) (SeqT t2) =
--     SeqT $ unifyT t1 t2

-- unifyT (ArrowT arg1 res1) (ArrowT arg2 res2) =
--     let t1 = unifyT arg1 arg2 in
--     ArrowT t1 $ unifyT res1 res2

-- unifyT t1 t2 = DynT


-- 'substituteTvarT' @t1 var t2@ replaces type variables
-- named @var@ that occur in @t2@ with @t1@.
substituteTvarT :: Type -> String -> Type -> Type
substituteTvarT _ _ BoolT = BoolT
substituteTvarT _ _ IntT = IntT
substituteTvarT _ _ DoubleT = DoubleT
substituteTvarT _ _ CharT = CharT
substituteTvarT t var (TupT ts) = TupT $ map (substituteTvarT t var) ts
substituteTvarT t var (SeqT seqT) = SeqT $ substituteTvarT t var seqT
substituteTvarT t var DynT = DynT

substituteTvarT t var (ArrowT fnT argT) =
  ArrowT (substituteTvarT t var fnT) (substituteTvarT t var argT)

substituteTvarT _ _ t@(EvarT _) = t

substituteTvarT t var (ForallT vars forallT) =
  ForallT vars $ substituteTvarT t var forallT

substituteTvarT t var1 tvarT@(TvarT var2)
  | var1 == var2 = t
  | otherwise = tvarT


rebuildForallT :: Type -> Type
rebuildForallT t = foldr ForallT t (freeTvarsT t)
    