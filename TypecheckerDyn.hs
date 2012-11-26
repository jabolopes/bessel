module TypecheckerDyn where

import Data.Functor ((<$>))
import Data.List (find)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe

import Data.Stx


import Debug.Trace


idsOf :: Stx a -> [Stx a]
idsOf (IntStx _) = []
idsOf (SeqStx stxs) = concatMap idsOf stxs
idsOf stx@(IdStx val) = [stx]
idsOf (AppStx stx1 stx2) = idsOf stx1 ++ idsOf stx2
idsOf (LambdaStx _ body) = idsOf body


data Type
    = BoolT
    | IntT

    | SeqT Type

    | DynT
    | ArrowT Type Type
      deriving (Eq)

instance Show Type where
    show BoolT = "Bool"
    show IntT = "Int"
    show (SeqT t) = "[" ++ show t ++ "]"
    show DynT = "Dyn"
    show (ArrowT t1@(ArrowT _ _) t2) = "(" ++ show t1 ++ ") -> " ++ show t2
    show (ArrowT t1 t2) = show t1 ++ " -> " ++ show t2


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


data Mode
    = Synth
    | Check Type


dynT = DynT

dynT' _ = DynT


BoolT <: BoolT = True
IntT <: IntT = True
(SeqT t1) <: (SeqT t2) = t1 <: t2

(ArrowT argT1 rangeT1) <: (ArrowT argT2 rangeT2) =
    argT2 <: argT1 && rangeT1 <: rangeT2

DynT <: _ = True
_ <: DynT = True

t1 <: t2 | trace (show t1 ++ " <: " ++ show t2 ++ " (false)") True = False


(~~) = (<:)


typecheckM :: Map String Type -> Mode -> Stx String -> (Type, Stx (String, Type, Type))

-- S-Int
typecheckM syms Synth (IntStx i) =
    (IntT, IntStx i)

-- C-Int
typecheckM syms (Check t) (IntStx i)
    | IntT ~~ t = (IntT, IntStx i)
    | otherwise = error $ "\n\n\ttypecheckM: IntStx: type inconsistency" ++
                          "\n\n\t\t" ++ show IntT ++ " ~~ " ++ show t ++ " (false)\n"

-- S-Seq
typecheckM syms Synth (SeqStx stxs) =
    (SeqT seqT, SeqStx stxs')
    where (ts, stxs') = unzip $ map (typecheckM syms Synth) stxs
          seqT = fromJust $ generalizeTs ts

-- C-Seq
typecheckM syms (Check (SeqT t)) (SeqStx stxs) =
    (SeqT elT, SeqStx stxs')
    where (elT:_, stxs') = unzip $ map (typecheckM syms (Check t)) stxs

typecheckM syms (Check t) (SeqStx stxs) =
    (t, SeqStx stxs')
    where (elT:_, stxs') = unzip $ map (typecheckM syms (Check DynT)) stxs

-- S-Var
typecheckM syms Synth stx@(IdStx name) =
    case Map.lookup name syms of
      Nothing -> error $ "typecheckM: IdStx: name " ++ show name ++ " not bound"
      Just t -> (t, IdStx (name, t, t))

-- C-Var
typecheckM syms (Check t) stx@(IdStx name) =
    if idT ~~ t then
        (t, IdStx (name, idT, t))
    else
        error $ "\n\n\ttypecheckM: IdStx: type inconsistency" ++
                "\n\n\t\t" ++ show idT ++ " ~~ " ++ show t ++ " (false)\n"
    where idT = case Map.lookup name syms of
                  Nothing -> error $ "typecheckM: IdStx: name " ++ show name ++ " not bound"
                  Just idT -> idT

-- S-Abs (annotated)
-- typecheckM syms Synth stx@(LambdaStx arg body) =
--     error "typecheckM: LambdaStx (annotated): Synth: not implemented"

-- C-Abs (annotated)
-- typecheckM syms (Check _) stx@(LambdaStx arg body) =
--     error "typecheckM: LambdaStx (annotated): Check: not implemented"

-- S-Abs (unannotated) no rule in Pierce's paper, use Siek instead
typecheckM syms Synth (LambdaStx arg body) =
    (ArrowT argT bodyT, LambdaStx arg body')
    where (bodyT, body') = typecheckM (Map.insert arg DynT syms) Synth body

          argT = case find (\(IdStx (name, _, _)) -> name == arg) $ idsOf body' of
                   Nothing -> DynT
                   Just (IdStx (_, _, argT)) -> argT
                   

-- C-Abs (unannotated)
-- edit: type consistency? pierce's paper does not have
typecheckM syms (Check t@(ArrowT argT rangeT)) (LambdaStx arg body) =
    (t, LambdaStx arg body')
    where (_, body') = typecheckM (Map.insert arg argT syms) (Check rangeT) body

typecheckM syms (Check t@DynT) (LambdaStx arg body) =
    (t, LambdaStx arg body')
    where (_, body') = typecheckM (Map.insert arg DynT syms) (Check DynT) body    

-- S-App with lambda
typecheckM syms Synth (AppStx fn@(LambdaStx x body) arg) =
    (rangeT, AppStx fn' arg')
    where (_, fn') = typecheckM syms (Check (ArrowT argT rangeT)) fn
          (argT, arg') = typecheckM syms Synth arg
          (rangeT, _) = typecheckM (Map.insert x argT syms) Synth body

-- S-App
typecheckM syms Synth (AppStx fn arg) =
    (rangeT, AppStx fn' arg')
    where (ArrowT argT rangeT, fn') = typecheckM syms Synth fn
          (_, arg') = typecheckM syms (Check argT) arg

-- C-App
typecheckM syms (Check t) (AppStx fn arg) =
    if rangeT ~~ t then
        (rangeT, AppStx fn' arg')
    else
        error "typecheckM: AppStx: type inconsistency"
    where (ArrowT argT rangeT, fn') = case typecheckM syms Synth fn of
                                        val@(ArrowT _ _, _) -> val
                                        (DynT, fn') -> (ArrowT dynT dynT, fn')
                                        (t, _) -> error $ "\n\n\ttypecheckM: Check: AppStx: expected arrow type, got " ++ show t ++ "\n"
          (_, arg') = typecheckM syms (Check argT) arg


tc stx =
    typecheckM symbols Synth stx

tc' stx = (t, idsOf stx')
    where (t, stx') = typecheckM symbols Synth stx

tc'' stx =
    do putStrLn $ show t
       mapM_ (putStrLn . show) ids
    where (t, ids) = tc' stx


typecheck = tc'



syms :: [(String, Type)]
syms = [
 ("K", ArrowT dynT (ArrowT dynT dynT)),

 -- constants
 ("false", BoolT),
 ("true", BoolT),

 -- -- predicates
 -- --- predicates for numbers
 ("isint", ArrowT IntT BoolT),
 -- ("isreal", m isreal),
 -- ("isnum", m isnum),
 -- ("ispos", m ispos),
 -- ("isneg", m isneg),
 -- ("iszero", m iszero),
 -- --- predicates for other atoms, functions, and user types
 -- ("isatom", m isatom),
 -- ("isbool", m isbool),
 -- ("isutype", m isutype),
 -- ("isfunc", m isfunc),
 -- --- predicates for sequences
 -- ("isnull", m isnull),
 -- ("ispair", m ispair),
 -- ("isseq", m isseq),
 -- ("isstring", m isstring),
 -- --- identically true and false predicates
 -- ("ff", m ff),
 -- ("isval", m isval),
 -- ("tt", m tt),
 -- -- boolean and comparison functions
 -- --- boolean functions
 -- ("and", m and),
 -- ("not", m not),
 -- ("or", m or),
 -- --- comparison functions
 -- ("eq", m eq),
 -- ("less", m less),
 -- -- arithmetic functions
 ("+", ArrowT IntT (ArrowT IntT IntT)),
 -- ("sub", m sub),
 -- ("mult", m mul),
 -- ("div", m div),
 -- ("floor", m floor),
 -- ("ceiling", m ceiling),
 -- ("abs", m abs),
 -- -- combining forms
 -- ("o", m o),
 -- ("cons", m cons),
 ("cond", ArrowT (ArrowT dynT BoolT) (ArrowT (ArrowT dynT dynT) (ArrowT dynT dynT))),
 -- ("apply", m apply),
 -- ("lift", m lift),
 -- -- ("raise", m raise),
 -- -- predicate combining forms
 -- ("pcons", m pcons),
 -- ("seqof", m seqof),
 -- ("lenis", m lenis),
 -- ("&&", m (&&)),
 -- ("||", m (||)),
 -- ("¬", m (¬)),
 -- ("=>", m rarrow),
 -- ("<=", m (<=)),
 -- ("map", m alpha),
 ("mapInt", ArrowT (ArrowT IntT IntT) (ArrowT IntT IntT))]
 -- ("al", m al),
 -- ("ar", m ar),
 -- ("cat", m cat),
 -- ("distl", m distl),
 -- ("distr", m distr),
 -- ("len", m len),
 -- ("reverse", m reverse),
 -- ("sel", m sel),
 -- ("s", m s),
 -- ("hd", m hd),
 -- ("tl", m tl),
 -- ("hdr", m hdr),
 -- ("tlr", m tlr),
 -- -- io
 -- ("out", m out),
 -- -- misc
 -- ("id", m id),
 -- ("signal", m signal)


symbols :: Map String Type
symbols = Map.fromList syms



ex0 :: Int
ex0 = (\f -> \x -> f x) (\x -> x + 1) 3

ex0' =
    (AppStx
     (AppStx
      (LambdaStx "f" (LambdaStx "x" (AppStx (IdStx "f") (IdStx "x"))))
      (LambdaStx "y" (AppStx (AppStx (IdStx "+") (IdStx "y")) (IntStx 1))))
     (IntStx 3))


ex1 :: Int
ex1 = (\f -> \g -> \x -> f (g x)) (\y -> y + 1) (\z -> z + 2) 3

ex1' =
    (AppStx
     (AppStx
      (AppStx
       (LambdaStx "f" (LambdaStx "g" (LambdaStx "x" (AppStx (IdStx "f") (AppStx (IdStx "g") (IdStx "x"))))))
       (LambdaStx "y" (AppStx (AppStx (IdStx "+") (IdStx "y")) (IntStx 1))))
      (LambdaStx "z" (AppStx (AppStx (IdStx "+") (IdStx "z")) (IntStx 2))))
     (IntStx 3))


ex2 :: Int -> Int
ex2 = (\f -> \g -> \x -> f (g x)) (\x -> x + 1) (\y -> y - 2)


ex2' =
    (AppStx
     (AppStx
      (LambdaStx "f" (LambdaStx "g" (LambdaStx "x" (AppStx (IdStx "f") (AppStx (IdStx "g") (IdStx "x"))))))
      (LambdaStx "y" (AppStx (AppStx (IdStx "+") (IdStx "y")) (IntStx 1))))
     (LambdaStx "z" (AppStx (AppStx (IdStx "+") (IdStx "z")) (IntStx 2))))


ex3 =
    map (\x -> x + 1) [1,2,3]


ex3' =
    (AppStx
     (AppStx
      (IdStx "mapInt")
      (LambdaStx "x" (AppStx (AppStx (IdStx "+") (IdStx "x")) (IntStx 1))))
     (IntStx 2))