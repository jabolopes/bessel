module TypecheckerTopBot where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Stx


data Type
    = BoolT
    | IntT

    | SeqT Type

    | TopT
    | BotT
    | ArrowT Type Type
      deriving (Eq, Show)


data Mode
    = Synth
    | Check Type


dynT = TopT

dynT' TopT = TopT
dynT' BotT = BotT


BoolT <: BoolT = True
IntT <: IntT = True
(SeqT t1) <: (SeqT t2) = t1 <: t2

_ <: TopT = True
BotT <: _ = True

(ArrowT argT1 rangeT1) <: (ArrowT argT2 rangeT2) =
    argT2 <: argT1 && rangeT1 <: rangeT2

_ <: _ = False


(~~) = (<:)


-- S-Int
typecheckM syms Synth stx@(IntStx _) = (IntT, stx)

-- C-Int
typecheckM syms (Check t) stx@(IntStx _)
    | IntT ~~ t = (IntT, stx)
    | otherwise = error $ "\n\n\ttypecheckM: IntStx: type inconsistency" ++
                          "\n\n\t\t" ++ show IntT ++ " ~~ " ++ show t ++ " (false)\n"

-- S-Seq
typecheckM syms Synth (SeqStx stxs) =
    if all (~~ (head ts)) ts then
        (SeqT (head ts), SeqStx stxs')
    else
        error "typecheckM: SeqStx: type inconsistency"
    where (ts, stxs') = unzip $ map (typecheckM syms Synth) stxs

-- C-Seq
-- edit: probably need to check for type consistency
typecheckM syms (Check (SeqT t)) (SeqStx stxs) =
    (SeqT elT, SeqStx stxs')
    where (elT:_, stxs') = unzip $ map (typecheckM syms (Check t)) stxs

typecheckM _ _ (SeqStx _) = error "typecheckM: SeqStx: type inconsistency"

-- S-Var
typecheckM syms Synth stx@(IdStx name) =
    case Map.lookup name syms of
      Nothing -> error $ "typecheckM: IdStx: name " ++ show name ++ " not bound"
      Just t -> (t, stx)

-- C-Var
typecheckM syms (Check t) stx@(IdStx name) =
    case Map.lookup name syms of
      Nothing -> error $ "typecheckM: IdStx: name " ++ show name ++ " not bound"
      Just idT | idT ~~ t -> (t, stx)
               | otherwise -> error $ "\n\n\ttypecheckM: IdStx: type inconsistency" ++
                                      "\n\n\t\t" ++ show idT ++ " ~~ " ++ show t ++ " (false)\n"

-- S-Abs (annotated)
-- typecheckM syms Synth stx@(LambdaStx arg body) =
--     error "typecheckM: LambdaStx (annotated): Synth: not implemented"

-- C-Abs (annotated)
-- typecheckM syms (Check _) stx@(LambdaStx arg body) =
--     error "typecheckM: LambdaStx (annotated): Check: not implemented"

-- S-Abs (unannotated) no rule in Pierce's paper, use Siek instead
-- typecheckM syms Synth (LambdaStx arg body) =
--     error "typecheckM: LambdaStx (unannotated): Synth: no rule"

typecheckM syms Synth (LambdaStx arg body) =
    (ArrowT argT bodyT, LambdaStx arg body')
    where argT = dynT' TopT
          (bodyT, body') = typecheckM (Map.insert arg argT syms) Synth body

-- C-Abs (unannotated)
-- edit: type consistency? pierce's paper does not have
typecheckM syms (Check t@(ArrowT argT rangeT)) (LambdaStx arg body) =
    (t, LambdaStx arg body')
    where (_, body') = typecheckM (Map.insert arg argT syms) (Check rangeT) body

typecheckM syms (Check _) (LambdaStx arg body) =
    error "typecheckM: LambdaStx: type inconsistency"

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
    where (ArrowT argT rangeT, fn') = typecheckM syms Synth fn
          (_, arg') = typecheckM syms (Check argT) arg


tc stx =
    typecheckM symbols Synth stx


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
 ("cond", ArrowT (ArrowT dynT BoolT) (ArrowT (ArrowT dynT dynT) (ArrowT dynT dynT)))]
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
      (LambdaStx "x" (AppStx (AppStx (IdStx "+") (IdStx "x")) (IntStx 1))))
     (IntStx 3))


ex1 :: Int
ex1 = (\f -> \g -> \x -> f (g x)) (\x -> x + 1) (\y -> y - 2) 3

ex1' =
    (AppStx
     (AppStx
      (AppStx
       (LambdaStx "f"
        (LambdaStx "g"
         (LambdaStx "x" (AppStx (IdStx "f") (AppStx (IdStx "g") (IdStx "x"))))))
       (LambdaStx "x" (AppStx (AppStx (IdStx "+") (IdStx "x")) (IntStx 1))))
      (LambdaStx "y" (AppStx (AppStx (IdStx "-") (IdStx "y")) (IntStx 2))))
     (IntStx 3))


ex2 :: Int -> Int
ex2 = (\f -> \g -> \x -> f (g x)) (\x -> x + 1) (\y -> y - 2)