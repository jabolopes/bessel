module Core where

import Prelude hiding ((&&), (||), (<=), not, and, or, id, compare, div, floor, ceiling, abs, reverse, read)
import qualified Prelude

import Data.Functor ((<$>))
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO hiding (hGetContents)
import System.IO.Strict
import System.IO.Unsafe

import Data.Env (Env)
import qualified Data.Env as Env (fromList)
import Data.Exception
import Data.SrcFile
import Data.Symbol
import Data.Type
import Monad.InterpreterM
import Renamer


-- predicates

--- predicates for numbers

isint :: Expr -> Expr
isint (IntExpr _) = true
isint _ = false


isreal :: Expr -> Expr
isreal (DoubleExpr _) = true
isreal _ = false


isnum :: Expr -> Expr
isnum expr = or $ SeqExpr [isint expr, isreal expr]


ispos :: Expr -> Expr
ispos (IntExpr i) = BoolExpr $ i > 0
ispos (DoubleExpr n) = BoolExpr $ n > 0


isneg :: Expr -> Expr
isneg (IntExpr i) = BoolExpr $ i < 0
isneg (DoubleExpr n) = BoolExpr $ n < 0


iszero :: Expr -> Expr
iszero (IntExpr i) = BoolExpr $ i == 0
iszero (DoubleExpr n) = BoolExpr $ n == 0.0


-- predicates for other atoms, functions, and user types

isatom :: Expr -> Expr
isatom expr = or $ SeqExpr [isnum expr, isbool expr, ischar expr]


isbool :: Expr -> Expr
isbool (BoolExpr _) = true
isbool _ = false


ischar :: Expr -> Expr
ischar (CharExpr _) = true
ischar _ = false


isutype :: Expr -> Expr
isutype (TypeExpr _ _) = true
isutype _ = false


isfunc' :: Expr -> Bool
isfunc' (FnExpr _) = True
isfunc' _ = False


isfunc :: Expr -> Expr
isfunc = BoolExpr . isfunc'


-- edit: implement this
-- isobj


--- predicates for sequences

isnull :: Expr -> Expr
isnull (SeqExpr []) = true
isnull _ = false


ispair :: Expr -> Expr
ispair (SeqExpr [_, _]) = true
ispair _ = false


isseq :: Expr -> Expr
isseq (SeqExpr _) = true
isseq _ = false


isstring :: Expr -> Expr
isstring (SeqExpr exprs) | all isCharExpr exprs = true
isstring _ = false


--- identically true and false predicates

ff :: Expr -> Expr
ff _ = false


isval :: Expr -> Expr
isval _ = true


tt :: Expr -> Expr
tt _ = true


-- boolean and comparison functions

--- boolean functions

and :: Expr -> Expr
and (BoolExpr False) = false
and expr@(BoolExpr _) = expr
and (SeqExpr exprs) = andHof exprs
    where andHof [] = true
          andHof (expr:_) | isFalseExpr expr = false
          andHof (_:exprs) = andHof exprs
and expr = expr


not :: Expr -> Expr
not (BoolExpr b) = BoolExpr $ Prelude.not b
not _ = false


or :: Expr -> Expr
or expr@(BoolExpr False) = false
or (SeqExpr exprs) = orHof exprs
    where orHof [] = false
          orHof (expr:exprs) | isFalseExpr expr = orHof exprs
          orHof _ = true
or expr = expr


--- comparison functions

eq :: Expr -> Expr
eq expr@(SeqExpr []) = expr
eq expr@(SeqExpr (val1:exprs)) = eq' exprs
    where eq' [] = expr
          eq' (val2:exprs) | isNotFalseExpr (val1 `exprEq` val2) = eq' exprs
          eq' _ = false
eq (BoolExpr False) = true
eq expr = expr


-- neq (see Prelude)


compare :: (Expr -> Expr -> Expr) -> Expr -> Expr
compare _ expr@(SeqExpr []) = expr
compare fn expr@(SeqExpr exprs) = compare' exprs
    where compare' [_] = expr
          compare' (val1:(exprs@(val2:_))) | isNotFalseExpr (val1 `fn` val2) = compare' exprs
          compare' _ = false
compare _ (BoolExpr False) = true
compare _ expr = expr


less :: Expr -> Expr
less = compare exprLt


-- lesseq (see Prelude)
-- greater (see Prelude)
-- greatereq (see Prelude)


--- raised comparison functions (see Prelude)


-- arithmetic functions

add :: Expr -> Expr
add expr@(IntExpr _) = expr
add expr@(DoubleExpr _) = expr
add (SeqExpr exprs) = foldr add' (IntExpr 0) exprs
    where add' (IntExpr i1) (IntExpr i2) = IntExpr $ i1 + i2
          add' (DoubleExpr d1) (DoubleExpr d2) = DoubleExpr $ d1 + d2
          add' (IntExpr i) (DoubleExpr d) = DoubleExpr $ (fromIntegral i) + d
          add' (DoubleExpr d) (IntExpr i) = DoubleExpr $ d + (fromIntegral i)
          add' expr1 _ = signal $ SeqExpr [boxString "add", boxString "arg", expr1]
add expr = signal $ SeqExpr [boxString "add", boxString "arg", expr]


sub :: Expr -> Expr
sub (IntExpr i) = IntExpr $ - i
sub (DoubleExpr d) = DoubleExpr $ - d
sub (SeqExpr [IntExpr i]) = IntExpr $ - i
sub (SeqExpr [DoubleExpr d]) = DoubleExpr $ - d
sub (SeqExpr (expr:exprs)) = expr `sub'` add (SeqExpr exprs)
    where sub' (IntExpr i1) (IntExpr i2) = IntExpr $ i1 - i2
          sub' (DoubleExpr d1) (DoubleExpr d2) = DoubleExpr $ d1 - d2
          sub' (IntExpr i) (DoubleExpr d) = DoubleExpr $ (fromIntegral i) - d
          sub' (DoubleExpr d) (IntExpr i) = DoubleExpr $ d - (fromIntegral i)


mul :: Expr -> Expr
mul expr@(IntExpr _) = expr
mul expr@(DoubleExpr _) = expr
mul (SeqExpr exprs) = foldr mul' (IntExpr 1) exprs
    where mul' (IntExpr i1) (IntExpr i2) = IntExpr $ i1 * i2
          mul' (DoubleExpr d1) (DoubleExpr d2) = DoubleExpr $ d1 * d2
          mul' (IntExpr i) (DoubleExpr d) = DoubleExpr $ (fromIntegral i) * d
          mul' (DoubleExpr d) (IntExpr i) = DoubleExpr $ d * (fromIntegral i)


div :: Expr -> Expr
div expr@(IntExpr _) = expr
div expr@(DoubleExpr _) = expr
div (SeqExpr [IntExpr i]) = DoubleExpr $ 1 / fromIntegral i
div (SeqExpr [DoubleExpr d]) = DoubleExpr $ 1 / d
div (SeqExpr (expr:exprs)) = expr `div'` mul (SeqExpr exprs)
    where div' (IntExpr i1) (IntExpr i2) = DoubleExpr $ fromIntegral i1 / fromIntegral i2
          div' (DoubleExpr d1) (DoubleExpr d2) = DoubleExpr $ d1 / d2
          div' (IntExpr i) (DoubleExpr d) = DoubleExpr $ fromIntegral i / d
          div' (DoubleExpr d) (IntExpr i) = DoubleExpr $ d / fromIntegral i


floor :: Expr -> Expr
floor expr@(IntExpr _) = expr
floor (DoubleExpr d) = IntExpr $ Prelude.floor d
floor (SeqExpr exprs) = SeqExpr $ map floor' exprs
    where floor' expr@(IntExpr i) = expr
          floor' (DoubleExpr d) = IntExpr $ Prelude.floor d


ceiling :: Expr -> Expr
ceiling expr@(IntExpr _) = expr
ceiling (DoubleExpr d) = IntExpr $ Prelude.ceiling d
ceiling (SeqExpr exprs) = SeqExpr $ map floor' exprs
    where floor' expr@(IntExpr i) = expr
          floor' (DoubleExpr d) = IntExpr $ Prelude.ceiling d


abs :: Expr -> Expr
abs (IntExpr i) = IntExpr $ Prelude.abs i
abs (DoubleExpr d) = DoubleExpr $ Prelude.abs d
abs (SeqExpr exprs) = SeqExpr $ map floor' exprs
    where floor' (IntExpr i) = IntExpr $ Prelude.abs i
          floor' (DoubleExpr d) = DoubleExpr $ Prelude.abs d


--- raised arithmetic functions (see Prelude)


-- combining forms

o :: Expr -> Expr
o (FnExpr fn) = FnExpr $ \expr -> fn expr
o (SeqExpr fns) = FnExpr $ \expr -> foldr (\(FnExpr fn) arg -> arg >>= fn) (return expr) fns


cons :: Expr -> Expr
cons (FnExpr fn) =
    FnExpr $ \expr -> fn expr >>= return . SeqExpr . (:[])
cons expr@(SeqExpr fns) =
    FnExpr $ \expr -> mapM (\(FnExpr fn) -> fn expr) fns >>= return . SeqExpr


cond :: Expr -> Expr
cond (SeqExpr [FnExpr fn1, FnExpr fn2]) =
    FnExpr $ \expr -> do
      expr' <- fn1 expr
      if isNotFalseExpr expr'
        then fn2 expr
        else signal $ SeqExpr [boxString "cond", boxString "1arm", expr]

cond (SeqExpr [FnExpr fn1, FnExpr fn2, FnExpr fn3]) =
    FnExpr $ \expr -> do
      expr' <- fn1 expr
      if isNotFalseExpr expr'
        then fn2 expr
        else fn3 expr

cond expr = signal $ SeqExpr [boxString "cond", boxString "arg", expr]


apply :: Expr -> InterpreterM Expr
apply (SeqExpr [FnExpr fn, expr]) = fn expr


-- K (see Prelude)


lift' :: Expr -> (Expr -> InterpreterM Expr)
lift' fn@(FnExpr _) =
    \vals -> return $ o $ SeqExpr [fn, cons vals]


lift :: Expr -> Expr
lift = f lift'


-- C (see Prelude)


-- raise (see Prelude)

-- raise :: Expr -> Expr
-- raise fn@(FnExpr fn') =
--     FnExpr hof
--     where hof expr | isNotFalseExpr (isfunc expr) = (lift' fn) expr
--           hof expr =
--               do b <- seqof' (m isfunc) expr
--                  if isNotFalseExpr b then (lift' fn) expr else fn' expr


-- edit: catch


-- edit: delay



-- predicate combining forms

pcons :: Expr -> Expr
pcons (SeqExpr fns) = FnExpr pconsHof
    where all' :: [Expr] -> [Expr] -> InterpreterM Bool
          all' [] [] = return True
          all' [] _ = return False
          all' _ [] = return False
          all' (FnExpr fn:fns') (val:vals') =
              do expr <- fn val
                 if isNotFalseExpr expr
                   then all' fns' vals'
                   else return False

          pconsHof (SeqExpr vals)
                   | length fns == length vals = BoolExpr <$> all' fns vals
                   | otherwise = return false
          pconsHof _ = return false


-- '=' (see Prelude)


seqof' :: Expr -> Expr -> InterpreterM Expr
seqof' (FnExpr fn) (SeqExpr exprs) =
    do b <- all isNotFalseExpr <$> mapM fn exprs
       return $ if b then true else false
seqof' (FnExpr fn) _ = return false


seqof :: Expr -> Expr
seqof = f seqof'


-- eqto (see Prelude)


lenis :: Expr -> Expr
lenis (IntExpr i) =
    FnExpr $ \(SeqExpr exprs) -> return $ BoolExpr $ i == length exprs


(&&) :: Expr -> Expr
(&&) (FnExpr fn) = FnExpr $ \expr -> fn expr
(&&) (SeqExpr exprs) =
    FnExpr $ \expr -> andHof expr exprs
    where andHof _ [] = return true
          andHof expr [FnExpr fn] = fn expr
          andHof expr (FnExpr fn:exprs) =
              do val <- fn expr
                 if isFalseExpr val
                   then return false
                   else andHof expr exprs


(||) :: Expr -> Expr
(||) (FnExpr fn) = FnExpr $ \expr -> fn expr
(||) (SeqExpr exprs) =
    FnExpr $ \expr -> orHof expr exprs
    where orHof _ [] = return false
          orHof expr [FnExpr fn] = fn expr
          orHof expr (FnExpr fn:exprs) =
              do val <- fn expr
                 if isFalseExpr val
                   then orHof expr exprs
                   else return true


(¬) :: Expr -> Expr
(¬) (FnExpr fn) = FnExpr $ \expr -> fn expr >>= return . not
(¬) expr = signal $ SeqExpr [boxString "¬", boxString "arg", expr]


rarrow :: Expr -> Expr
rarrow (SeqExpr [FnExpr fn1, FnExpr fn2]) =
    FnExpr rarrow'
    where rarrow' (SeqExpr (x:xs)) =
              do expr1 <- fn1 x
                 if isNotFalseExpr expr1
                   then do expr2 <- fn2 (SeqExpr xs)
                           if isNotFalseExpr expr2
                             then return true
                             else return false
                   else return false
          rarrow' _ = return false


(<=) :: Expr -> Expr
(<=) (SeqExpr [FnExpr fn1, FnExpr fn2]) =
    FnExpr larrow'
    where larrow' (SeqExpr xs) | Prelude.not (null xs) =
              do expr1 <- fn1 $ SeqExpr $ init xs
                 if isNotFalseExpr expr1
                   then do expr2 <- fn2 $ last xs
                           if isNotFalseExpr expr2
                             then return true
                             else return false
                   else return false
          larrow' _ = return false


-- pattern combining forms (see Lexer.x, Parser.y, ...)


-- sequence combining forms

-- sl (see Prelude)

-- sr (see Prelude)

-- edit: tree

alpha :: Expr -> Expr
alpha (FnExpr fn) =
    FnExpr alphaHof
    where alphaHof (SeqExpr exprs) = SeqExpr <$> mapM fn exprs
          alphaHof expr = fn expr

-- edit: merge


-- sequence function

al :: Expr -> Expr
al (SeqExpr [expr, SeqExpr exprs]) =
    SeqExpr $ expr:exprs


ar :: Expr -> Expr
ar (SeqExpr [SeqExpr exprs, expr]) =
    SeqExpr $ exprs ++ [expr]


cat :: Expr -> Expr
cat (SeqExpr exprs) =
    SeqExpr $ concatMap (\(SeqExpr exprs) -> exprs) exprs


distl :: Expr -> Expr
distl (SeqExpr [expr, SeqExpr exprs]) =
    SeqExpr $ map (\expr' -> SeqExpr [expr, expr']) exprs


distr :: Expr -> Expr
distr (SeqExpr [SeqExpr exprs, expr]) =
    SeqExpr $ map (\expr' -> SeqExpr [expr, expr']) exprs


-- edit: intsto


len :: Expr -> Expr
len (SeqExpr exprs) =
    IntExpr $ length exprs


reverse :: Expr -> Expr
reverse (SeqExpr exprs) =
    SeqExpr $ Prelude.reverse exprs


sel :: Expr -> Expr
sel (SeqExpr [IntExpr i, SeqExpr vals]) = vals !! (i - 1)


-- edit: trans

s :: Expr -> Expr
s (IntExpr i) = FnExpr $ \(SeqExpr vals) -> return $ vals !! (i - 1)


hd :: Expr -> Expr
hd (SeqExpr (expr:_)) = expr


tl :: Expr -> Expr
tl (SeqExpr (_:exprs)) = SeqExpr exprs


hdr :: Expr -> Expr
hdr (SeqExpr exprs@(_:_)) = last exprs


tlr :: Expr -> Expr
tlr (SeqExpr exprs@(_:_)) = SeqExpr $ init exprs


-- input, output, and file functions

out :: Expr -> Expr
{-# NOINLINE out #-}
out expr@(SeqExpr [SeqExpr [CharExpr 's', CharExpr 'c', CharExpr 'r'], SeqExpr str]) =
    unsafePerformIO $ do putStr $ map (\(CharExpr c) -> c) str
                         return expr
out expr = signal $ SeqExpr [boxString "out", boxString "arg1", expr]


-- edit: in, get, put


-- misc

id :: Expr -> Expr
id expr = expr


-- edit: delta


signal :: Expr -> a
signal expr = throwSignalException $ show expr


-- environment

f :: (Expr -> Expr -> InterpreterM Expr) -> Expr -> Expr
f fn = FnExpr . fn


m :: (Expr -> Expr) -> Expr
m fn = FnExpr $ \expr -> return $ fn expr


predT :: Type
predT = ArrowT DynT BoolT


predTs :: Type
predTs = ArrowT (SeqT DynT) BoolT


listPredT :: Type
listPredT = ArrowT (SeqT DynT) BoolT


syms :: [(String, Type, Expr)]
syms = [
  -- constants
  ("false", BoolT, false),
  ("true", BoolT, true),
  -- predicates
  --- predicates for numbers
  ("isint", predT, m isint),
  ("isreal", predT, m isreal),
  ("isnum", predT, m isnum),
  ("ispos", predT, m ispos),
  ("isneg", predT, m isneg),
  ("iszero", predT, m iszero),
  --- predicates for other atoms, functions, and user types
  ("isatom", predT, m isatom),
  ("isbool", predT, m isbool),
  ("ischar", predT, m ischar),
  ("isutype", predT, m isutype),
  ("isfunc", predT, m isfunc),
  --- predicates for sequences
  ("isnull", predT, m isnull),
  ("ispair", predT, m ispair),
  ("isseq", predT, m isseq),
  ("isstring", predT, m isstring),
  --- identically true and false predicates
  ("ff", predT, m ff),
  ("isval", predT, m isval),
  ("tt", predT, m tt),
  -- boolean and comparison functions
  --- boolean functions
  ("and", ArrowT (SeqT BoolT) BoolT, m and),
  ("not", ArrowT BoolT BoolT, m not),
  ("or", ArrowT (SeqT BoolT) BoolT, m or),
  --- comparison functions
  ("eq", predTs, m eq),
  ("less", predTs, m less),
  -- arithmetic functions
  ("add", ArrowT (SeqT DynT) DynT, m add),
  ("sub", ArrowT (SeqT DynT) DynT, m sub),
  ("mult", ArrowT (SeqT DynT) DynT, m mul),
  ("div", ArrowT (SeqT DynT) DynT, m div),
  ("floor", ArrowT DynT DynT, m floor),
  ("ceiling", ArrowT DynT DynT, m ceiling),
  ("abs", ArrowT DynT DynT, m abs),
  -- combining forms
  ("o", ArrowT (SeqT (ArrowT DynT DynT)) (ArrowT DynT DynT), m o),
  ("cons", (ForallT "a" (ArrowT (SeqT (ArrowT (TvarT "a") DynT)) (ArrowT (TvarT "a") DynT))), m cons),
  -- ("cons", (ForallT "a" (ForallT "b" (ArrowT (SeqT (ArrowT (TvarT "a") (TvarT "b"))) (ArrowT (TvarT "a") (TvarT "b"))))), m cons),
  ("ifthen", (ForallT "a" (ForallT "b" (ArrowT (TupT [ArrowT (TvarT "a") BoolT, ArrowT (TvarT "a") (TvarT "b")]) (ArrowT (TvarT "a") (TvarT "b"))))), m cond),
  ("ifelse", (ForallT "a"
              (ForallT "b"
               (ForallT "c"
                (ArrowT (TupT [ArrowT (TvarT "a") BoolT,
                               ArrowT (TvarT "a") (TvarT "b"),
                               ArrowT (TvarT "a") (TvarT "c")])
                 (ArrowT (TvarT "a") DynT))))), m cond),
  ("cond", ArrowT (SeqT (ArrowT DynT DynT)) (ArrowT DynT DynT), m cond),
  ("apply", ArrowT (TupT [ArrowT DynT DynT, DynT]) DynT, FnExpr apply),
  ("lift", ArrowT (ArrowT DynT DynT) (ArrowT DynT DynT), m lift),
  -- ("raise", m raise),
  -- predicate combining forms
  ("pcons", ArrowT (SeqT predT) predT, m pcons),
  ("seqof", ArrowT predT predT, m seqof),
  ("lenis", ArrowT IntT listPredT, m lenis),
  ("&&", ArrowT (SeqT predT) predT, m (&&)),
  ("||", ArrowT (SeqT predT) predT, m (||)),
  ("¬", ArrowT predT predT, m (¬)),
  ("=>", ArrowT (TupT [predT, predT]) listPredT, m rarrow),
  ("<=", ArrowT (TupT [predT, predT]) listPredT, m (<=)),
  ("map", ArrowT (ArrowT DynT DynT) (ArrowT (SeqT DynT) (SeqT DynT)), m alpha),
  ("al", ArrowT (SeqT DynT) (SeqT DynT), m al),
  ("ar", ArrowT (SeqT DynT) (SeqT DynT), m ar),
  ("cat", ArrowT (SeqT (SeqT DynT)) (SeqT DynT), m cat),
  ("distl", ArrowT (SeqT DynT) (SeqT DynT), m distl),
  ("distr", ArrowT (SeqT DynT) (SeqT DynT), m distr),
  ("len", ArrowT (SeqT DynT) IntT, m len),
  ("reverse", ArrowT (SeqT DynT) (SeqT DynT), m reverse),
  ("sel", ArrowT (SeqT DynT) DynT, m sel),
  ("s", ArrowT IntT (ArrowT (SeqT DynT) DynT), m s),
  ("hd", ForallT "a" (ArrowT (SeqT (TvarT "a")) (TvarT "a")), m hd),
  ("tl", ForallT "a" (ArrowT (SeqT (TvarT "a")) (SeqT (TvarT "a"))), m tl),
  ("hdr", ForallT "a" (ArrowT (SeqT (TvarT "a")) (TvarT "a")), m hdr),
  ("tlr", ForallT "a" (ArrowT (SeqT (TvarT "a")) (SeqT (TvarT "a"))), m tlr),
  -- io
  ("out", ArrowT (SeqT (SeqT CharT)) (SeqT (SeqT CharT)), m out),
  -- misc
  ("id", ForallT "a" (ArrowT (TvarT "a") (TvarT "a")), m id),
  ("signal", ForallT "a" (ArrowT (TvarT "a") (ForallT "b" (TvarT "b"))), m signal),
  ("K", ForallT "a" (ArrowT (TvarT "a") (ForallT "b" (ArrowT (TvarT "b") (TvarT "a")))), m k)]


k :: Expr -> Expr
k expr = FnExpr $ \_ -> return expr


srcfile :: SrcFile
srcfile = SrcFile "Core" [] Nothing $ Right $
          Map.fromList $ map (\(a, b, c) -> (a, (b, c))) syms