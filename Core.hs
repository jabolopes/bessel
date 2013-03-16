module Core where

import Prelude hiding ((&&), (||), id, compare, reverse, read)
import qualified Prelude

import Data.Functor ((<$>))
import Data.Map (Map)
import qualified Data.Map as Map (fromList)
import System.IO hiding (hGetContents)
import System.IO.Strict
import System.IO.Unsafe

import Data.Env (Env)
import qualified Data.Env as Env (fromList)
import Data.Exception
import Data.SrcFile
import qualified Data.SrcFile as SrcFile (initial)
import Data.Symbol
import Data.Type
import Monad.InterpreterM
import Renamer


-- predicates

isint :: Expr -> Expr
isint (IntExpr _) = true
isint _ = false


isreal :: Expr -> Expr
isreal (DoubleExpr _) = true
isreal _ = false

isbool :: Expr -> Expr
isbool (BoolExpr _) = true
isbool _ = false


ischar :: Expr -> Expr
ischar (CharExpr _) = true
ischar _ = false


isfn :: Expr -> Expr
isfn (FnExpr _) = true
isfn _ = false


isobj :: Expr -> Expr
isobj (TypeExpr _ _ _) = true
isobj _ = false


isseq :: Expr -> Expr
isseq (SeqExpr _) = true
isseq _ = false


isseqof :: Expr -> Expr
isseqof (FnExpr fn) = FnExpr hof
    where hof (SeqExpr exprs) =
              do b <- all isNotFalseExpr <$> mapM fn exprs
                 return $ if b then true else false
          hof _ = return false


-- comparison functions

eq :: Expr -> Expr
eq expr1 =
    FnExpr $ \expr2 -> return (expr1 `exprEq` expr2)


less :: Expr -> Expr
less expr1 =
    FnExpr $ \expr2 -> return (expr1 `exprLt` expr2)


-- arithmetic functions

mkInt :: Expr -> Expr
mkInt expr@(IntExpr _) = expr
mkInt (DoubleExpr d) = IntExpr (Prelude.floor d)


mkReal :: Expr -> Expr
mkReal (IntExpr i) = DoubleExpr (fromIntegral i)
mkReal expr@(DoubleExpr _) = expr


addInt :: Expr -> Expr
addInt (IntExpr i1) =
    FnExpr $ \(IntExpr i2) -> return $ IntExpr (i1 + i2)


addReal :: Expr -> Expr
addReal (DoubleExpr i1) =
    FnExpr $ \(DoubleExpr i2) -> return $ DoubleExpr (i1 + i2)


subInt :: Expr -> Expr
subInt (IntExpr i1) =
    FnExpr $ \(IntExpr i2) -> return $ IntExpr (i1 - i2)


subReal :: Expr -> Expr
subReal (DoubleExpr d1) =
    FnExpr $ \(DoubleExpr d2) -> return $ DoubleExpr (d1 - d2)


mulInt :: Expr -> Expr
mulInt (IntExpr i1) =
    FnExpr $ \(IntExpr i2) -> return $ IntExpr (i1 * i2)


mulReal :: Expr -> Expr
mulReal (DoubleExpr d1) =
    FnExpr $ \(DoubleExpr d2) -> return $ DoubleExpr (d1 * d2)


divInt :: Expr -> Expr
divInt (IntExpr i1) =
    FnExpr $ \(IntExpr i2) -> return $ IntExpr (i1 `div` i2)


divReal :: Expr -> Expr
divReal (DoubleExpr d1) =
    FnExpr $ \(DoubleExpr d2) -> return $ DoubleExpr (d1 / d2)


absInt :: Expr -> Expr
absInt (IntExpr i) = IntExpr (Prelude.abs i)


absReal :: Expr -> Expr
absReal (DoubleExpr d) = DoubleExpr (abs d)


ceilingReal :: Expr -> Expr
ceilingReal (DoubleExpr d) = IntExpr (ceiling d)


floorReal :: Expr -> Expr
floorReal (DoubleExpr d) = IntExpr (floor d)


negInt :: Expr -> Expr
negInt (IntExpr i) = IntExpr (- i)


negReal :: Expr -> Expr
negReal (DoubleExpr d) = DoubleExpr (- d)


-- combining forms

apply :: Expr -> InterpreterM Expr
apply (SeqExpr [FnExpr fn, expr]) = fn expr


o :: Expr -> Expr
o (FnExpr fn1) =
    FnExpr $ \(FnExpr fn2) ->
        return $ FnExpr $ \expr -> fn1 =<< fn2 expr


-- K (see Prelude)


-- C (see Prelude)


-- raise (see Prelude)


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


larrow :: Expr -> Expr
larrow (SeqExpr [FnExpr fn1, FnExpr fn2]) =
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
id = Prelude.id


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


desc :: [(String, Type, Expr)]
desc = [
  -- constants
  ("false", BoolT, false),
  ("true", BoolT, true),
  -- predicates
  ("isint", predT, m isint),
  ("isreal", predT, m isreal),
  ("isbool", predT, m isbool),
  ("ischar", predT, m ischar),
  ("isfn", predT, m isfn),
  ("isobj", predT, m isobj),
  ("isseq", predT, m isseq),
  ("isseqof", ArrowT predT DynT, m isseqof),
  -- comparison functions
  ("eq", ArrowT DynT (ArrowT DynT BoolT), m eq),
  ("less", ArrowT DynT (ArrowT DynT BoolT), m less),
  -- arithmetic functions
  ("mkInt", ArrowT DynT IntT, m mkInt),
  ("mkReal", ArrowT DynT DoubleT, m mkReal),
  ("addInt", ArrowT IntT (ArrowT IntT IntT), m addInt),
  ("addReal", ArrowT DoubleT (ArrowT DoubleT DoubleT), m addReal),
  ("subInt", ArrowT IntT (ArrowT IntT IntT), m subInt),
  ("subReal", ArrowT DoubleT (ArrowT DoubleT DoubleT), m subReal),
  ("mulInt", ArrowT IntT (ArrowT IntT IntT), m mulInt),
  ("mulReal", ArrowT DoubleT (ArrowT DoubleT DoubleT), m mulReal),
  ("divInt", ArrowT IntT (ArrowT IntT IntT), m divInt),
  ("divReal", ArrowT DoubleT (ArrowT DoubleT DoubleT), m divReal),
  ("absInt", ArrowT IntT IntT, m absInt),
  ("absReal", ArrowT DoubleT DoubleT, m absReal),
  ("ceilingReal", ArrowT DoubleT IntT, m ceilingReal),
  ("floorReal", ArrowT DoubleT IntT, m floorReal),
  ("negInt", ArrowT IntT (ArrowT IntT IntT), m negInt),
  ("negReal", ArrowT DoubleT (ArrowT DoubleT DoubleT), m negReal),
  -- combining forms
  ("apply", ArrowT (TupT [ArrowT DynT DynT, DynT]) DynT, FnExpr apply),
  ("o", ForallT "a"
         (ForallT "b"
          (ForallT "c"
           (ArrowT (ArrowT (TvarT "b") (TvarT "c"))
            (ArrowT (ArrowT (TvarT "a") (TvarT "b"))
             (ArrowT (TvarT "a") (TvarT "c")))))), m o),
  -- predicate combining forms
  ("pcons", ArrowT (SeqT predT) predT, m pcons),
  ("lenis", ArrowT IntT listPredT, m lenis),
  ("&&", ArrowT (SeqT predT) predT, m (&&)),
  ("||", ArrowT (SeqT predT) predT, m (||)),
  ("->", ArrowT (TupT [predT, predT]) listPredT, m rarrow),
  ("<-", ArrowT (TupT [predT, predT]) listPredT, m larrow),
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
  ("signal", ForallT "a" (ArrowT (TvarT "a") (ForallT "b" (TvarT "b"))), m signal)]


-- edit: fixed undefined
srcfile :: SrcFile
srcfile = mkCoreSrcFile "Core" [] desc
