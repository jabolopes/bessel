module Core where

import Prelude hiding (concat, reverse)
import qualified Prelude

import Control.Monad ((<=<))
import Data.Functor ((<$>))
import Data.Map (Map)
import qualified Data.Map as Map (fromList)
import System.IO hiding (hGetContents)
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

isBool :: Val -> Val
isBool BoolVal {} = true
isBool _ = false


isInt :: Val -> Val
isInt IntVal {} = true
isInt _ = false


isReal :: Val -> Val
isReal DoubleVal {} = true
isReal _ = false


isChar :: Val -> Val
isChar CharVal {} = true
isChar _ = false


isFn :: Val -> Val
isFn FnVal {} = true
isFn _ = false


isObj :: Val -> Val
isObj TypeVal {} = true
isObj _ = false


isSeq :: Val -> Val
isSeq SeqVal {} = true
isSeq _ = false


isSeqOf :: Val -> Val
isSeqOf (FnVal fn) = FnVal hof
    where hof (SeqVal exprs) =
              do b <- all isNotFalseVal <$> mapM fn exprs
                 return $ if b then true else false
          hof _ = return false


-- comparison functions

eqBool :: Val -> Val
eqBool (BoolVal b1) = FnVal eqBoolHof
    where eqBoolHof (BoolVal b2)
              | b1 == b2 = return true
              | otherwise = return false


eqInt :: Val -> Val
eqInt (IntVal i1) = FnVal eqIntHof
    where eqIntHof (IntVal i2)
              | i1 == i2 = return true
              | otherwise = return false


eqReal :: Val -> Val
eqReal (DoubleVal d1) = FnVal eqRealHof
    where eqRealHof (DoubleVal d2)
              | d1 == d2 = return true
              | otherwise = return false


eqChar :: Val -> Val
eqChar (CharVal c1) = FnVal eqCharHof
    where eqCharHof (CharVal c2)
              | c1 == c2 = return true
              | otherwise = return false


all2 :: (a -> b -> Val) -> [a] -> [b] -> Val
all2 _ [] [] = true
all2 _ [] _ = false
all2 _ _ [] = false
all2 fn (x1:xs1) (x2:xs2)
    | isNotFalseVal (fn x1 x2) = all2 fn xs1 xs2
    | otherwise = false


ltBool :: Val -> Val
ltBool (BoolVal b1) = FnVal ltBoolHof
    where ltBoolHof (BoolVal b2)
              | b1 < b2 = return true
              | otherwise = return false


ltInt :: Val -> Val
ltInt (IntVal i1) = FnVal ltIntHof
    where ltIntHof (IntVal i2)
              | i1 < i2 = return true
              | otherwise = return false


ltReal :: Val -> Val
ltReal (DoubleVal d1) = FnVal ltRealHof
    where ltRealHof (DoubleVal d2)
              | d1 < d2 = return true
              | otherwise = return false


ltChar :: Val -> Val
ltChar (CharVal c1) = FnVal ltCharHof
    where ltCharHof (CharVal c2)
              | c1 < c2 = return true
              | otherwise = return false


-- edit: to eliminate
exprLt :: Val -> Val -> Val
exprLt (IntVal b1) (IntVal b2) | b1 < b2 = true
exprLt (IntVal i1) (IntVal i2) | i1 < i2 = true
exprLt (DoubleVal d1) (DoubleVal d2) | d1 < d2 = true
exprLt (CharVal c1) (CharVal c2) | c1 < c2 = true
exprLt (SeqVal exprs1) (SeqVal exprs2)
    | null exprs1 && null exprs2 = false
    | length exprs1 < length exprs2 = true
    | length exprs1 == length exprs2 = all2 exprLt exprs1 exprs2
exprLt _ _ = false


-- edit: improve efficiency
ltSeq :: Val -> Val
ltSeq (SeqVal exprs1) = FnVal ltSeqHof
    where ltSeqHof (SeqVal exprs2)
              | null exprs1 && null exprs2 = return false
              | length exprs1 < length exprs2 = return true
              | length exprs1 == length exprs2 =
                  return (all2 exprLt exprs1 exprs2)
              | otherwise = return false


-- arithmetic functions

mkInt :: Val -> Val
mkInt expr@(IntVal _) = expr
mkInt (DoubleVal d) = IntVal (floor d)


mkReal :: Val -> Val
mkReal (IntVal i) = DoubleVal (fromIntegral i)
mkReal expr@(DoubleVal _) = expr


addInt :: Val -> Val
addInt (IntVal i1) =
    FnVal $ \(IntVal i2) -> return $ IntVal (i1 + i2)


addReal :: Val -> Val
addReal (DoubleVal i1) =
    FnVal $ \(DoubleVal i2) -> return $ DoubleVal (i1 + i2)


subInt :: Val -> Val
subInt (IntVal i1) =
    FnVal $ \(IntVal i2) -> return $ IntVal (i1 - i2)


subReal :: Val -> Val
subReal (DoubleVal d1) =
    FnVal $ \(DoubleVal d2) -> return $ DoubleVal (d1 - d2)


mulInt :: Val -> Val
mulInt (IntVal i1) =
    FnVal $ \(IntVal i2) -> return $ IntVal (i1 * i2)


mulReal :: Val -> Val
mulReal (DoubleVal d1) =
    FnVal $ \(DoubleVal d2) -> return $ DoubleVal (d1 * d2)


divInt :: Val -> Val
divInt (IntVal i1) =
    FnVal $ \(IntVal i2) -> return $ IntVal (i1 `div` i2)


divReal :: Val -> Val
divReal (DoubleVal d1) =
    FnVal $ \(DoubleVal d2) -> return $ DoubleVal (d1 / d2)


absInt :: Val -> Val
absInt (IntVal i) = IntVal (abs i)


absReal :: Val -> Val
absReal (DoubleVal d) = DoubleVal (abs d)


ceilingReal :: Val -> Val
ceilingReal (DoubleVal d) = IntVal (ceiling d)


floorReal :: Val -> Val
floorReal (DoubleVal d) = IntVal (floor d)


negInt :: Val -> Val
negInt (IntVal i) = IntVal (- i)


negReal :: Val -> Val
negReal (DoubleVal d) = DoubleVal (- d)


remInt :: Val -> Val
remInt (IntVal i1) =
    FnVal $ \(IntVal i2) -> return $ IntVal (i1 `rem` i2)


-- combining forms

apply :: Val -> InterpreterM Val
apply (SeqVal [FnVal fn, expr]) = fn expr


o :: Val -> Val
o (FnVal fn1) =
    FnVal $ \(FnVal fn2) -> return $ FnVal (fn1 <=< fn2)


-- K (see Prelude)


-- C (see Prelude)


-- raise (see Prelude)


-- edit: catch


-- edit: delay



-- predicate combining forms

plist :: Val -> Val
plist (SeqVal fns) = FnVal plistHof
    where all' :: [Val] -> [Val] -> InterpreterM Bool
          all' [] [] = return True
          all' [] _ = return False
          all' _ [] = return False
          all' (FnVal fn:fns') (val:vals') =
              do expr <- fn val
                 if isNotFalseVal expr
                   then all' fns' vals'
                   else return False

          plistHof (SeqVal vals)
                   | length fns == length vals = BoolVal <$> all' fns vals
                   | otherwise = return false
          plistHof _ = return false


pand :: Val -> Val
pand (FnVal fn) = FnVal $ \expr -> fn expr
pand (SeqVal exprs) =
    FnVal $ \expr -> andHof expr exprs
    where andHof _ [] = return true
          andHof expr [FnVal fn] = fn expr
          andHof expr (FnVal fn:exprs) =
              do val <- fn expr
                 if isFalseVal val
                   then return false
                   else andHof expr exprs


por :: Val -> Val
por (FnVal fn) = FnVal $ \expr -> fn expr
por (SeqVal exprs) =
    FnVal $ \expr -> orHof expr exprs
    where orHof _ [] = return false
          orHof expr [FnVal fn] = fn expr
          orHof expr (FnVal fn:exprs) =
              do val <- fn expr
                 if isFalseVal val
                   then orHof expr exprs
                   else return true


pal :: Val -> Val
pal (SeqVal [FnVal fn1, FnVal fn2]) =
    FnVal pal'
    where pal' (SeqVal (x:xs)) =
              do expr1 <- fn1 x
                 if isNotFalseVal expr1
                   then do expr2 <- fn2 (SeqVal xs)
                           return (if isNotFalseVal expr2
                                   then true
                                   else false)
                   else return false
          pal' _ = return false


par :: Val -> Val
par (SeqVal [FnVal fn1, FnVal fn2]) =
    FnVal par'
    where par' (SeqVal xs) | not (null xs) =
              do expr1 <- fn1 $ SeqVal $ init xs
                 if isNotFalseVal expr1
                   then do expr2 <- fn2 $ last xs
                           return (if isNotFalseVal expr2
                                   then true
                                   else false)
                   else return false
          par' _ = return false


al :: Val -> Val
al expr =
    FnVal $ \(SeqVal exprs) -> return $ SeqVal (expr:exprs)


ar :: Val -> Val
ar (SeqVal exprs) =
    FnVal $ \expr -> return $ SeqVal (exprs ++ [expr])


concat :: Val -> Val
concat (SeqVal exprs) =
    SeqVal $ concatMap (\(SeqVal exprs) -> exprs) exprs


hd :: Val -> Val
hd (SeqVal (expr:_)) = expr


tl :: Val -> Val
tl (SeqVal (_:exprs)) = SeqVal exprs


hdr :: Val -> Val
hdr (SeqVal exprs@(_:_)) = last exprs


tlr :: Val -> Val
tlr (SeqVal exprs@(_:_)) = SeqVal $ init exprs


-- input, output, and file functions

out :: Val -> Val
{-# NOINLINE out #-}
out expr@(SeqVal [SeqVal [CharVal 's', CharVal 'c', CharVal 'r'], SeqVal str]) =
    unsafePerformIO $ do putStr $ map (\(CharVal c) -> c) str
                         return expr
out expr = signal $ SeqVal [boxString "out", boxString "arg1", expr]


signal :: Val -> a
signal expr = throwSignalException $ show expr


-- environment

m :: (Val -> Val) -> Val
m fn = FnVal $ \expr -> return $ fn expr


predT :: Type
predT = ArrowT DynT BoolT


listPredT :: Type
listPredT = ArrowT (SeqT DynT) BoolT


typeDesc :: TypeDesc
typeDesc =
    [("Bool", BoolT),
     ("Int", IntT),
     ("Double", DoubleT),
     ("Char", CharT),
     ("Dyn", DynT)]


fnDesc :: FnDesc
fnDesc = [
  -- constants
  ("false", BoolT, false),
  ("true", BoolT, true),
  -- predicates
  ("isBool", predT, m isBool),
  ("isInt", predT, m isInt),
  ("isReal", predT, m isReal),
  ("isChar", predT, m isChar),
  ("isFn", predT, m isFn),
  ("isObj", predT, m isObj),
  ("isSeq", predT, m isSeq),
  ("isSeqOf", ArrowT predT predT, m isSeqOf),
  -- comparison functions
  ("eqBool", ArrowT BoolT (ArrowT BoolT BoolT), m eqBool),
  ("eqInt", ArrowT IntT (ArrowT IntT BoolT), m eqInt),
  ("eqReal", ArrowT DoubleT (ArrowT DoubleT BoolT), m eqReal),
  ("eqChar", ArrowT CharT (ArrowT CharT BoolT), m eqChar),
  ("ltBool", ArrowT BoolT (ArrowT BoolT BoolT), m ltBool),
  ("ltInt", ArrowT IntT (ArrowT IntT BoolT), m ltInt),
  ("ltReal", ArrowT DoubleT (ArrowT DoubleT BoolT), m ltReal),
  ("ltChar", ArrowT CharT (ArrowT CharT BoolT), m ltChar),
  ("ltSeq", ArrowT DynT (ArrowT DynT BoolT), m ltSeq),
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
  ("negInt", ArrowT IntT IntT, m negInt),
  ("negReal", ArrowT DoubleT DoubleT, m negReal),
  ("remInt", ArrowT IntT (ArrowT IntT IntT), m remInt),
  -- combining forms
  ("apply", ArrowT (TupT [ArrowT DynT DynT, DynT]) DynT, FnVal apply),
  ("o", ForallT "a"
         (ForallT "b"
          (ForallT "c"
           (ArrowT (ArrowT (TvarT "b") (TvarT "c"))
            (ArrowT (ArrowT (TvarT "a") (TvarT "b"))
             (ArrowT (TvarT "a") (TvarT "c")))))), m o),
  -- predicate combining forms
  ("plist", ArrowT (SeqT predT) predT, m plist),
  ("pand", ArrowT (SeqT predT) predT, m pand),
  ("por", ArrowT (SeqT predT) predT, m por),
  ("pal", ArrowT (TupT [predT, predT]) listPredT, m pal),
  ("par", ArrowT (TupT [predT, predT]) listPredT, m par),
  ("al", ForallT "a" (ArrowT (TvarT "a") (ArrowT (SeqT (TvarT "a")) (SeqT (TvarT "a")))), m al),
  ("ar", ForallT "a" (ArrowT (SeqT (TvarT "a")) (ArrowT (TvarT "a") (SeqT (TvarT "a")))), m ar),
  ("concat", ArrowT (SeqT (SeqT DynT)) (SeqT DynT), m concat),
  ("hd", ForallT "a" (ArrowT (SeqT (TvarT "a")) (TvarT "a")), m hd),
  ("tl", ForallT "a" (ArrowT (SeqT (TvarT "a")) (SeqT (TvarT "a"))), m tl),
  ("hdr", ForallT "a" (ArrowT (SeqT (TvarT "a")) (TvarT "a")), m hdr),
  ("tlr", ForallT "a" (ArrowT (SeqT (TvarT "a")) (SeqT (TvarT "a"))), m tlr),
  -- io
  ("out", ArrowT (SeqT (SeqT CharT)) (SeqT (SeqT CharT)), m out),
  -- misc
  ("signal", ForallT "a" (ArrowT (TvarT "a") (ForallT "b" (TvarT "b"))), m signal),

  ("un#", ArrowT DynT DynT, m unSharp),

  ("index", ArrowT IntT (ArrowT DynT DynT), m index)]


unSharp :: Val -> Val
unSharp (TypeVal expr) = expr


index :: Val -> Val
index (IntVal i) = FnVal (return . hof)
    where hof (SeqVal exprs) = exprs !! i


srcfile :: SrcFile
srcfile = mkCoreSrcFile "Core" [] typeDesc fnDesc
