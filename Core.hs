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
isSeq (FnVal fn) = FnVal hof
  where hof (SeqVal vals) =
            do b <- all isNotFalseVal <$> mapM fn vals
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
valLt :: Val -> Val -> Val
valLt (IntVal b1) (IntVal b2) | b1 < b2 = true
valLt (IntVal i1) (IntVal i2) | i1 < i2 = true
valLt (DoubleVal d1) (DoubleVal d2) | d1 < d2 = true
valLt (CharVal c1) (CharVal c2) | c1 < c2 = true
valLt (SeqVal vals1) (SeqVal vals2)
    | null vals1 && null vals2 = false
    | length vals1 < length vals2 = true
    | length vals1 == length vals2 = all2 valLt vals1 vals2
valLt _ _ = false


-- edit: improve efficiency
ltSeq :: Val -> Val
ltSeq (SeqVal vals1) = FnVal ltSeqHof
    where ltSeqHof (SeqVal vals2)
              | null vals1 && null vals2 = return false
              | length vals1 < length vals2 = return true
              | length vals1 == length vals2 =
                  return (all2 valLt vals1 vals2)
              | otherwise = return false


-- arithmetic functions

mkBool :: Val -> Val
mkBool val@BoolVal {} = val


mkInt :: Val -> Val
mkInt val@IntVal {} = val
mkInt (DoubleVal d) = IntVal (floor d)


mkReal :: Val -> Val
mkReal (IntVal i) = DoubleVal (fromIntegral i)
mkReal val@DoubleVal {} = val


mkChar :: Val -> Val
mkChar val@CharVal {} = val


mkSeq :: Val -> Val
mkSeq (FnVal fn) = FnVal hof
  where hof (SeqVal vals) = SeqVal <$> mapM fn vals


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
apply (SeqVal [FnVal fn, val]) = fn val


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
          all' (FnVal fn:fns) (val:vals) =
              do val' <- fn val
                 if isNotFalseVal val'
                   then all' fns vals
                   else return False

          plistHof (SeqVal vals)
                   | length fns == length vals = BoolVal <$> all' fns vals
                   | otherwise = return false
          plistHof _ = return false


pand :: Val -> Val
pand val@FnVal {} = val
pand (SeqVal vals) =
    FnVal $ \val -> andHof val vals
    where andHof _ [] = return true
          andHof val [FnVal fn] = fn val
          andHof val (FnVal fn:vals) =
              do val' <- fn val
                 if isFalseVal val'
                   then return false
                   else andHof val vals


por :: Val -> Val
por val@FnVal {} = val
por (SeqVal vals) =
    FnVal $ \val -> orHof val vals
    where orHof _ [] = return false
          orHof val [FnVal fn] = fn val
          orHof val (FnVal fn:vals) =
              do val' <- fn val
                 if isFalseVal val'
                   then orHof val vals
                   else return true


pal :: Val -> Val
pal (SeqVal [FnVal fn1, FnVal fn2]) =
    FnVal pal'
    where pal' (SeqVal (x:xs)) =
              do val1 <- fn1 x
                 if isNotFalseVal val1
                   then do val2 <- fn2 (SeqVal xs)
                           return (if isNotFalseVal val2
                                   then true
                                   else false)
                   else return false
          pal' _ = return false


par :: Val -> Val
par (SeqVal [FnVal fn1, FnVal fn2]) =
    FnVal par'
    where par' (SeqVal xs) | not (null xs) =
              do val1 <- fn1 $ SeqVal $ init xs
                 if isNotFalseVal val1
                   then do val2 <- fn2 $ last xs
                           return (if isNotFalseVal val2
                                   then true
                                   else false)
                   else return false
          par' _ = return false


al :: Val -> Val
al val =
    FnVal $ \(SeqVal vals) -> return $ SeqVal (val:vals)


ar :: Val -> Val
ar (SeqVal vals) =
    FnVal $ \val -> return $ SeqVal (vals ++ [val])


concat :: Val -> Val
concat (SeqVal vals) =
    SeqVal $ concatMap (\(SeqVal vals) -> vals) vals


hd :: Val -> Val
hd (SeqVal (val:_)) = val


tl :: Val -> Val
tl (SeqVal (_:vals)) = SeqVal vals


hdr :: Val -> Val
hdr (SeqVal vals@(_:_)) = last vals


tlr :: Val -> Val
tlr (SeqVal vals@(_:_)) = SeqVal $ init vals


-- input, output, and file functions

out :: Val -> Val
{-# NOINLINE out #-}
out val@(SeqVal [SeqVal [CharVal 's', CharVal 'c', CharVal 'r'], SeqVal str]) =
    unsafePerformIO $ do putStr $ map (\(CharVal c) -> c) str
                         return val
out val = signal $ SeqVal [boxString "out", boxString "arg1", val]


signal :: Val -> a
signal = throwSignalException . show


-- environment

m :: (Val -> Val) -> Val
m fn = FnVal $ \val -> return $ fn val


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
  ("isSeq", ArrowT predT predT, m isSeq),
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
  ("mkBool", ArrowT DynT BoolT, m mkBool),
  ("mkInt", ArrowT DynT IntT, m mkInt),
  ("mkReal", ArrowT DynT DoubleT, m mkReal),
  ("mkChar", ArrowT DynT CharT, m mkChar),

  ("mkSeq", ForallT "a"
            (ArrowT
             (ArrowT DynT (VarT "a"))
             (ArrowT DynT (SeqT (VarT "a")))), m mkSeq),

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
           (ArrowT (ArrowT (VarT "b") (VarT "c"))
            (ArrowT (ArrowT (VarT "a") (VarT "b"))
             (ArrowT (VarT "a") (VarT "c")))))), m o),
  -- predicate combining forms
  ("plist", ArrowT (SeqT predT) predT, m plist),
  ("pand", ArrowT (SeqT predT) predT, m pand),
  ("por", ArrowT (SeqT predT) predT, m por),
  ("pal", ArrowT (TupT [predT, predT]) listPredT, m pal),
  ("par", ArrowT (TupT [predT, predT]) listPredT, m par),
  ("al", ForallT "a" (ArrowT (VarT "a") (ArrowT (SeqT (VarT "a")) (SeqT (VarT "a")))), m al),
  ("ar", ForallT "a" (ArrowT (SeqT (VarT "a")) (ArrowT (VarT "a") (SeqT (VarT "a")))), m ar),
  ("concat", ArrowT (SeqT (SeqT DynT)) (SeqT DynT), m concat),
  ("hd", ForallT "a" (ArrowT (SeqT (VarT "a")) (VarT "a")), m hd),
  ("tl", ForallT "a" (ArrowT (SeqT (VarT "a")) (SeqT (VarT "a"))), m tl),
  ("hdr", ForallT "a" (ArrowT (SeqT (VarT "a")) (VarT "a")), m hdr),
  ("tlr", ForallT "a" (ArrowT (SeqT (VarT "a")) (SeqT (VarT "a"))), m tlr),
  -- io
  ("out", ArrowT (SeqT (SeqT CharT)) (SeqT (SeqT CharT)), m out),
  -- misc
  ("signal", ForallT "a" (ArrowT (VarT "a") (ForallT "b" (VarT "b"))), m signal),

  ("un#", ForallT "a" (ForallT "b" (ArrowT (VarT "a") (VarT "b"))), m unSharp),

  ("fix#", fixSharpT, m fixSharp),

  ("null#", nullSharpT, nullSharp),

  -- and types
  ("cast#", ForallT "a"
            (ForallT "b"
             (ArrowT (VarT "a") (VarT "b"))), m castSharp),

  ("isAnd", ArrowT (SeqT predT) predT, m isAnd),

  ("mkAnd", ForallT "a"
            (ForallT "b"
             (ArrowT (VarT "a")
              (ArrowT (VarT "b")
               (AndT (VarT "a") (VarT "b"))))), m mkAnd),

  ("index", ArrowT IntT (ArrowT DynT DynT), m index)]


unSharp :: Val -> Val
unSharp (TypeVal val) = val


-- | fixSharp : ((a -> b) -> a -> b) -> a -> b
fixSharpT =
  ForallT "a"
   (ForallT "b"
    (ArrowT
     (ArrowT
      (ArrowT (VarT "a") (VarT "b"))
      (ArrowT (VarT "a") (VarT "b")))
      (ArrowT (VarT "a") (VarT "b"))))
fixSharp :: Val -> Val
fixSharp (FnVal fn) = FnVal hof
  where hof val =
          do FnVal fn' <- fn (FnVal hof)
             fn' val


nullSharpT :: Type
nullSharpT = ForallT "a" (SeqT (VarT "a"))

nullSharp :: Val
nullSharp = SeqVal []


index :: Val -> Val
index (IntVal i) = FnVal (return . hof)
    where hof (SeqVal vals) = vals !! i


castSharp :: Val -> Val
castSharp = id

-- and types

mkAnd :: Val -> Val
mkAnd val1 = FnVal (return . hof)
  where hof (TypeVal (SeqVal vals)) =
          TypeVal $ SeqVal $ val1:vals
        hof val2 =
          TypeVal $ SeqVal $ [val1, val2]

isAnd :: Val -> Val
isAnd (SeqVal [FnVal hdFn, FnVal tlFn]) = FnVal hof
  where hof (TypeVal (SeqVal (val:vals))) =
          do val' <- hdFn val
             if isFalseVal val'
             then return false
             else tlFn (TypeVal (SeqVal vals))

srcfile :: SrcFile
srcfile = mkCoreSrcFile "Core" [] typeDesc fnDesc
