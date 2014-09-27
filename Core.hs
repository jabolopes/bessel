module Core where

import Prelude hiding (concat, null, reverse)
import qualified Prelude

import Control.Arrow ((***))
import Control.Monad ((<=<))
import Data.Hashable (hash)

import Config
import Data.Exception
import Data.Module
import Monad.InterpreterM

-- Bool

isBool :: Val -> Val
isBool BoolVal {} = true
isBool _ = false

eqBool :: Val -> Val
eqBool (BoolVal b1) = FnVal $ return . eqBoolHof
  where
    eqBoolHof (BoolVal b2)
      | b1 == b2 = true
      | otherwise = false

-- Int

isInt :: Val -> Val
isInt IntVal {} = true
isInt _ = false

eqInt :: Val -> Val
eqInt (IntVal i1) = FnVal $ return . eqIntHof
  where
    eqIntHof (IntVal i2)
      | i1 == i2 = true
      | otherwise = false

ltInt :: Val -> Val
ltInt (IntVal i1) = FnVal $ return . ltIntHof
  where ltIntHof (IntVal i2)
          | i1 < i2 = true
          | otherwise = false

addInt :: Val -> Val
addInt (IntVal i1) =
  FnVal $ \(IntVal i2) -> return . IntVal $ i1 + i2

subInt :: Val -> Val
subInt (IntVal i1) =
  FnVal $ \(IntVal i2) -> return . IntVal $ i1 - i2

mulInt :: Val -> Val
mulInt (IntVal i1) =
  FnVal $ \(IntVal i2) -> return . IntVal $ i1 * i2

divInt :: Val -> Val
divInt (IntVal i1) =
  FnVal $ \(IntVal i2) -> return . IntVal $ i1 `div` i2

absInt :: Val -> Val
absInt (IntVal i) = IntVal (abs i)

negInt :: Val -> Val
negInt (IntVal i) = IntVal (- i)

invInt :: Val -> Val
invInt (IntVal i) = RealVal (1.0 / fromIntegral i)

remInt :: Val -> Val
remInt (IntVal i1) =
  FnVal $ \(IntVal i2) -> return . IntVal $ i1 `rem` i2

-- Real

isReal :: Val -> Val
isReal RealVal {} = true
isReal _ = false

eqReal :: Val -> Val
eqReal (RealVal d1) = FnVal $ return . eqRealHof
  where
    eqRealHof (RealVal d2)
      | d1 == d2 = true
      | otherwise = false

ltReal :: Val -> Val
ltReal (RealVal d1) = FnVal $ return . ltRealHof
  where
    ltRealHof (RealVal d2)
      | d1 < d2 = true
      | otherwise = false

addReal :: Val -> Val
addReal (RealVal i1) =
  FnVal $ \(RealVal i2) -> return .  RealVal $ i1 + i2

subReal :: Val -> Val
subReal (RealVal d1) =
  FnVal $ \(RealVal d2) -> return . RealVal $ d1 - d2

mulReal :: Val -> Val
mulReal (RealVal d1) =
  FnVal $ \(RealVal d2) -> return . RealVal $ d1 * d2

divReal :: Val -> Val
divReal (RealVal d1) =
  FnVal $ \(RealVal d2) -> return . RealVal $ d1 / d2

absReal :: Val -> Val
absReal (RealVal d) = RealVal (abs d)

ceilingReal :: Val -> Val
ceilingReal (RealVal d) = IntVal (ceiling d)

floorReal :: Val -> Val
floorReal (RealVal d) = IntVal (floor d)

negReal :: Val -> Val
negReal (RealVal d) = RealVal (- d)

invReal :: Val -> Val
invReal (RealVal d) = RealVal (1 / d)

-- Int and Real

ltIntReal :: Val -> Val
ltIntReal (IntVal i) = FnVal $ return . ltIntRealHof
  where
    ltIntRealHof (RealVal d)
      | fromIntegral i < d = true
      | otherwise = false

ltRealInt :: Val -> Val
ltRealInt (RealVal d) = FnVal $ return . ltRealIntHof
  where
    ltRealIntHof (IntVal i)
      | d < fromIntegral i = true
      | otherwise = false

addIntReal :: Val -> Val
addIntReal (IntVal i) = addReal (RealVal (fromIntegral i))

mulIntReal :: Val -> Val
mulIntReal (IntVal i) = mulReal (RealVal (fromIntegral i))

-- Char

isChar :: Val -> Val
isChar CharVal {} = true
isChar _ = false

eqChar :: Val -> Val
eqChar (CharVal c1) = FnVal $ return . eqCharHof
    where
      eqCharHof (CharVal c2)
        | c1 == c2 = true
        | otherwise = false

ltChar :: Val -> Val
ltChar (CharVal c1) = FnVal $ return . ltCharHof
    where
      ltCharHof (CharVal c2)
        | c1 < c2 = true
        | otherwise = false

-- Seq

isTuple :: Val -> Val
isTuple (SeqVal fns) = FnVal isTupleHof
    where
      all' [] [] = return true
      all' [] _ = return false
      all' _ [] = return false
      all' (FnVal fn:fns) (val:vals) =
        do val' <- fn val
           if isNotFalseVal val'
             then all' fns vals
             else return false

      isTupleHof (SeqVal vals)
        | length fns == length vals = all' fns vals
      isTupleHof _ = return false

isList :: Val -> Val
isList (FnVal fn1) = FnVal isListHof
  where
    isListHof (FnVal fn2) = return $ FnVal isListHof'
      where
        isListHof' (SeqVal (x:xs)) =
          do val1 <- fn1 x
             if isNotFalseVal val1
               then do
                 val2 <- fn2 (SeqVal xs)
                 if isNotFalseVal val2
                   then return true
                   else return false
               else
                 return false
        isListHof' _ =
          return false

null :: Val
null = SeqVal []

cons :: Val -> Val
cons val = FnVal $ \(SeqVal vals) -> return . SeqVal $ val:vals

hd :: Val -> Val
hd (SeqVal (val:_)) = val

tl :: Val -> Val
tl (SeqVal (_:vals)) = SeqVal vals

-- Fn

isFn :: Val -> Val
isFn FnVal {} = true
isFn _ = false

-- Obj

isObj :: Val -> Val
isObj TypeVal {} = true
isObj _ = false

-- combining forms

apply :: Val -> InterpreterM Val
apply (SeqVal [FnVal fn, val]) = fn val

o :: Val -> Val
o (FnVal fn1) = FnVal $ return . oHof
  where
    oHof (FnVal fn2) = FnVal $ \val -> fn1 =<< fn2 val

pand :: Val -> Val
pand val@FnVal {} = val
pand (SeqVal vals) =
    FnVal $ \val -> andHof val vals
    where andHof _ [] = true
          andHof val [FnVal fn] = fn val
          andHof val (FnVal fn:vals) =
              let val' = fn val in
              if isFalseVal val' then
                false
              else
                andHof val vals

por :: Val -> Val
por val@FnVal {} = val
por (SeqVal vals) =
    FnVal $ \val -> orHof val vals
    where orHof _ [] = false
          orHof val [FnVal fn] = fn val
          orHof val (FnVal fn:vals) =
              let val' = fn val in
              if isFalseVal val' then
                orHof val vals
              else
                true

-- environment

m :: (Val -> Val) -> Val
m fn = FnVal $ return . fn

fnDesc :: FnDesc
fnDesc =
  map ((++ "#") *** id)
  [-- Bool
   ("isBool", m isBool),
   ("false", false),
   ("true", true),
   ("eqBool", m eqBool),
   -- Int
   ("isInt", m isInt),
   ("eqInt", m eqInt),
   ("ltInt", m ltInt),
   ("addInt", m addInt),
   ("subInt", m subInt),
   ("mulInt", m mulInt),
   ("divInt", m divInt),
   ("absInt", m absInt),
   ("negInt", m negInt),
   ("invInt", m invInt),
   ("remInt", m remInt),
   -- Real
   ("isReal", m isReal),
   ("eqReal", m eqReal),
   ("ltReal", m ltReal),
   ("addReal", m addReal),
   ("subReal", m subReal),
   ("mulReal", m mulReal),
   ("divReal", m divReal),
   ("absReal", m absReal),
   ("ceilingReal", m ceilingReal),
   ("floorReal", m floorReal),
   ("negReal", m negReal),
   ("invReal", m invReal),
   -- Int and Real
   ("ltIntReal", m ltIntReal),
   ("ltRealInt", m ltRealInt),
   ("addIntReal", m addIntReal),
   ("mulIntReal", m mulIntReal),
   -- Char
   ("isChar", m isChar),
   ("eqChar", m eqChar),
   ("ltChar", m ltChar),
   -- Seq
   ("isTuple", m isTuple),
   ("isList", m isList),
   ("null", null),
   ("cons", m cons),
   ("hd", m hd),
   ("tl", m tl),
   -- Fn
   ("isFn", m isFn),
   -- Obj
   ("isObj", m isObj),
   -- combining forms
   ("apply", FnVal apply),
   ("o", m o),
   ("pand", m pand),
   ("por", m por),
   ("un", m unSharp),
   ("index", m index),
   ("mkCons", m mkCons),
   ("isCons", m isCons),
   ("unCons", m unCons),
   ("link", m link)]

unSharp :: Val -> Val
unSharp (TypeVal val) = val

index :: Val -> Val
index (IntVal i) = FnVal $ return . indexHof
  where
    indexHof (SeqVal vals) = vals !! i

mkCons :: Val -> Val
mkCons (IntVal typeId) = FnVal $ return . mkConsHof
  where
    mkConsHof val = TypeVal $ SeqVal [IntVal typeId, val]

unCons :: Val -> Val
unCons (TypeVal (SeqVal [_, val])) = val

isCons :: Val -> Val
isCons (IntVal typeId) = FnVal $ return . isConsHof
  where
    isConsHof (TypeVal (SeqVal [IntVal typeId', _]))
      | typeId == typeId' = true
    isConsHof _ = false
isCons _ = false

link :: Val -> Val
link val = IntVal . hash . unboxString $ val

coreModule :: Module
coreModule = mkCoreModule coreName [] fnDesc
