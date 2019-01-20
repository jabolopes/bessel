module Core where

import Prelude hiding (null)

import Control.Applicative ((<$>))
import Control.Arrow ((***))
import Control.Monad.IO.Class
import Data.Array (Array, (!))
import qualified Data.Array as Array
import Data.Hashable (hash)
import System.IO as IO

import Config
import Data.Module
import Monad.InterpreterM
import qualified Stage.Interpreter as Interpreter

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

isList :: Val -> Val
isList (SeqVal fns) = FnVal arg2
    where
      arg2 (SeqVal vals) | length fns == length vals = apply fns vals
      arg2 _ = return false

      apply [] [] = return true
      apply [] _ = return false
      apply _ [] = return false
      apply (FnVal fn:fns) (val:vals) =
        do val' <- fn val
           if isNotFalseVal val'
             then apply fns vals
             else return false
isList _ = error "Core.isList: expected forall a. [a -> Bool]"

isHeadTail :: Val -> Val
isHeadTail (FnVal fn1) = FnVal arg2
  where
    arg2 (FnVal fn2) = return $ FnVal $ arg3 fn2
    arg2 _ = error "Core.isHeadTail: expected forall a. -> Bool"

    arg3 fn2 (SeqVal vals) = apply fn1 fn2 vals
    arg3 _ _ = error "Core.isHeadTail: expected list"

    apply _ _ [] =
      return false
    apply fn1 fn2 (x:xs) =
      do val1 <- fn1 x
         if isNotFalseVal val1
           then do
             val2 <- fn2 (SeqVal xs)
             if isNotFalseVal val2
               then return true
               else return false
           else
             return false

null :: Val
null = SeqVal []

cons :: Val -> Val
cons val = FnVal $ \(SeqVal vals) -> return . SeqVal $ val:vals

hd :: Val -> Val
hd (SeqVal (val:_)) = val

tl :: Val -> Val
tl (SeqVal (_:vals)) = SeqVal vals

-- Tuple

arrayLength :: Array Int a -> Int
arrayLength array =
  let (x, y) = Array.bounds array in
  y - x + 1

isTuple0 :: Val -> Val
isTuple0 (TupleVal vals)
  | arrayLength vals == 0 = true
  | otherwise = false
isTuple0 _ = error "Core.isTuple0: expected unit"

isTuple :: Val -> Val
isTuple (TupleVal fns) = FnVal arg2
    where
      arg2 (TupleVal vals) | arrayLength fns == arrayLength vals = apply [0..arrayLength fns - 1] vals
      arg2 _ = return false

      apply :: [Int] -> Array Int Val -> InterpreterM Val
      apply [] _ = return true
      apply (index:indexes) vals =
        do let fn = fns ! index
           let val = vals ! index
           case fn of
             FnVal predicate ->
               do val' <- predicate val
                  if isNotFalseVal val'
                    then apply indexes vals
                    else return false
             _ ->
               error "Core.isTuple: expected forall a. -> Bool"
isTuple _ = error "Core.isTuple: expected forall a b ... . (a -> Bool, b -> Bool, ...)"

mkTuple0 :: Val
mkTuple0 = TupleVal emptyArray
  where
    emptyArray = Array.array (0, -1) []

mkTuple2 :: Val -> Val
mkTuple2 val1 = FnVal $
  \val2 -> return . TupleVal $ Array.listArray (0, 1) [val1, val2]

mkTuple3 :: Val -> Val
mkTuple3 val1 = FnVal $
  \val2 -> return . FnVal $
    \val3 -> return . TupleVal $ Array.listArray (0, 2) [val1, val2, val3]

tupleRef0 :: Val -> Val
tupleRef0 (TupleVal vals) = vals ! 0

tupleRef1 :: Val -> Val
tupleRef1 (TupleVal vals) = vals ! 1

tupleRef2 :: Val -> Val
tupleRef2 (TupleVal vals) = vals ! 2

-- Variant (i.e., algebraic datatype)

isVariant :: Val -> Val
isVariant typeName@SeqVal {} = FnVal arg2
  where
    arg2 (IntVal isCons) = return . FnVal $ arg3 isCons
    arg2 _ = fail $ "Core.isVariant: expected integer as second argument"

    arg3 isCons (FnVal isFn) = return . FnVal $ arg4 isCons isFn
    arg3 _ _ = fail $ "Core.isVariant: expected function as third argument"

    arg4 isCons isFn (VariantVal typeId cons val) = apply isCons isFn typeId cons val
    arg4 _ _ _ = return false

    apply isCons isFn typeId cons val =
      let isTypeId = hash $ unboxString typeName in
      if isTypeId == typeId && isCons == cons then
        do val' <- isFn val
           if isNotFalseVal val'
             then return true
             else return false
      else
        return false
isVariant _ =
  error $ "Core.isVariant: expected string as first argument"

mkVariant :: Val -> Val
mkVariant typeName@SeqVal {} = FnVal arg2
  where
    arg2 (IntVal cons) = return . FnVal $ arg3 cons
    arg2 _ = fail $ "Core.mkVariant: expected integer as second argument"

    arg3 cons val = apply (hash $ unboxString typeName) cons val

    apply typeId cons val = return $ VariantVal typeId cons val

unVariant :: Val -> Val
unVariant (VariantVal _ _ val) = val
unVariant _ = error $ "Core.unVariant: expected variant as first argument"

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

-- pand :: Val -> Val
-- pand val@FnVal {} = val
-- pand (SeqVal vals) =
--     FnVal $ \val -> andHof val vals
--     where andHof _ [] = true
--           andHof val [FnVal fn] = fn val
--           andHof val (FnVal fn:vals) =
--               let val' = fn val in
--               if isFalseVal val' then
--                 false
--               else
--                 andHof val vals

-- por :: Val -> Val
-- por val@FnVal {} = val
-- por (SeqVal vals) =
--     FnVal $ \val -> orHof val vals
--     where orHof _ [] = false
--           orHof val [FnVal fn] = fn val
--           orHof val (FnVal fn:vals) =
--               let val' = fn val in
--               if isFalseVal val' then
--                 orHof val vals
--               else
--                 true

-- io

mapFile :: Val -> InterpreterM Val
mapFile filename =
  return . FnVal $ return . mapFileHof
  where
    mapFileHof (FnVal fn) =
      IOVal $ do
        contents <- IO.readFile $ unboxString filename
        Interpreter.liftInterpreterM . fn $ boxString contents

readLine :: Val
readLine =
  IOVal $ boxString <$> liftIO (hGetLine stdin)

putLine :: Val -> Val
putLine str =
  IOVal $ do
    liftIO . hPutStrLn stdout $ unboxString str
    return $ mkTuple0

bindIO :: Val -> Val
bindIO (IOVal m) = FnVal $ return . IOVal . bindIOHof
  where
    bindIOHof :: Val -> IO Val
    bindIOHof (FnVal fn) =
      do res <- m
         val <- Interpreter.liftInterpreterM $ fn res
         return val

-- environment

fnDesc :: FnDesc
fnDesc =
  map ((++ "#") *** id)
  [-- Bool
   ("isBool", primitive isBool),
   ("false", false),
   ("true", true),
   ("eqBool", primitive eqBool),
   -- Int
   ("isInt", primitive isInt),
   ("eqInt", primitive eqInt),
   ("ltInt", primitive ltInt),
   ("addInt", primitive addInt),
   ("subInt", primitive subInt),
   ("mulInt", primitive mulInt),
   ("divInt", primitive divInt),
   ("absInt", primitive absInt),
   ("negInt", primitive negInt),
   ("invInt", primitive invInt),
   ("remInt", primitive remInt),
   -- Real
   ("isReal", primitive isReal),
   ("eqReal", primitive eqReal),
   ("ltReal", primitive ltReal),
   ("addReal", primitive addReal),
   ("subReal", primitive subReal),
   ("mulReal", primitive mulReal),
   ("divReal", primitive divReal),
   ("absReal", primitive absReal),
   ("ceilingReal", primitive ceilingReal),
   ("floorReal", primitive floorReal),
   ("negReal", primitive negReal),
   ("invReal", primitive invReal),
   -- Int and Real
   ("ltIntReal", primitive ltIntReal),
   ("ltRealInt", primitive ltRealInt),
   ("addIntReal", primitive addIntReal),
   ("mulIntReal", primitive mulIntReal),
   -- Char
   ("isChar", primitive isChar),
   ("eqChar", primitive eqChar),
   ("ltChar", primitive ltChar),
   -- Seq
   ("isList", primitive isList),
   ("isHeadTail", primitive isHeadTail),
   ("null", null),
   ("cons", primitive cons),
   ("hd", primitive hd),
   ("tl", primitive tl),
   -- Tuple
   ("isTuple0", primitive isTuple0),
   ("isTuple2", primitive isTuple),
   ("isTuple3", primitive isTuple),
   ("mkTuple0", mkTuple0),
   ("mkTuple2", primitive mkTuple2),
   ("mkTuple3", primitive mkTuple3),
   ("tuple2Ref0", primitive tupleRef0),
   ("tuple2Ref1", primitive tupleRef1),
   ("tuple3Ref0", primitive tupleRef0),
   ("tuple3Ref1", primitive tupleRef1),
   ("tuple3Ref2", primitive tupleRef2),
   -- Variant
   ("isVariant", primitive isVariant),
   ("mkVariant", primitive mkVariant),
   ("unVariant", primitive unVariant),
   -- Fn
   ("isFn", primitive isFn),
   -- Obj
   ("isObj", primitive isObj),
   -- combining forms
   ("apply", FnVal apply),
   ("o", primitive o),
   -- io
   ("mapFile", FnVal mapFile),
   ("readLine", readLine),
   ("putLine", primitive putLine),
   ("bindIO", primitive bindIO),
   -- misc
   ("un", primitive unSharp),
   ("index", primitive index),
   ("mkCons", primitive mkCons),
   ("isCons", primitive isCons),
   ("unCons", primitive unCons),
   ("link", primitive link)]

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

coreModule :: IO Module
coreModule = mkCoreModule coreName [] fnDesc
