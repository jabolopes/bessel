module Core where

import Prelude hiding (head, null, tail)

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

-- Blame

check :: Val -> Val
check (FnVal fn) = FnVal arg2
  where
    arg2 val =
      do val' <- fn val
         if isNotFalseVal val'
           then return val
           else fail "type error"
check _ = error "Core.check: expected forall a. a -> Bool as first argument"

-- Bool

isBool :: Val -> Val
isBool BoolVal {} = true
isBool _ = false

eqBool :: Val -> Val
eqBool (BoolVal bool1) = FnVal arg2
  where
    arg2 (BoolVal bool2) = apply bool1 bool2
    arg2 _ = fail "Core.eqBool: expected bool as second argument"

    apply b1 b2
      | b1 == b2 = return true
      | otherwise = return false
eqBool _ =
  error "Core.eqBool: expected bool as first argument"

-- Int

isInt :: Val -> Val
isInt IntVal {} = true
isInt _ = false

eqInt :: Val -> Val
eqInt (IntVal integer1) = FnVal arg2
  where
    arg2 (IntVal integer2) = apply integer1 integer2
    arg2 _ = fail "Core.eqInt: expected integer as second argument"

    apply i1 i2
      | i1 == i2 = return true
      | otherwise = return false
eqInt _ =
  error "Core.eqInt: expected integer as first argument"

ltInt :: Val -> Val
ltInt (IntVal integer1) = FnVal arg2
  where
    arg2 (IntVal integer2) = apply integer1 integer2
    arg2 _ = fail "Core.ltInt: expected integer as second argument"

    apply i1 i2
      | i1 < i2 = return true
      | otherwise = return false
ltInt _ =
  error "Core.ltInt: expected integer as first argument"

addInt :: Val -> Val
addInt (IntVal integer1) = FnVal arg2
  where
    arg2 (IntVal integer2) = apply integer1 integer2
    arg2 _ = fail "Core.addInt: expected integer as second argument"

    apply i1 i2 = return . IntVal $ i1 + i2
addInt _ =
  error "Core.addInt: expected integer as first argument"

subInt :: Val -> Val
subInt (IntVal integer1) = FnVal arg2
  where
    arg2 (IntVal integer2) = apply integer1 integer2
    arg2 _ = fail "Core.subInt: expected integer as second argument"

    apply i1 i2 = return . IntVal $ i1 - i2
subInt _ =
  error "Core.subInt: expected integer as first argument"

mulInt :: Val -> Val
mulInt (IntVal integer1) = FnVal arg2
  where
    arg2 (IntVal integer2) = apply integer1 integer2
    arg2 _ = fail "Core.mulInt: expected integer as second argument"

    apply i1 i2 = return . IntVal $ i1 * i2
mulInt _ =
  error "Core.mulInt: expected integer as first argument"

divInt :: Val -> Val
divInt (IntVal integer1) = FnVal arg2
  where
    arg2 (IntVal integer2) = apply integer1 integer2
    arg2 _ = fail "Core.divInt: expected integer as second argument"

    apply i1 i2 = return . IntVal $ i1 `div` i2
divInt _ =
  error "Core.divInt: expected integer as first argument"

remInt :: Val -> Val
remInt (IntVal integer1) = FnVal arg2
  where
    arg2 (IntVal integer2) = apply integer1 integer2
    arg2 _ = fail "Core.remInt: expected integer as second argument"

    apply i1 i2 = return . IntVal $ i1 `rem` i2
remInt _ =
  error "Core.remInt: expected integer as first argument"

absInt :: Val -> Val
absInt (IntVal i) = IntVal (abs i)
absInt _ = error "Core.absInt: expected integer as first argument"

negInt :: Val -> Val
negInt (IntVal i) = IntVal (- i)
negInt _ = error "Core.negInt: expected integer as first argument"

invInt :: Val -> Val
invInt (IntVal i) = RealVal (1.0 / fromIntegral i)
invInt _ = error "Core.invInt: expected integer as first argument"

-- Real

isReal :: Val -> Val
isReal RealVal {} = true
isReal _ = false

eqReal :: Val -> Val
eqReal (RealVal double1) = FnVal arg2
  where
    arg2 (RealVal double2) = apply double1 double2
    arg2 _ = fail "Core.eqReal: expected real as second argument"

    apply d1 d2
      | d1 == d2 = return true
      | otherwise = return false
eqReal _ =
  error "Core.eqReal: expected real as first argument"

ltReal :: Val -> Val
ltReal (RealVal double1) = FnVal arg2
  where
    arg2 (RealVal double2) = apply double1 double2
    arg2 _ = fail "Core.ltReal: expected real as second argument"

    apply d1 d2
      | d1 < d2 = return true
      | otherwise = return false
ltReal _ =
  error "Core.ltReal: expected real as first argument"

addReal :: Val -> Val
addReal (RealVal double1) = FnVal arg2
  where
    arg2 (RealVal double2) = apply double1 double2
    arg2 _ = fail "Core.addReal: expected real as second argument"

    apply d1 d2 = return . RealVal $ d1 + d2
addReal _ =
  error "Core.addReal: expected real as first argument"

subReal :: Val -> Val
subReal (RealVal double1) = FnVal arg2
  where
    arg2 (RealVal double2) = apply double1 double2
    arg2 _ = fail "Core.subReal: expected real as second argument"

    apply d1 d2 = return . RealVal $ d1 - d2
subReal _ =
  error "Core.subReal: expected real as first argument"

mulReal :: Val -> Val
mulReal (RealVal double1) = FnVal arg2
  where
    arg2 (RealVal double2) = apply double1 double2
    arg2 _ = fail "Core.mulReal: expected real as second argument"

    apply d1 d2 = return . RealVal $ d1 * d2
mulReal _ =
  error "Core.mulReal: expected real as first argument"

divReal :: Val -> Val
divReal (RealVal double1) = FnVal arg2
  where
    arg2 (RealVal double2) = apply double1 double2
    arg2 _ = fail "Core.divReal: expected real as second argument"

    apply d1 d2 = return . RealVal $ d1 / d2
divReal _ =
  error "Core.divReal: expected real as first argument"

absReal :: Val -> Val
absReal (RealVal d) = RealVal $ abs d
absReal _ = error "Core.absReal: expected real as first argument"

ceilingReal :: Val -> Val
ceilingReal (RealVal d) = IntVal $ ceiling d
ceilingReal _ = error "Core.ceilingReal: expected real as first argument"

floorReal :: Val -> Val
floorReal (RealVal d) = IntVal $ floor d
floorReal _ = error "Core.floorReal: expected real as first argument"

negReal :: Val -> Val
negReal (RealVal d) = RealVal $ -d
negReal _ = error "Core.negReal: expected real as first argument"

invReal :: Val -> Val
invReal (RealVal d) = RealVal $ 1 / d
invReal _ = error "Core.invReal: expected real as first argument"

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

-- String

isString :: Val -> Val
isString StringVal {} = true
isString _ = false

eqString :: Val -> Val
eqString (StringVal string1) = FnVal arg2
  where
    arg2 (StringVal string2) = apply string1 string2
    arg2 _ = fail "Core.eqString: expected string as second argument"

    apply b1 b2
      | b1 == b2 = return true
      | otherwise = return false
eqString _ =
  error "Core.eqString: expected string as first argument"

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
isList _ =
  error "Core.isList: expected forall a. [a -> Bool] as first argument"

isHeadTail :: Val -> Val
isHeadTail (FnVal fn1) = FnVal arg2
  where
    arg2 (FnVal fn2) =
      return $ FnVal $ arg3 fn2
    arg2 _ =
      error "Core.isHeadTail: expected forall a. -> Bool as second argument"

    arg3 fn2 (SeqVal vals) =
      apply fn1 fn2 vals
    arg3 _ _ =
      error "Core.isHeadTail: expected list as third argument"

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
isHeadTail _ =
  error "Core.isHeadTail: expected function as first argument"

null :: Val
null = SeqVal []

cons :: Val -> Val
cons val = FnVal $ \(SeqVal vals) -> return . SeqVal $ val:vals

head :: Val -> Val
head (SeqVal (val:_)) = val
head _ = error "Core.head: expected non-empty list"

tail :: Val -> Val
tail (SeqVal (_:vals)) = SeqVal vals
tail _ = error "Core.tail: expected non-empty list"

index :: Val -> Val
index (IntVal i) = FnVal $ return . indexHof
  where
    indexHof (SeqVal vals) = vals !! i

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
tupleRef0 _ = error "Core.tupleRef0: expected tuple"

tupleRef1 :: Val -> Val
tupleRef1 (TupleVal vals) = vals ! 1
tupleRef1 _ = error "Core.tupleRef1: expected tuple"

tupleRef2 :: Val -> Val
tupleRef2 (TupleVal vals) = vals ! 2
tupleRef2 _ = error "Core.tupleRef2: expected tuple"

-- Type

isType :: Val -> Val
isType (StringVal typeName) = FnVal arg2
  where
    arg2 (VariantVal typeId _ _) = apply typeId
    arg2 _ = return false

    apply typeId =
      if hash typeName == typeId
        then return true
        else return false
isType _ =
  error $ "Core.isType: expected string as first argument"

-- Variant (i.e., algebraic datatype)

isVariant0 :: Val -> Val
isVariant0 (StringVal typeName) = FnVal arg2
  where
    arg2 (IntVal isCons) = return . FnVal $ arg3 isCons
    arg2 _ = fail $ "Core.isVariant0: expected integer as second argument"

    arg3 isCons (VariantVal typeId cons Nothing) = apply isCons typeId cons
    arg3 _ _ = return false

    apply isCons typeId cons =
      let isTypeId = hash typeName in
      if hash typeName == typeId && isCons == cons then
        return true
      else
        return false
isVariant0 _ =
  error $ "Core.isVariant0: expected string as first argument"

isVariant :: Val -> Val
isVariant (StringVal typeName) = FnVal arg2
  where
    arg2 (IntVal isCons) = return . FnVal $ arg3 isCons
    arg2 _ = fail $ "Core.isVariant: expected integer as second argument"

    arg3 isCons (FnVal isFn) = return . FnVal $ arg4 isCons isFn
    arg3 _ _ = fail $ "Core.isVariant: expected function as third argument"

    arg4 isCons isFn (VariantVal typeId cons (Just val)) = apply isCons isFn typeId cons val
    arg4 _ _ _ = return false

    apply isCons isFn typeId cons val =
      let isTypeId = hash typeName in
      if hash typeName == typeId && isCons == cons then
        do val' <- isFn val
           if isNotFalseVal val'
             then return true
             else return false
      else
        return false
isVariant _ =
  error $ "Core.isVariant: expected string as first argument"

mkVariant0 :: Val -> Val
mkVariant0 (StringVal typeName) = FnVal arg2
  where
    arg2 (IntVal cons) = apply (hash typeName) cons
    arg2 _ = fail $ "Core.mkVariant0: expected integer as second argument"

    apply typeId cons = return $ VariantVal typeId cons Nothing
mkVariant0 _ =
  error "Core.mkVariant0: expected string as first argument"

mkVariant :: Val -> Val
mkVariant (StringVal typeName) = FnVal arg2
  where
    arg2 (IntVal cons) = return . FnVal $ arg3 cons
    arg2 _ = fail $ "Core.mkVariant: expected integer as second argument"

    arg3 cons val = apply (hash typeName) cons val

    apply typeId cons val = return $ VariantVal typeId cons (Just val)
mkVariant _ =
  error "Core.mkVariant: expected string as first argument"

unVariant :: Val -> Val
unVariant (VariantVal _ _ (Just val)) = val
unVariant (VariantVal _ _ Nothing) = error "Core.unVariant: expected variant with value"
unVariant _ = error "Core.unVariant: expected variant as first argument"

-- Fn

isFn :: Val -> Val
isFn FnVal {} = true
isFn _ = false

-- Obj

isObj :: Val -> Val
isObj TypeVal {} = true
isObj _ = false

-- combining forms

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
mapFile (StringVal filename) =
  return . FnVal $ return . mapFileHof
  where
    mapFileHof (FnVal fn) =
      IOVal $ do
        contents <- IO.readFile filename
        Interpreter.liftInterpreterM . fn $ StringVal contents
mapFile _ =
  error "Core.mapFile: expected string as first argument"

readLine :: Val
readLine =
  IOVal $ StringVal <$> liftIO (hGetLine stdin)

putLine :: Val -> Val
putLine (StringVal str) =
  IOVal $ do
    liftIO $ hPutStrLn stdout str
    return $ mkTuple0
putLine _ =
  error "Core.putLine: expected string as first argument"

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
  [-- Blame
   ("check", primitive check),
   -- Bool
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
   -- String
   ("isString", primitive isString),
   ("eqString", primitive eqString),
   -- Seq
   ("isList", primitive isList),
   ("isHeadTail", primitive isHeadTail),
   ("null", null),
   ("cons", primitive cons),
   ("head", primitive head),
   ("tail", primitive tail),
   ("index", primitive index),
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
   -- Type
   ("isType", primitive isType),
   -- Variant
   ("isVariant0", primitive isVariant0),
   ("isVariant", primitive isVariant),
   ("mkVariant0", primitive mkVariant0),
   ("mkVariant", primitive mkVariant),
   ("unVariant", primitive unVariant),
   -- Fn
   ("isFn", primitive isFn),
   -- Obj
   ("isObj", primitive isObj),
   -- Combining forms
   ("o", primitive o),
   -- io
   ("mapFile", FnVal mapFile),
   ("readLine", readLine),
   ("putLine", primitive putLine),
   ("bindIO", primitive bindIO)]

coreModule :: IO Module
coreModule = mkCoreModule coreName [] fnDesc
