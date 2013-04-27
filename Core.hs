module Core where

import Prelude hiding (concat, reverse)
import qualified Prelude

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

eqBool :: Expr -> Expr
eqBool (BoolExpr b1) = FnExpr eqBoolHof
    where eqBoolHof (BoolExpr b2)
              | b1 == b2 = return true
              | otherwise = return false


eqInt :: Expr -> Expr
eqInt (IntExpr i1) = FnExpr eqIntHof
    where eqIntHof (IntExpr i2)
              | i1 == i2 = return true
              | otherwise = return false


eqReal :: Expr -> Expr
eqReal (DoubleExpr d1) = FnExpr eqRealHof
    where eqRealHof (DoubleExpr d2)
              | d1 == d2 = return true
              | otherwise = return false


eqChar :: Expr -> Expr
eqChar (CharExpr c1) = FnExpr eqCharHof
    where eqCharHof (CharExpr c2)
              | c1 == c2 = return true
              | otherwise = return false


all2 :: (a -> b -> Expr) -> [a] -> [b] -> Expr
all2 _ [] [] = true
all2 _ [] _ = false
all2 _ _ [] = false
all2 fn (x1:xs1) (x2:xs2)
    | isNotFalseExpr (fn x1 x2) = all2 fn xs1 xs2
    | otherwise = false


exprEq :: Expr -> Expr -> Expr
exprEq (BoolExpr b1) (BoolExpr b2) | b1 == b2 = true
exprEq (IntExpr i1) (IntExpr i2) | i1 == i2 = true
exprEq (DoubleExpr d1) (DoubleExpr d2) | d1 == d2 = true
exprEq (CharExpr c1) (CharExpr c2) | c1 == c2 = true
exprEq (SeqExpr exprs1) (SeqExpr exprs2) = all2 exprEq exprs1 exprs2
exprEq (TypeExpr _ tid1 expr1) (TypeExpr _ tid2 expr2) | tid1 == tid2 = expr1 `exprEq` expr2
exprEq _ _ = false


eqSeq :: Expr -> Expr
eqSeq (SeqExpr exprs1) = FnExpr eqSeqHof
    where eqSeqHof (SeqExpr exprs2) =
              return (all2 exprEq exprs1 exprs2)


eqObj :: Expr -> Expr
eqObj (TypeExpr _ tid1 expr1) = FnExpr eqObjHof
    where eqObjHof (TypeExpr _ tid2 expr2)
              | tid1 == tid2 = return (expr1 `exprEq` expr2)
              | otherwise = return false


exprLt :: Expr -> Expr -> Expr
exprLt (BoolExpr b1) (BoolExpr b2) | b1 < b2 = true
exprLt expr@(IntExpr i1) (IntExpr i2) = if i1 < i2 then expr else false
exprLt expr@(DoubleExpr d1) (DoubleExpr d2) = if d1 < d2 then expr else false
exprLt expr@(CharExpr c1) (CharExpr c2) = BoolExpr $ c1 < c2
exprLt expr@(SeqExpr exprs1) (SeqExpr exprs2)
      | length exprs1 < length exprs2 = expr
      | length exprs1 == length exprs2 && isNotFalseExpr (all2 exprLt exprs1 exprs2) = expr
      | otherwise = false
exprLt (FnExpr _) (FnExpr _) = false
exprLt (TypeExpr _ _ _) (TypeExpr _ _ _) = false
exprLt (BoolExpr _) _ = true
exprLt (IntExpr _) (BoolExpr _) = false
exprLt expr@(IntExpr i) (DoubleExpr d) = if (fromIntegral i) < d then expr else false
exprLt expr@(IntExpr _) _ = expr
exprLt (DoubleExpr _) (BoolExpr _) = false
exprLt expr@(DoubleExpr d) (IntExpr i) = BoolExpr $ d < (fromIntegral i)
exprLt expr@(DoubleExpr _) _ = expr
exprLt (CharExpr _) (BoolExpr _) = false
exprLt (CharExpr _) (IntExpr _) = false
exprLt (CharExpr _) (DoubleExpr _) = false
exprLt expr@(CharExpr _) _ = expr
exprLt (SeqExpr _) (BoolExpr _) = false
exprLt (SeqExpr _) (IntExpr _) = false
exprLt (SeqExpr _) (DoubleExpr _) = false
exprLt (SeqExpr _) (CharExpr _) = false
exprLt expr@(SeqExpr _) _ = expr
exprLt expr@(FnExpr _) (TypeExpr _ _ _) = expr
exprLt (FnExpr _) _ = false
exprLt (TypeExpr _ _ _) _ = false


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
absInt (IntExpr i) = IntExpr (abs i)


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


remInt :: Expr -> Expr
remInt (IntExpr i1) =
    FnExpr $ \(IntExpr i2) -> return $ IntExpr (i1 `rem` i2)


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

plist :: Expr -> Expr
plist (SeqExpr fns) = FnExpr plistHof
    where all' :: [Expr] -> [Expr] -> InterpreterM Bool
          all' [] [] = return True
          all' [] _ = return False
          all' _ [] = return False
          all' (FnExpr fn:fns') (val:vals') =
              do expr <- fn val
                 if isNotFalseExpr expr
                   then all' fns' vals'
                   else return False

          plistHof (SeqExpr vals)
                   | length fns == length vals = BoolExpr <$> all' fns vals
                   | otherwise = return false
          plistHof _ = return false


pand :: Expr -> Expr
pand (FnExpr fn) = FnExpr $ \expr -> fn expr
pand (SeqExpr exprs) =
    FnExpr $ \expr -> andHof expr exprs
    where andHof _ [] = return true
          andHof expr [FnExpr fn] = fn expr
          andHof expr (FnExpr fn:exprs) =
              do val <- fn expr
                 if isFalseExpr val
                   then return false
                   else andHof expr exprs


por :: Expr -> Expr
por (FnExpr fn) = FnExpr $ \expr -> fn expr
por (SeqExpr exprs) =
    FnExpr $ \expr -> orHof expr exprs
    where orHof _ [] = return false
          orHof expr [FnExpr fn] = fn expr
          orHof expr (FnExpr fn:exprs) =
              do val <- fn expr
                 if isFalseExpr val
                   then orHof expr exprs
                   else return true


pal :: Expr -> Expr
pal (SeqExpr [FnExpr fn1, FnExpr fn2]) =
    FnExpr pal'
    where pal' (SeqExpr (x:xs)) =
              do expr1 <- fn1 x
                 if isNotFalseExpr expr1
                   then do expr2 <- fn2 (SeqExpr xs)
                           if isNotFalseExpr expr2
                             then return true
                             else return false
                   else return false
          pal' _ = return false


par :: Expr -> Expr
par (SeqExpr [FnExpr fn1, FnExpr fn2]) =
    FnExpr par'
    where par' (SeqExpr xs) | Prelude.not (null xs) =
              do expr1 <- fn1 $ SeqExpr $ init xs
                 if isNotFalseExpr expr1
                   then do expr2 <- fn2 $ last xs
                           if isNotFalseExpr expr2
                             then return true
                             else return false
                   else return false
          par' _ = return false


al :: Expr -> Expr
al expr =
    FnExpr $ \(SeqExpr exprs) -> return $ SeqExpr (expr:exprs)


ar :: Expr -> Expr
ar (SeqExpr exprs) =
    FnExpr $ \expr -> return $ SeqExpr (exprs ++ [expr])


concat :: Expr -> Expr
concat (SeqExpr exprs) =
    SeqExpr $ concatMap (\(SeqExpr exprs) -> exprs) exprs


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


signal :: Expr -> a
signal expr = throwSignalException $ show expr


-- environment

m :: (Expr -> Expr) -> Expr
m fn = FnExpr $ \expr -> return $ fn expr


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
  ("isbool", predT, m isbool),
  ("isint", predT, m isint),
  ("isreal", predT, m isreal),
  ("ischar", predT, m ischar),
  ("isfn", predT, m isfn),
  ("isobj", predT, m isobj),
  ("isseq", predT, m isseq),
  ("isseqof", ArrowT predT DynT, m isseqof),
  -- comparison functions
  ("eqBool", ArrowT BoolT (ArrowT BoolT BoolT), m eqBool),
  ("eqInt", ArrowT IntT (ArrowT IntT BoolT), m eqInt),
  ("eqReal", ArrowT DoubleT (ArrowT DoubleT BoolT), m eqReal),
  ("eqChar", ArrowT CharT (ArrowT CharT BoolT), m eqChar),
  ("eqSeq", ArrowT DynT (ArrowT DynT BoolT), m eqSeq),
  ("eqObj", ArrowT DynT (ArrowT DynT BoolT), m eqObj),
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
  ("negInt", ArrowT IntT IntT, m negInt),
  ("negReal", ArrowT DoubleT DoubleT, m negReal),
  ("remInt", ArrowT IntT (ArrowT IntT IntT), m remInt),
  -- combining forms
  ("apply", ArrowT (TupT [ArrowT DynT DynT, DynT]) DynT, FnExpr apply),
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
  ("al", ArrowT (SeqT DynT) (SeqT DynT), m al),
  ("ar", ArrowT (SeqT DynT) (SeqT DynT), m ar),
  ("concat", ArrowT (SeqT (SeqT DynT)) (SeqT DynT), m concat),
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
srcfile = mkCoreSrcFile "Core" [] typeDesc fnDesc
