{-# LANGUAGE BangPatterns, NamedFieldPuns,
             ParallelListComp, TupleSections #-}
module Typechecker where

import Prelude hiding ((<=))

import Control.Monad (liftM, when)
import Data.Functor ((<$>))
import Data.List (intercalate, isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust)

import Config
import Data.Context
import qualified Data.Context as Context
import Data.Exception
import Data.Expr (DefnKw(..), Expr(..), isValueE)
import Data.FileSystem (FileSystem)
import qualified Data.FileSystem as FileSystem
import qualified Data.QualName as QualName (fromQualName)
import Data.SrcFile
import qualified Data.SrcFile as SrcFile
import Data.Symbol
import Data.Type
import Printer.AbbrevExpr

import Data.Definition
import qualified Data.Definition as Definition

import Debug.Trace
import System.IO.Unsafe


debug :: Bool
debug = False


debugT :: String -> Bool
debugT desc = (debug && trace desc True) || True


debugRules :: Bool
debugRules = debug


debugRule :: String -> [(String, Bool)] -> Bool
debugRule desc [] = (debugRules && trace desc False) || False
debugRule desc ((msg, val):conds) =
  debugRule (desc ++ "\n" ++ msg ++ " = " ++ show val) conds


judgement' :: String -> String -> String -> ()
judgement' name str1 str2 =
  unsafePerformIO $ do
    let l1 = length str1
        l2 = length str2
        n = max l1 l2
        s1 = (n - l1) `div` 2
        s2 = (n - l2) `div` 2
    putStrLn $ replicate s1 ' ' ++ str1
    putStrLn $ replicate n '-' ++ " (" ++ name ++ ")"
    putStrLn $ replicate s2 ' ' ++ str2
    putStrLn ""
    return ()


judgement :: String -> String -> String -> a -> a
judgement name str1 str2 val
  | debug =
    let !() = judgement' name str1 str2 in val
  | otherwise =
    val

judgementM :: Monad m => String -> String -> String -> m ()
judgementM name str1 str2 =
  let !() = judgement name str1 str2 () in
  return ()


-- edit: maybe make (String, a) into (a, a) or (a, b)
-- and force uses of gamma to pass in types always instead of vars
gamma :: Show a => String -> [(String, a)] -> String
gamma name vars =
    name ++ intercalate "" (map (\(var, t) -> "[" ++ var ++ "=" ++ show t ++ "]") vars)


gamma' :: String -> [String] -> String
gamma' name vars =
    name ++ concatMap (\var -> "[" ++ var ++ "]") vars


gamma'' :: (Show a, Show b) => String -> [(a, Maybe b)] -> String
gamma'' name ts =
    name ++ concatMap assign ts
    where assign (t1, Nothing) = "[" ++ show t1 ++ "]"
          assign (t1, Just t2) = "[" ++ show t1 ++ "=" ++ show t2 ++ "]"


infixl 8 |-
infixl 8 |/-
infixl 8 -|
(|-) x y = x ++ " |- " ++ y
(|/-) x y = x ++ " |/- " ++ y
(-|) x y = x ++ " -| " ++ y

(<=) x y = showAbbrev x ++ " <= " ++ show y
x `synth` y = showAbbrev x ++ " => " ++ show y


infixl 9 <:
infixl 9 >:
infixl 9 ~~
(<:) x y = show x ++ " <: " ++ show y
(>:) x y = show x ++ " :> " ++ show y
(~~) x y = show x ++ " ~ " ++ show y

subst var1 var2 = "[" ++ var1 ++ "/" ++ var2 ++ "]"


-- types and contexts

genEvar :: Context -> (Type, Context)
genEvar ctx =
    let
        evar = ['^', toEnum (count ctx)]
        evarT = EvarT evar
        ctx' = insertContext ctx { count = count ctx + 1 } evar evarT
    in
      (evarT, ctx')


-- 'eliminateForalls' @ctx t@ instantiates all outer forall types
-- ('ForallT') by replacing the type variables in @t@ by existential
-- variables ('EvarT').  The updated 'Context' and resulting 'Type'
-- are returned.
--
-- Note that this function assumes that there is no evar previously
-- defined with name '^var'.  This means that the forall type must be
-- fresh.
eliminateForalls :: Context -> Type -> (Context, Type)
eliminateForalls ctx (ForallT var forallT) =
  let
      var' = '^':var
      existT = EvarT var'
      ctx' = insertContext ctx var' existT
      forallT' = substituteTvarT existT var forallT
  in
    eliminateForalls ctx' forallT'

eliminateForalls ctx t = (ctx, t)


-- 'substituteEvarTs' @ctx t@ replaces existential variables that
-- occur in @t@ by the types that have been assigned to them in
-- 'Context' @ctx@.
substituteEvarTs :: Context -> Type -> Type
substituteEvarTs _ t@BoolT = t
substituteEvarTs _ t@IntT  = t
substituteEvarTs _ t@DoubleT = t
substituteEvarTs _ t@CharT = t
substituteEvarTs ctx (TupT ts) = TupT $ map (substituteEvarTs ctx) ts
substituteEvarTs ctx (SeqT t) = SeqT $ substituteEvarTs ctx t
substituteEvarTs ctx t@DynT = t

substituteEvarTs ctx (AndT t1 t2) =
  AndT (substituteEvarTs ctx t1) (substituteEvarTs ctx t2)

substituteEvarTs ctx (ArrowT t1 t2) =
  ArrowT (substituteEvarTs ctx t1) (substituteEvarTs ctx t2)

substituteEvarTs ctx (CoT obs) =
  CoT $ map substitutePar obs
  where substitutePar (x, y) = (x, substituteEvarTs ctx y)

substituteEvarTs ctx t@(EvarT var) =
  case lookupContext ctx var of
    Nothing -> error $ "Typechecker.substituteEvarTs: " ++ show var
    Just (EvarT var') | var == var' -> t
    Just t' -> substituteEvarTs ctx t'

substituteEvarTs ctx (OrT ts) = OrT $ map (substituteEvarTs ctx) ts
substituteEvarTs ctx (ForallT vars t) = ForallT vars $ substituteEvarTs ctx t
substituteEvarTs ctx t@(TvarT _) = t


-- | Implements the following context transformation
-- @
-- Gamma [â]
-- @
--
-- @
-- Gamma [â1,â2,â = â1 -> â2]
-- @
arrowifyVar :: Context -> String -> (Context, Type)
arrowifyVar ctx var =
  let
      (Context { syms = syms1 }, Context { syms = syms2 }) = splitContext ctx var
      name1 = var ++ "1"
      name2 = var ++ "2"
      evar1 = EvarT name1
      evar2 = EvarT name2
      a1 = (name1, evar1)
      a2 = (name2, evar2)
      at = ArrowT evar1 evar2
      a  = (var, at)
  in
    (ctx { syms = syms1 ++ [a, a2, a1] ++ syms2 }, at)


-- | Implements the following context transformation
-- @
-- Gamma [â]
-- @
--
-- @
-- Gamma [â1,...,ân,â=(â1,...,ân)]
-- @
tuplifyVar :: Context -> String -> Int -> (Context, Type)
tuplifyVar ctx var n =
  let
    (Context { syms = syms1 }, Context { syms = syms2 }) = splitContext ctx var
    names = [ var ++ show i | i <- [1..n] ]
    evars = map EvarT names
    as = [ (name, evar) | name <- names | evar <- evars ]
    at = TupT evars
    a = (var, at)
  in
    (ctx { syms = syms1 ++ (a:as) ++ syms2 }, at)

-- | Implements the following context transformation
-- @
-- Gamma [â]
-- @
--
-- @
-- Gamma [â1,â = [â1]]
-- @
seqifyVar :: Context -> String -> (Context, Type)
seqifyVar ctx var =
  let
    (Context { syms = syms1 }, Context { syms = syms2 }) = splitContext ctx var
    name1 = var ++ "1"
    evar1 = EvarT name1
    a1 = (name1, evar1)
    at = SeqT evar1
    a = (var, at)
  in
    (ctx { syms = syms1 ++ [a, a1] ++ syms2 }, at)

-- | Implements the following context transformation
-- @
-- Gamma [â]
-- @
--
-- @
-- Gamma [â1,â2,â = â1 & â2]
-- @
andifyVar :: Context -> String -> (Context, Type)
andifyVar ctx var =
  let
    (Context { syms = syms1 }, Context { syms = syms2 }) = splitContext ctx var
    name1 = var ++ "1"
    name2 = var ++ "2"
    evar1 = EvarT name1
    evar2 = EvarT name2
    a1 = (name1, evar1)
    a2 = (name2, evar2)
    at = AndT evar1 evar2
    a = (var, at)
  in
    (ctx { syms = syms1 ++ [a, a1, a2] ++ syms2 }, at)


occursContextT :: Context -> Type -> Type -> Bool
occursContextT ctx t1 t2
    | t1 /= t2 && not (isForallT t1) && not (isForallT t2) && (isEvarT t1 || isEvarT t2) =
        let
            t1' = substituteEvarTs ctx t1
            t2' = substituteEvarTs ctx t2
        in
          t1' /= DynT && t2' /= DynT && t1' /= t2' && (occursT t1' t2' || occursT t2' t1')
    | otherwise = False

-- / types and contexts


consJudgementM :: Monad m => String -> [Type] -> Type -> Type -> m ()
consJudgementM name evars t1 t2 =
    judgementM name
               (gamma "ctx1" vars |- t1 <: t2 -| "ctx2")
               (gamma "ctx1" vars |- t1 ~~ t2 -| "ctx2")
    where vars = [ (var, t) | t@(EvarT var) <- evars ]


consJudgementSymM name evars t1 t2 =
    judgementM name
               (gamma "ctx1" vars |- t1 >: t2 -| "ctx2")
               (gamma "ctx1" vars |- t1 ~~ t2 -| "ctx2")
    where vars = [ (var, t) | t@(EvarT var) <- evars ]


consJudgementDynM name evars t1 t2 =
    judgementM name
               (gamma "ctx" vars |- t1 <: t2 ++ "  " ++ gamma "ctx" vars |/- t1 >: t2)
               (gamma "ctx" vars |- t1 ~~ t2 -| gamma "ctx" dyns)
    where vars = [ (var, t) | t@(EvarT var) <- evars ]
          dyns = [ (var, DynT) | EvarT var <- evars ]


consistentT :: Context -> Type -> Type -> Maybe Context
consistentT ctx t1@(EvarT var1) t2@(EvarT var2)
  | not (isEmptyTypeContext ctx var1) && not (isEmptyTypeContext ctx var2) =
      let
          Right (ctx', t1') = typeContext ctx var1
          Right (ctx'', t2') = typeContext ctx' var2
      in
        case subT ctx'' t1' t2' of
          Nothing -> let ctx''' = updateContext ctx'' var2 DynT in
                     Just $ updateContext ctx''' var1 DynT
          x -> x
    where consistentLR ctx t1 t2 =
              do val <- subT ctx t1 t2
                 consJudgementM "~LR" [t1, t2] t1 t2
                 return val

          consistentSymLR ctx t1 t2 =
              do val <- subT ctx t2 t1
                 consJudgementSymM "~SymLR" [t1, t2] t1 t2
                 return val

          consistentDynLR ctx t1 t2 =
              do consJudgementDynM "~DynLR" [t1, t2] t1 t2
                 Just $ updateContext (updateContext ctx var1 DynT) var2 DynT

consistentT ctx t1@(EvarT var) t2
  | not (isEmptyTypeContext ctx var) =
    let Right (ctx', t1') = typeContext ctx var in
    case consistentL ctx' t1' t2 of
      val@(Just _) -> val
      Nothing -> case consistentSymL ctx' t1' t2 of
                   val@(Just _) -> val
                   Nothing -> consistentDynL ctx' t1' t2
    where consistentL ctx t1 t2 =
              do val <- subT ctx t1 t2
                 consJudgementM "~L" [t1] t1 t2
                 return val

          consistentSymL ctx t1 t2 =
              do val <- subT ctx t2 t1
                 consJudgementSymM "~SymL" [t1] t1 t2
                 return val

          consistentDynL ctx t1 t2 =
            do consJudgementDynM "~DynL" [t1] t1 t2
               Just $ updateContext ctx var DynT

consistentT ctx t1 t2@(EvarT var)
  | not (isEmptyTypeContext ctx var) =
    let Right (ctx', t2') = typeContext ctx var in
    case consistentR ctx' t1 t2' of
      val@(Just _) -> val
      Nothing -> case consistentSymR ctx' t1 t2' of
                   val@(Just _) -> val
                   Nothing -> consistentDynR ctx' t1 t2'
    where consistentR ctx t1 t2 =
              do val <- subT ctx t1 t2
                 consJudgementM "~R" [t2] t1 t2
                 return val

          consistentSymR ctx t1 t2 =
              do val <- subT ctx t2 t1
                 consJudgementSymM "~SymR" [t2] t1 t2
                 return val

          -- edit: replace DynT with the least upper bound (do the same for rule "~DynL")
          consistentDynR ctx t1 t2 =
              do judgementM "~DynR"
                            (gamma "ctx" [(var, t2)] |/- t1 <: t2 ++ "  " ++ gamma "ctx" [(var, t2)] |/- t1 >: t2)
                            (gamma "ctx" [(var, t2)] |- t1 ~~ var -| gamma "ctx" [(var, DynT)])
                 Just $ updateContext ctx var DynT


subT :: Context -> Type -> Type -> Maybe Context
-- subT ctx t1 t2
--   | trace ("subT: " ++ show t1 ++ " <: " ++ show t2 ++ "\n\n\t ctx = " ++ show ctx ++ "\n") False = undefined

subT ctx t1 t2
    | occursContextT ctx t1 t2 =
        throwTypecheckerException $ "occurs " ++ show t1 ++  " <: " ++ show t2

-- <:Refl
subT ctx t1 t2
    | isAtomicT t1 && t1 == t2 =
      do judgementM "<:Refl"
                    ""
                    ("ctx" |- t1 <: t2 -| "ctx")
         return ctx

-- <:And
subT ctx t1@(AndT a1 a2) t2@(AndT b1 b2) =
    do judgementM "<:And"
                  ("ctx1" |- a1 <: b1 -| "ctx2" ++ "  " ++ "ctx2" |- a2 <: b2 -| "ctx3")
                  ("ctx1" |- t1 <: t2 -| "ctx3")

       ctx' <- subT ctx a1 b1
       subT ctx' a2 b2

-- <:EvarAnd
subT ctx t1@(EvarT var) t2@AndT {}
  | isEmptyTypeContext ctx var =
    do judgementM "<:EvarAnd"
                  ("ctx1," ++ var ++ "1," ++ var ++ "2," ++ var ++ "=" ++ var ++ "1 & " ++ var ++ "2" |- t1 <: t2 -| "ctx2")
                  (gamma' "ctx1" [var] |- var <: t2 -| "ctx2")

       let (ctx', _) = andifyVar ctx var
       subT ctx' t1 t2

-- <:AndEvar
subT ctx t1@AndT {} t2@(EvarT var)
  | isEmptyTypeContext ctx var =
    do judgementM "<And:Evar"
                  ("ctx1," ++ var ++ "1," ++ var ++ "2," ++ var ++ "=" ++ var ++ "1 & " ++ var ++ "2" |- t1 <: t2 -| "ctx2")
                  (gamma' "ctx1" [var] |- t1 <: var -| "ctx2")

       let (ctx', _) = andifyVar ctx var
       subT ctx' t1 t2

-- <:Arrow
subT ctx t1@(ArrowT argT1 rangeT1) t2@(ArrowT argT2 rangeT2) =
    do judgementM "<:Arrow"
                  ("ctx1" |- argT2 <: argT1 -| "ctx2" ++ "  " ++ "ctx2" |- rangeT1 <: rangeT2 -| "ctx3")
                  ("ctx1" |- t1 <: t2 -| "ctx3")

       ctx' <- subT ctx argT2 argT1
       subT ctx' rangeT1 rangeT2

-- <:EvarArrow
subT ctx t1@(EvarT var) t2@ArrowT {}
  | isEmptyTypeContext ctx var =
      do judgementM "<:EvarArrow"
                    ("ctx1," ++ var ++ "1," ++ var ++ "2," ++ var ++ "=" ++ var ++ "1->" ++ var ++ "2 " |- t1 <: t2 -| "ctx2")
                    (gamma' "ctx1" [var] |- var <: t2 -| "ctx2")

         let (ctx', _) = arrowifyVar ctx var
         subT ctx' t1 t2

-- <:ArrowEvar
subT ctx t1@ArrowT {} t2@(EvarT var)
  | isEmptyTypeContext ctx var =
      do judgementM "<:ArrowEvar"
                    ("ctx1," ++ var ++ "1," ++ var ++ "2," ++ var ++ "=" ++ var ++ "1->" ++ var ++ "2 " |- t1 <: t2 -| "ctx2")
                    (gamma' "ctx1" [var] |- t1 <: var -| "ctx2")

         let (ctx', _) = arrowifyVar ctx var
         subT ctx' t1 t2

-- <:Tup
subT ctx t1@(TupT ts1) t2@(TupT ts2)
  | length ts1 == length ts2 =
    do judgementM "<:Tup"
                  ("ctx1" |- head ts1 <: head ts2 ++ " ... " ++ "ctxn" |- last ts1 <: last ts2 -| "ctxn+1")
                  ("ctx1" |- t1 <: t2 -| "ctxn+1")
       subT' ctx ts1 ts2
  where subT' ctx [] [] = return ctx
	subT' ctx (t1:ts1) (t2:ts2) =
    	  do ctx' <- subT ctx t1 t2
	     subT' ctx' ts1 ts2

-- <:EvarTup
subT ctx t1@(EvarT var) t2@(TupT ts)
  | isEmptyTypeContext ctx var =
    do judgementM "<:EvarTup"
                  (gamma' "ctx1" [var ++ "1,...," ++ var ++ "n," ++ var ++ "=(" ++ var ++ "1,..." ++ var ++ "n)"] |- t1 <: t2 -| "ctx2")
                  (gamma' "ctx1" [var] |- t1 <: t2 -| "ctx2")

       let (ctx', _) = tuplifyVar ctx var (length ts)
       subT ctx' t1 t2

-- <:TupEvar
subT ctx t1@(TupT ts) t2@(EvarT var)
  | isEmptyTypeContext ctx var =
    do judgementM "<:TupEvar"
                  (gamma' "ctx1" [var ++ "1,...," ++ var ++ "n," ++ var ++ "=(" ++ var ++ "1,..." ++ var ++ "n)"] |- t1 <: t2 -| "ctx2")
                  (gamma' "ctx1" [var] |- t1 <: t2 -| "ctx2")

       let (ctx', _) = tuplifyVar ctx var (length ts)
       subT ctx' t1 t2

-- <:List
subT ctx t1@(SeqT seqT1) t2@(SeqT seqT2) =
  do judgementM "<:List"
                ("ctx1" |- seqT1 <: seqT2 -| "ctx2")
                ("ctx1" |- t1 <: t2 -| "ctx2")

     subT ctx seqT1 seqT2

-- <:EvarList
subT ctx t1@(EvarT var) t2@SeqT {}
  | isEmptyTypeContext ctx var =
    do judgementM "<:EvarList"
                  ("ctx1," ++ var ++ "1," ++ var ++ "=[" ++ var ++ "1] " |- t1 <: t2 -| "ctx2")
                  (gamma' "ctx1" [var] |- var <: t2 -| "ctx2")

       let (ctx', _) = seqifyVar ctx var
       subT ctx' t1 t2

-- <:ListEvar
subT ctx t1@SeqT {} t2@(EvarT var)
  | isEmptyTypeContext ctx var =
    do judgementM "<:ListEvar"
                  ("ctx1," ++ var ++ "1," ++ var ++ "=[" ++ var ++ "1] " |- t1 <: t2 -| "ctx2")
                  (gamma' "ctx1" [var] |- t1 <: var -| "ctx2")

       let (ctx', _) = seqifyVar ctx var
       subT ctx' t1 t2

-- <:TupList
subT ctx t1@(TupT ts1) t2@(SeqT t) =
  do let t2' = TupT (replicate (length ts1) t)

     judgementM "<:TupList"
                ("ctx1" |- t1 <: t2' -| "ctx2")
                ("ctx1" |- t1 <: t2  -| "ctx2")

     subT ctx t1 t2'

-- <:ForallL
subT ctx t1@(ForallT var _) t2 =
  do let (ctx', t1') = eliminateForalls ctx t1
                
     judgementM "<:ForallL"
                ("ctx1,<|," ++ var |- subst ('^':var) var ++ t1' <: t2 -| "ctx2',<|,ctx2''")
                ("ctx1" |- t1 <: t2 -| "ctx2'")

     subT ctx' t1' t2

-- <:ForallR
subT ctx t1 t2@(ForallT var forallT) =
  do judgementM "<:ForallR"
                ("ctx1," ++ var |- t1 <: forallT -| "ctx2'," ++ var ++ ",ctx2''")
                ("ctx1" |- t1 <: t2 -| "ctx2'")

     let ctx' = insertContext ctx var (TvarT var)
     ctx'' <- subT ctx' t1 forallT
     return $ dropContext ctx'' var

-- <:ConsistentR
subT ctx t1 t2@(EvarT var)
  | not (isEmptyTypeContext ctx var) =
    do let Right (_, varT) = typeContext ctx var
       val <- consistentT ctx t1 t2

       judgementM "<:ConsistentR"
                  (gamma "ctx1" [(var, varT)] |- t1 ~~ t2 -| "ctx2")
                  (gamma "ctx1" [(var, varT)] |- t1 <: t2 -| "ctx2")

       return val

-- <:ConsistentL
subT ctx t1@(EvarT var) t2
  | not (isEmptyTypeContext ctx var) =
    do let Right (_, varT) = typeContext ctx var
       val <- consistentT ctx t1 t2

       judgementM "<:ConsistentL"
                  (gamma "ctx1" [(var, varT)] |- t1 ~~ t2 -| "ctx2")
                  (gamma "ctx1" [(var, varT)] |- t1 <: t2 -| "ctx2")

       return val

-- <:InstR
subT ctx t1 t2@(EvarT var)
  | isEmptyTypeContext ctx var && isAtomicT t1 && isWellformed ctx t2 t1 =
    do judgementM "<:InstR"
                  ("ctx'" |- show t1 ++ " wf")
                  ("ctx'," ++ var ++ ",ctx''" |- t1 <: t2 -| "ctx'," ++ var ++ "=" ++ show t1 ++ ",ctx''")

       return $ updateContext ctx var t1

-- <:InstL
subT ctx t1@(EvarT var) t2
  | debugRule ("looking at rule '<:InstL'")
    [("context empty (isEmptyTypeContext ctx " ++ var ++ ")", isEmptyTypeContext ctx var),
     ("t2 is atomic (isAtomicT " ++ show t2 ++ ")", isAtomicT t2),
     ("context is wellformed (isWellformed " ++ show ctx ++ " " ++ show t1 ++ " " ++ show t2 ++ ")", isWellformed ctx t1 t2)] = undefined

subT ctx t1@(EvarT var) t2
  | isEmptyTypeContext ctx var && isAtomicT t2 && isWellformed ctx t1 t2 =
    do judgementM "<:InstL"
                  ("ctx'" |- show t2 ++ " wf")
                  ("ctx'," ++ var ++ ",ctx''" |- t1 <: t2 -| "ctx'," ++ var ++ "=" ++ show t2 ++ ",ctx''")

       return $ updateContext ctx var t2

-- <:Dyn
subT ctx t1 t2@DynT =
  do judgementM "<:Dyn"
                ""
                ("ctx" |- t1 <: t2 -| "ctx")

     return ctx

subT _ t1 t2
  | trace ("No subtype rule for: " ++ show t1 ++ " <: " ++ show t2) False = undefined

subT _ _ _ = Nothing


subM :: Context -> Type -> Type -> Either String Context
subM ctx t1 t2 =
  case subT ctx t1 t2 of
    Nothing -> Left $ "\n\n\ttype inconsistency: " ++ show t1 ++ " <: " ++ show t2 ++ " (false)\n"
    Just ctx' -> Right ctx'


type TypecheckerM a = Either String a
type SynthM = TypecheckerM (Context, Expr, Type)
type CheckM = TypecheckerM (Context, Expr)


synthVals
  :: Context
  -> (Context
  -> a
  -> TypecheckerM (Context, a, Type))
  -> [a]
  -> TypecheckerM (Context, [a], [Type])
synthVals ctx m vals = synthVals' vals ctx [] []
  where synthVals' [] ctx exprs ts =
          return (ctx, reverse exprs, reverse ts)
        synthVals' (x:xs) ctx exprs ts =
          do (ctx', expr, t) <- m ctx x
             synthVals' xs ctx' (expr:exprs) (t:ts)


synthM :: Context -> Expr -> SynthM
synthM ctx expr = synthCast . synthSubst <$> synthAbstrM ctx expr
    where -- ⇑Subst
          synthSubst (ctx', expr', t@(EvarT var))
              | not (isEmptyTypeContext ctx' var) =
                  let t' = substituteEvarTs ctx' t in
                  judgement "=>Subst"
                            ("ctx1 |- " ++ showAbbrev expr ++ " => " ++ show t ++ " -| ctx2")
                            ("ctx1 |- " ++ showAbbrev expr ++ " => " ++ show t' ++ " -| ctx2")
                            $
                  (ctx', expr', t')

          synthSubst val = val

          synthCast (ctx', expr'@CastE {}, t) = (ctx', expr', rebuildForallT t)
          synthCast (ctx', expr', t) = (ctx', CastE t expr', t)


synthAbstrM :: Context -> Expr -> SynthM

-- ⇑Const (Char)
synthAbstrM ctx expr@(CharE c) =
  do let t = CharT

     judgementM "=>Const"
                ("ctx" |- show expr ++ ":" ++ show t)
                ("ctx" |- expr `synth` t -| "ctx")

     return (ctx, CharE c, t)

-- ⇑Const (Int)
synthAbstrM ctx expr@(IntE i) =
  do let t = IntT

     judgementM "=>Const"
                ("ctx" |- show expr ++ ":" ++ show t)
                ("ctx" |- expr `synth` t -| "ctx")

     return (ctx, IntE i, t)

-- ⇑Const (Real)
synthAbstrM ctx expr@(RealE d) =
  do let t = DoubleT

     judgementM "=>Const"
                ("ctx" |- show expr ++ ":" ++ show t)
                ("ctx" |- expr `synth` t -| "ctx")

     return (ctx, RealE d, t)

-- ⇑Seq
synthAbstrM ctx expr@(SeqE exprs) =
  do (ctx', exprs', ts) <- synthSeq ctx [] [] exprs

     judgementM "=>Seq"
                "ctx1 |- ... -| ctxn+1"
                ("ctx1" |- expr `synth` TupT ts -| "ctxn+1")

     return (ctx', SeqE exprs', kickForalls (TupT ts))
  where synthSeq ctx exprs ts [] =
          return (ctx, reverse exprs, reverse ts)

        synthSeq ctx exprs ts (seqExpr:seqExprs) =
          do (ctx', expr', t) <- synthM ctx seqExpr
             synthSeq ctx' (expr':exprs) (t:ts) seqExprs

-- ⇑Var
synthAbstrM ctx expr@(IdE name) =
  do (ctx', t) <- typeContext ctx (QualName.fromQualName name)

     judgementM "=>Var"
                ("ctx(" ++ showAbbrev expr ++ ") = " ++ show t)
                ("ctx" |- expr `synth` t -| "ctx")

     return (ctx', IdE name, t)

-- ⇑Lambda (annotated)
synthAbstrM ctx expr@(LambdaE arg (Just ann) body) =
  do (ctx', argT) <- typeContext ctx ann
     (ctx'', body', rangeT) <- synthAbstrM (insertContext ctx' arg argT) body
       
     judgementM "=>Lambda" "" ""

     return (ctx'', LambdaE arg (Just ann) body', ArrowT argT rangeT)

-- synthAbstrM syms Synth expr@(LambdaExpr arg body) =
--     error "synthAbstrM: LambdaExpr (annotated): Synth: not implemented"

-- ⇑Lambda (unannotated) no rule in Pierce's paper, use Siek instead
-- synthAbstrM ctx (LambdaExpr arg body) =
--     do let arg' = '^':arg
--            argT = EvarT arg'
--            ctx' = insertContext ctx arg' (simpleType argT)
--            ctx'' = insertContext ctx' arg (simpleType argT)
--        (bodyT, body', ctx''') <- synthM ctx'' body
--        return (ArrowT argT bodyT, LambdaExpr arg body', ctx''')

-- ⇑LetChk
synthAbstrM ctx expr@(AppE fn@(LambdaE x Nothing body) arg@LambdaE {}) =
  do let (evarT@(EvarT evar), ctx') = genEvar ctx

     (ctx'', arg') <- checkM ctx' arg evarT
     (ctx''', body', rangeT) <- synthM (insertContext ctx'' x evarT) body

     judgementM "=>LetChk"
                ("ctx1," ++ evar |- arg <= evarT -| "ctx2  ctx2," ++ x ++ ":^a" |- body `synth` rangeT -| "ctx3")
                ("ctx1" |- expr `synth` rangeT -| "ctx3")

     return (ctx''', AppE (LambdaE x Nothing body') arg', rangeT)

-- ⇑LetSyn
synthAbstrM ctx expr@(AppE fn@(LambdaE x Nothing body) arg) =
  do (ctx', arg', argT) <- synthM ctx arg
     (ctx'', body', rangeT) <- synthM (insertContext ctx' x argT) body

     judgementM "=>LetSyn"
                ("ctx1" |- arg `synth` argT -| "ctx2  ctx2," ++ x ++ ":" ++ show argT |- body `synth` rangeT -| "ctx3")
                ("ctx1" |- expr `synth` rangeT -| "ctx3")

     return (ctx'', AppE (LambdaE x Nothing body') arg', rangeT)

-- ⇑Forall
-- ⇑AppArrow
-- ⇑AppEvar
-- ⇑AppDyn
synthAbstrM ctx expr@(AppE fn arg) =
  do (ctx', fn', t) <- synthForallM ctx fn
     synthApp t fn' ctx'
  where -- ⇑Forall
        synthForallM :: Context -> Expr -> SynthM
        synthForallM ctx expr =
          do (ctx', expr', t') <- synthM ctx expr
             let (ctx'', t'') = eliminateForalls ctx' t'

             when (isForallT t') $
               judgementM "=>Forall"
                          ("ctx1" |- expr `synth` t' -| "ctx2")
                          ("ctx1" |- expr `synth` t'' -| "ctx2,...")

             return (ctx'', expr', t'')

        -- ⇑AppArrow
        synthApp t@(ArrowT argT rangeT) fn' ctx =
          do (ctx', arg') <- checkM ctx arg argT

             judgementM "=>AppArrow"
                        ("ctx1" |- fn `synth` t -| "ctx2  ctx2" |- arg <= argT -| "ctx3")
                        ("ctx1" |- expr `synth` rangeT -| "ctx3")

             return (ctx', AppE fn' arg', rangeT)

        -- ⇑AppEvar
        synthApp t@(EvarT var) fn' ctx =
          do let (ctx', t'@(ArrowT argT rangeT)) = arrowifyVar ctx var

             judgementM "=>AppEvar"
                        ("ctx1" |- fn `synth` t -| gamma' "ctx2" [var] ++ gamma'' "ctx2" [(argT, Nothing), (rangeT, Nothing), (t, Just t')] |- arg <= argT -| "ctx3")
                        ("ctx1" |- expr `synth` rangeT -| "ctx3")

             (ctx'', arg') <- checkM ctx' arg argT
             return (ctx'', AppE fn' arg', rangeT)

        -- ⇑AppDyn
        synthApp t@DynT fn' ctx =
          do let argT = DynT
                 rangeT = DynT
             (ctx', arg') <- checkM ctx arg argT

             judgementM "=>AppDyn"
                        ("ctx1" |- fn `synth` t -| "ctx2  ctx2" |- arg <= argT -| "ctx3")
                        ("ctx1" |- expr `synth` rangeT -| "ctx3")

             return (ctx', AppE fn' arg', rangeT)

        synthApp t fn' ctx =
          error $ "synthApp: unhandled case" ++
                  "\n\n\t t = " ++ show t ++
                  "\n\n\t fn' = " ++ show fn' ++
                  "\n\n\t ctx = " ++ show ctx ++ "\n"

-- ⇑Cast
synthAbstrM ctx (CastE typ expr) =
  do let (ctx', typ') = freshType ctx typ

     judgementM "=>Cast"
                ("ctx1" |- expr <= typ' -| "ctx2")
                ("ctx1" |- expr `synth` typ' -| "ctx2")

     (ctx'', expr') <- checkM ctx' expr typ'
     return (ctx'', CastE typ' expr', typ')

-- edit: this is untested
synthAbstrM ctx (CondE ms blame) =
  do (ctx', ms', ts) <- synthMs ms ctx [] []
     case consistentTs ctx' (head ts) (tail ts) of
       Just (ctx'', t) -> return (ctx'', CondE ms' blame, t)
  where synthMs [] ctx vals ts = return (ctx, reverse vals, reverse ts)
        synthMs ((expr1, expr2):ms) ctx vals ts =
            do (ctx', expr1') <- checkM ctx expr1 BoolT
               (ctx'', expr2', t) <- synthM ctx' expr2
               synthMs ms ctx'' ((expr1', expr2'):vals) (t:ts)

        consistentTs ctx t1 [] = return (ctx, t1)
        consistentTs ctx t1 (t2:ts) =
          do ctx' <- subT ctx t1 t2
             consistentTs ctx' t1 ts

synthAbstrM ctx (FnDecl Def name body) =
  error "Typechecker.synthAbstrM(FnDecl Def ...): recursive functions must be eliminated in previous stages"
    -- do (ctx', body', bodyT) <- synthM (insertContext ctx name DynT) body
    --    let bodyT' = rebuildForallT bodyT
    --    return (insertContext ctx' name bodyT', FnDecl Def name body', bodyT')

synthAbstrM ctx (FnDecl NrDef name body) =
    do (ctx', body', bodyT) <- synthM ctx body
       let bodyT' = rebuildForallT (substituteEvarTs ctx' bodyT)
       return (insertContext ctx' name bodyT', FnDecl NrDef name body', bodyT')

synthAbstrM ctx (MergeE obs) =
  do (ctx', exprs) <- synthObsM [] ctx obs
     let obs' = [ (name, ob) | (name, ob, _) <- exprs ]
         ts = [ (name, t) | (name, _, t) <- exprs ]
     return (ctx', MergeE obs', CoT ts)
  where synthObsM exprs ctx [] = return (ctx, reverse exprs)
        synthObsM exprs ctx ((name, expr):obs) =
          do (ctx', expr', t) <- synthM ctx expr
             synthObsM ((name, expr', t):exprs) ctx' obs

synthAbstrM ctx (WhereE expr exprs) =
  do (ctx', exprs', _) <- synthVals ctx synthM exprs
     (ctx'', expr', t) <- synthM ctx' expr
     return (ctx'', WhereE expr' exprs', t)

synthAbstrM _ expr =
    Left $ "\n\n\tsynthAbstrM: unhandled case" ++
           "\n\n\t expr = " ++ show expr ++ "\n"


-- edit: check (EvarT var) is necessary for lambda terms (Joshua said)
-- but it conflicts with the following examples
--
--  ap:1:2.0
--
-- because the derivation rules must reach the consistency relation
-- and this substitution makes it impossible.  Perhaps this rule
-- needs only to be applied around lambda terms...
checkM :: Context -> Expr -> Type -> CheckM
-- checkM t@(EvarT var) ctx expr | not (isEmptyTypeContext ctx var) =
--   do let Right (ctx', t') = typeContext ctx var

--      let !() = judgementM "<=Subst"
--                            ("ctx1 |- " ++ showAbbrev expr ++ " <= " ++ show t' ++ " -| ctx2")
--                            ("ctx1 |- " ++ showAbbrev expr ++ " <= " ++ show t ++ " -| ctx2")

--      checkInstM t' ctx' expr

-- checkM ctx _ _ | trace ("checkM: " ++ show ctx ++ "\n") False = undefined

checkM ctx expr t = checkInstM ctx expr t


checkInstM :: Context -> Expr -> Type -> CheckM

-- ⇓Forall
checkInstM ctx expr t@(ForallT var forallT) | isValueE expr =
  do judgementM "<=Forall"
                ("ctx1," ++ var |- expr <= forallT -| "ctx2'," ++ var ++ ",ctx2''")
                ("ctx1" |- expr <= t -| "ctx2'")

     let ctx' = insertContext ctx var (TvarT var)
     (ctx'', expr') <- checkM ctx' expr forallT
     return (dropContext ctx'' var, expr')

-- ⇓LambdaArrow
checkInstM ctx expr@(LambdaE arg Nothing body) t@(ArrowT argT rangeT) =
    do judgementM "<=LambdaArrow"
                  ("ctx1," ++ arg ++ ":" ++ show argT |- body <= rangeT -| "ctx2'," ++ arg ++ ":" ++ show argT ++ ",ctx2''")
                  ("ctx1" |- expr <= t -| "ctx2'")

       (ctx', body') <- checkM (insertContext ctx arg argT) body rangeT
       return (dropContext ctx' arg, LambdaE arg Nothing body')

-- ⇓LambdaEvar
checkInstM ctx expr@LambdaE {} (EvarT var)
    | isEmptyTypeContext ctx var =
        let (ctx', existT) = arrowifyVar ctx var in
        checkInstM ctx' expr existT

-- ⇓LambdaDyn
checkInstM ctx expr@(LambdaE arg Nothing body) t@DynT =
    do let t' = ArrowT t t

       judgementM "<=LambdaDyn"
                  ("ctx1" |- expr <= t' -| "ctx2")
                  ("ctx1" |- expr <= t -| "ctx2")

       (ctx', body') <- checkM (insertContext ctx arg t) body t
       return (ctx', LambdaE arg Nothing body')

-- ⇓SeqTup
checkInstM ctx expr@(SeqE exprs) t@(TupT ts) =
  do val <- checkSeqM ctx exprs ts

     judgementM "<=SeqTup"
                ("ctx1" |- "..." -| "ctx2  ...  ctxn" |- "..." -| "ctxn+1")
                ("ctx1" |- expr <= t -| "ctxn+1")

     return val
  where msg =
          "\n\n\tcheckInstM: SeqExpr: different length" ++
          "\n\n\t ts = " ++ show ts ++
          "\n\n\t t = " ++ show t ++
          "\n\n\t exprs = " ++ show exprs ++ "\n"

        checkSeqM ctx exprs ts = check [] ts ctx exprs
            where check exprs [] ctx [] = return (ctx, SeqE (reverse exprs))
                  check _ _ _ [] = Left msg
                  check _ [] _ _ = Left msg
                  check exprs (t:ts) ctx (expr:exprs') =
                    do (ctx', expr') <- checkM ctx expr t
                       check (expr':exprs) ts ctx' exprs'

-- ⇓SeqList
checkInstM ctx expr@(SeqE exprs) t@(SeqT seqT) =
    do let t' = TupT (replicate (length exprs) seqT)

       judgementM "<=SeqList"
                  ("ctx1" |- expr <= t' -| "ctx2")
                  ("ctx1" |- expr <= t -| "ctx2")

       checkInstM ctx expr t'

-- ⇓SeqDyn
checkInstM ctx expr@SeqE {} t@DynT =
    do let t' = SeqT DynT

       judgementM "<=SeqDyn"
                  ("ctx1" |- expr <= t' -| "ctx2")
                  ("ctx1" |- expr <= t -| "ctx2")

       checkInstM ctx expr t'

-- ⇓Others
checkInstM ctx (CondE ms blame) t =
  do judgementM "<=Cond" "" ""

     (ctx', ms') <- checkMs ms ctx []
     return (ctx', CondE ms' blame)
  where checkMs [] ctx vals = return (ctx, reverse vals)
        checkMs ((expr1, expr2):ms) ctx vals =
          do (ctx', expr1') <- checkM ctx expr1 BoolT
             (ctx'', expr2') <- checkM ctx' expr2 t
             checkMs ms ctx'' ((expr1', expr2'):vals)

checkInstM ctx (FnDecl Def name body) t =
  error "Typechecker.checkInstM(FnDecl Def ...): recursive functions must be eliminated in previous stages"
    -- do (ctx', body') <- checkM (insertContext ctx name t) body t

    --    judgementM "<=Defn"
    --               ("<=" ++ show t)
    --               ""

    --    return (ctx', FnDecl Def name body')


-- edit: the type inserted in the context and the type checked against
-- are not the same.  This needs testing.
checkInstM ctx (FnDecl NrDef name body) t =
  do (ctx', body') <- checkM ctx body t
     let bodyT = substituteEvarTs ctx' t

     judgementM "<=Defn"
                ("<=" ++ show bodyT)
                ""

     return (insertContext ctx' name bodyT, FnDecl NrDef name body')

checkInstM ctx (WhereE expr exprs) t =
  do (ctx', exprs', _) <- synthVals ctx synthM exprs
     (ctx'', expr') <- checkM ctx' expr t
     return (ctx'', WhereE expr' exprs')

-- <=Sub
checkInstM ctx expr t =
  do (ctx', expr', t') <- synthAbstrM ctx expr

     judgementM "<=Sub"
                ("ctx1" |- expr `synth` t'  -| "ctx2  ctx2" |- t' <: t |- "ctx3")
                ("ctx1" |- expr <= t -| "ctx3")

     (,expr') <$> subM ctx' t' t


typecheckSubstitute :: Context -> Expr -> TypecheckerM (Context, Expr, Type)
typecheckSubstitute ctx expr =
  do (ctx', expr', t) <- synthM ctx expr
     let !_ | debugT ("type before the final substitution: " ++ show t) = True
     return (ctx', expr', substituteEvarTs ctx' t)

-- edit: to remove
fromRight (Right x) = x

typecheckDefinitionM :: FileSystem -> Definition -> TypecheckerM Definition
typecheckDefinitionM fs def@Definition { renExpr = Just expr } =
  do let defs = map (FileSystem.definition fs) (freeNames def)
         typs = [ (sym, fromRight (Definition.typ def)) | def <- defs, let Just (FnSymbol sym) = Definition.symbol def ]
     case typecheckSubstitute (nothingSyms $ Map.fromList typs) expr of
       Left err -> return def { typ = Left err }
       Right (_, expr', t) -> return def { typ = Right t, typExpr = Just expr' }


typecheckDefinitionsM :: FileSystem -> SrcFile -> [Definition] -> TypecheckerM SrcFile
typecheckDefinitionsM _ srcfile [] = return srcfile

typecheckDefinitionsM fs srcfile (def:defs) =
  do def' <- typecheckDefinitionM fs def
     let srcfile' = SrcFile.updateDefinitions srcfile [def']
         fs' = FileSystem.add fs srcfile'
     typecheckDefinitionsM fs' srcfile' defs


typecheck :: FileSystem -> SrcFile -> Either String SrcFile
typecheck _ srcfile@SrcFile { t = CoreT } =
    return srcfile

typecheck fs srcfile =
    typecheckDefinitionsM fs srcfile (SrcFile.defsAsc srcfile)