{-# LANGUAGE BangPatterns, NamedFieldPuns,
             TupleSections #-}
module Typechecker where

import Prelude hiding ((<=))

import Control.Monad (liftM, when)
import Data.Functor ((<$>))
import Data.List (intercalate, isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)

import Config
import Data.Context
import qualified Data.Context as Context
import Data.SrcFile
import qualified Data.SrcFile as SrcFile
import Data.Exception
import Data.Type
import Data.Stx
import Data.Type

import Debug.Trace
import System.IO.Unsafe


debug = True
debugT desc = (debug && trace desc True) || True


debugConsistentT = True
debugConsistentTF desc = debugConsistentT && trace desc False
debugConsistentTT desc = (debugConsistentT && trace desc True) || True


logT desc t1 t2 =
    debugConsistentTT $ desc ++ ": " ++ show t1 ++ " <: " ++ show t2


judgement :: String -> String -> String -> ()
judgement name str1 str2 =
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


judgementM :: Monad m => String -> String -> String -> m ()
judgementM name str1 str2 =
    let !val = judgement name str1 str2 in return val


-- edit: maybe make (String, a) into (a, a) or (a, b)
-- and force uses of gamma to pass in types always instead of vars
gamma :: Show a => String -> [(String, a)] -> String
gamma name vars =
    name ++ intercalate "" (map (\(var, t) -> "[" ++ var ++ "=" ++ show t ++ "]") vars)


gamma' :: String -> [String] -> String
gamma' name vars =
    name ++ concat (map (\var -> "[" ++ var ++ "]") vars)


gamma'' name ts =
    name ++ concat (map assign ts)
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

genEvar ctx =
    let
        evar = ['^', toEnum (count ctx)]
        evarT = ExistT evar
        ctx' = insertContext ctx { count = count ctx + 1 } evar (simpleType evarT)
    in
      (evarT, ctx')


eliminateForalls :: Context -> Type -> (Context, Type)
eliminateForalls ctx (ForallT var forallT) =
  let
    var' = '^':var
    existT = ExistT var'
    ctx' = insertContext ctx var' (simpleType existT)
    forallT' = substituteTvarT existT var forallT
  in
   eliminateForalls ctx' forallT'

eliminateForalls ctx t = (ctx, t)


substituteExistTs :: Context -> Type -> Type
substituteExistTs _ t@BoolT = t
substituteExistTs _ t@IntT  = t
substituteExistTs _ t@DoubleT = t
substituteExistTs _ t@CharT = t
substituteExistTs ctx (TupT ts) = TupT $ map (substituteExistTs ctx) ts
substituteExistTs ctx (SeqT t) = SeqT $ substituteExistTs ctx t
substituteExistTs ctx t@DynT = t

substituteExistTs ctx (ArrowT t1 t2) =
  ArrowT (substituteExistTs ctx t1) (substituteExistTs ctx t2)

substituteExistTs ctx t@(ExistT var) =
  case lookupContext ctx var of
    Nothing -> error $ "Typechecker.substituteExistTs: " ++ show var
    Just (ExistT var', Nothing) | var == var' -> t
    Just (t', Nothing) -> substituteExistTs ctx t'
    Just (_, Just t') -> substituteExistTs ctx t'

substituteExistTs ctx (ForallT vars t) = ForallT vars $ substituteExistTs ctx t
substituteExistTs ctx t@(TvarT _) = t


arrowifyVar :: Context -> String -> (Type, Context)
arrowifyVar ctx var =
  let
    (Context { syms = syms1 }, Context { syms = syms2 }) = splitContext ctx var
    name1 = var ++ "1"
    name2 = var ++ "2"
    a1 = (name1, (ExistT name1, Nothing))
    a2 = (name2, (ExistT name2, Nothing))
    at = ArrowT (ExistT name1) (ExistT name2)
    a  = (var, (ExistT var, Just at))
  in
   (at, ctx { syms = syms1 ++ [a, a2, a1] ++ syms2 })


occursContextT :: Context -> Type -> Type -> Bool
occursContextT ctx t1 t2
  | t1 /= t2 && not (isForallT t1) && not (isForallT t2) && (isExistT t1 || isExistT t2) =
    let
      t1' = substituteExistTs ctx t1
      t2' = substituteExistTs ctx t2
    in
     t1' /= t2' && (occursT t1' t2' || occursT t2' t1')
  | otherwise = False

-- / types and contexts


consJudgementM :: Monad m => String -> [Type] -> Type -> Type -> m ()
consJudgementM name evars t1 t2 =
    judgementM name
               (gamma "ctx1" vars |- t1 <: t2 -| "ctx2")
               (gamma "ctx1" vars |- t1 ~~ t2 -| "ctx2")
    where vars = [ (var, t) | t@(ExistT var) <- evars ]


consJudgementSymM name evars t1 t2 =
    judgementM name
               (gamma "ctx1" vars |- t1 >: t2 -| "ctx2")
               (gamma "ctx1" vars |- t1 ~~ t2 -| "ctx2")
    where vars = [ (var, t) | t@(ExistT var) <- evars ]


consJudgementDynM name evars t1 t2 =
    judgementM name
               (gamma "ctx" vars |- t1 <: t2 ++ "  " ++ gamma "ctx" vars |/- t1 >: t2)
               (gamma "ctx" vars |- t1 ~~ t2 -| gamma "ctx" dyns)
    where vars = [ (var, t) | t@(ExistT var) <- evars ]
          dyns = [ (var, DynT) | ExistT var <- evars ]


consistentT :: Context -> Type -> Type -> Maybe Context
consistentT ctx t1@(ExistT var1) t2@(ExistT var2)
  | not (isEmptyTypeContext ctx var1) && not (isEmptyTypeContext ctx var2) && logT "(~LR)" t1 t2 =
      let
          Right (ctx', t1') = typeContext ctx var1
          Right (ctx'', t2') = typeContext ctx' var2
      in
        case subT ctx'' t1' t2' of
          Nothing -> let ctx''' = updateContext ctx' var2 (unifType t2 DynT) in
                     Just $ updateContext ctx''' var1 (unifType t1 DynT)
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
                 Just $ updateContext (updateContext ctx var1 (unifType t1 DynT)) var2 (unifType t2 DynT)

consistentT ctx t1@(ExistT var) t2
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
               Just $ updateContext ctx var (unifType t1 DynT)

consistentT ctx t1 t2@(ExistT var)
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

          consistentDynR ctx t1 t2 =
              do judgementM "~DynR"
                            (gamma "ctx" [(var, t2)] |/- t1 <: t2 ++ "  " ++ gamma "ctx" [(var, t2)] |/- t1 >: t2)
                            (gamma "ctx" [(var, t2)] |- t1 ~~ var -| gamma "ctx" [(var, DynT)])
                 Just $ updateContext ctx var (unifType t2 DynT)


subT :: Context -> Type -> Type -> Maybe Context
subT ctx t1 t2
  | occursContextT ctx t1 t2 =
    throwTypecheckerException $ "occurs " ++ show t1 ++  " <: " ++ show t2

-- <Refl
subT ctx t1 t2
    | isAtomicT t1 && t1 == t2 =
      do judgementM "<Refl"
                    ""
                    ("ctx" |- t1 <: t2 -| "ctx")
         return ctx

-- <TupTup
subT ctx t1@(TupT ts1) t2@(TupT ts2)
    | length ts1 == length ts2 && logT "(tup ~ tup)" t1 t2 =
        subT' ctx ts1 ts2
    where subT' ctx [] [] = return ctx
	  subT' ctx (t1:ts1) (t2:ts2) =
    	      do ctx' <- subT ctx t1 t2
	      	 subT' ctx' ts1 ts2

-- <TupSeq
subT ctx t1@(TupT ts1) t2@(SeqT t) =
    do let t2' = TupT (replicate (length ts1) t)

       judgementM "<TupSeq"
                  ("ctx1" |- t1 <: t2' -| "ctx2")
                  ("ctx1" |- t1 <: t2  -| "ctx2")

       subT ctx t1 t2'

-- <SeqSeq
subT ctx t1@(SeqT seqT1) t2@(SeqT seqT2) =
     do judgementM "<SeqSeq"
                   ("ctx1" |- seqT1 <: seqT2 -| "ctx2")
                   ("ctx1" |- t1 <: t2 -| "ctx2")

        subT ctx seqT1 seqT2

-- <Arrow
subT ctx t1@(ArrowT argT1 rangeT1) t2@(ArrowT argT2 rangeT2) =
    do judgementM "<Arrow"
                  ("ctx1" |- argT2 <: argT1 -| "ctx2" ++ "  " ++ "ctx2" |- rangeT1 <: rangeT2 -| "ctx3")
                  ("ctx1" |- t1 <: t2 -| "ctx3")

       ctx' <- subT ctx argT2 argT1
       subT ctx' rangeT1 rangeT2

-- <EvarArrow
subT ctx t1@(ExistT var) t2@(ArrowT _ _)
  | isEmptyTypeContext ctx var =
      do judgementM "<EvarArrow"
                    ("ctx1," ++ var ++ "1," ++ var ++ "2," ++ var ++ "=" ++ var ++ "1->" ++ var ++ "2 " |- t1 <: t2 -| "ctx2")
                    (gamma' "ctx1" [var] |- var <: t2 -| "ctx2")

         let (_, ctx') = arrowifyVar ctx var
         subT ctx' t1 t2

-- <ArrowEvar
subT ctx t1@(ArrowT _ _) t2@(ExistT var)
  | isEmptyTypeContext ctx var =
      do judgementM "<ArrowEvar"
                    ("ctx1," ++ var ++ "1," ++ var ++ "2," ++ var ++ "=" ++ var ++ "1->" ++ var ++ "2 " |- t1 <: t2 -| "ctx2")
                    (gamma' "ctx1" [var] |- t1 <: var -| "ctx2")

         let (_, ctx') = arrowifyVar ctx var
         subT ctx' t1 t2

-- <ForallL
subT ctx t1@(ForallT var _) t2 =
      do let (ctx', t1') = eliminateForalls ctx t1
                
         judgementM "<ForallL"
                    ("ctx1,<|," ++ var |- subst ('^':var) var ++ t1' <: t2 -| "ctx2',<|,ctx2''")
                    ("ctx1" |- t1 <: t2 -| "ctx2'")

         subT ctx' t1' t2

-- ForallR
subT ctx t1 t2@(ForallT var forallT) =
    do judgementM "<ForallR"
                  ("ctx1," ++ var |- t1 <: forallT -| "ctx2'," ++ var ++ ",ctx2''")
                  ("ctx1" |- t1 <: t2 -| "ctx2'")

       let ctx' = insertContext ctx var (simpleType (TvarT var))
       ctx'' <- subT ctx' t1 forallT
       return $ dropContext ctx'' var

-- <ConsistentR
subT ctx t1 t2@(ExistT var)
  | not (isEmptyTypeContext ctx var) =
      do let Right (_, varT) = typeContext ctx var
         val <- consistentT ctx t1 t2

         judgementM "<ConsistentR"
                    (gamma "ctx1" [(var, varT)] |- t1 ~~ t2 -| "ctx2")
                    (gamma "ctx1" [(var, varT)] |- t1 <: t2 -| "ctx2")
                    
         return val

-- <ConsistentL
subT ctx t1@(ExistT var) t2
  | not (isEmptyTypeContext ctx var) =
      do let Right (_, varT) = typeContext ctx var
         val <- consistentT ctx t1 t2

         judgementM "<ConsistentL"
                    (gamma "ctx1" [(var, varT)] |- t1 ~~ t2 -| "ctx2")
                    (gamma "ctx1" [(var, varT)] |- t1 <: t2 -| "ctx2")

         return val

-- <InstR
subT ctx t1 t2@(ExistT var)
    | isEmptyTypeContext ctx var && isAtomicT t1 && isWellformed ctx t2 t1 =
      do judgementM "<InstR"
                    ("ctx'" |- show t1 ++ " wf")
                    ("ctx'," ++ var ++ ",ctx''" |- t1 <: var -| "ctx'," ++ var ++ "=" ++ show t1 ++ ",ctx''")

         return $ updateContext ctx var (unifType t2 t1)

-- <InstL
subT ctx t1@(ExistT var) t2
    | isEmptyTypeContext ctx var && isAtomicT t2 && isWellformed ctx t1 t2 =
      do judgementM "<InstL"
                    ("ctx'" |- show t2 ++ " wf")
                    ("ctx'," ++ var ++ ",ctx''" |- t2 <: var -| "ctx'," ++ var ++ "=" ++ show t2 ++ ",ctx''")

         return $ updateContext ctx var (unifType t1 t2)

subT ctx t1 t2@DynT =
    do judgementM "<Dyn"
                  ""
                  ("ctx" |- t1 <: t2 -| "ctx")

       return ctx

subT _ _ _ = Nothing


subM :: Context -> Type -> Type -> Either String Context
subM ctx t1 t2 =
  case subT ctx t1 t2 of
    Nothing -> Left $ "\n\n\ttype inconsistency: " ++ show t1 ++ " <: " ++ show t2 ++ " (false)\n"
    Just ctx' -> Right ctx'


type TypecheckerM a = Either String a
type SynthM = TypecheckerM (Type, Stx (String, Type, Type), Context)
type CheckM = TypecheckerM (Stx (String, Type, Type), Context)


typecheckWhereM :: (Context -> Stx String -> TypecheckerM a) -> Context -> Stx String -> TypecheckerM a
typecheckWhereM m syms (WhereStx stx (stxs)) =
    check syms stxs
    where check syms [] = m syms stx
          check syms (stx:stxs) =
              do (_, _, syms') <- synthM syms stx
                 check syms' stxs


synthM :: Context -> Stx String -> SynthM
synthM ctx stx = synthSubst <$> synthAbstrM ctx stx
    where -- ⇑Subst
          synthSubst (t@(ExistT var), stx', ctx')
              | not (isEmptyTypeContext ctx' var) =
                  let
                      t' = substituteExistTs ctx' t
                      !() = judgement "=>Subst"
                                      ("ctx1 |- " ++ showAbbrev stx ++ " => " ++ show t ++ " -| ctx2")
                                      ("ctx1 |- " ++ showAbbrev stx ++ " => " ++ show t' ++ " -| ctx2")
                  in
                    (t', stx', ctx')

          synthSubst val = val


synthAbstrM :: Context -> Stx String -> SynthM

-- ⇑Const (Char)
synthAbstrM ctx stx@(CharStx c) =
    do let t = CharT

       judgementM "=>Const"
                  ("ctx" |- show stx ++ ":" ++ show t)
                  ("ctx" |- stx `synth` t -| "ctx")

       return (t, CharStx c, ctx)

-- ⇑Const (Int)
synthAbstrM ctx stx@(IntStx i) =
    do let t = IntT

       judgementM "=>Const"
                  ("ctx" |- show stx ++ ":" ++ show t)
                  ("ctx" |- stx `synth` t -| "ctx")

       return (t, IntStx i, ctx)

-- ⇑Const (Double)
synthAbstrM ctx stx@(DoubleStx d) =
    do let t = DoubleT

       judgementM "=>Const"
                  ("ctx" |- show stx ++ ":" ++ show t)
                  ("ctx" |- stx `synth` t -| "ctx")

       return (DoubleT, DoubleStx d, ctx)

-- ⇑Seq
synthAbstrM ctx stx@(SeqStx stxs) =
    do (ts, stxs', ctx') <- synthSeq [] [] ctx stxs

       judgementM "=>Seq"
                  "ctx1 |- ... -| ctxn+1"
                  ("ctx1" |- stx `synth` (TupT ts) -| "ctxn+1")

       return (kickForalls (TupT ts), SeqStx stxs', ctx')
    where synthSeq ts stxs ctx [] =
            return (reverse ts, reverse stxs, ctx)

          synthSeq ts stxs ctx (stx:stxs') =
            do (t, stx', ctx') <- synthM ctx stx
               synthSeq (t:ts) (stx':stxs) ctx' stxs'

-- ⇑Var
synthAbstrM ctx stx@(IdStx name) =
    do (ctx', t) <- typeContext ctx name

       judgementM "=>Var"
                  ("ctx(" ++ showAbbrev stx ++ ") = " ++ show t)
                  ("ctx" |- stx `synth` t -| "ctx")

       return (t, IdStx (name, t, t), ctx')

-- ⇑Lambda (annotated)
synthAbstrM ctx stx@(LambdaStx arg (Just ann) body) =
    do (ctx', argT) <- typeContext ctx ann
       (rangeT, body', ctx'') <- synthAbstrM (insertContext ctx' arg (simpleType argT)) body
       
       judgementM "=>Lambda" "" ""

       return (ArrowT argT rangeT, LambdaStx arg (Just ann) body', ctx'')

-- synthAbstrM syms Synth stx@(LambdaStx arg body) =
--     error "synthAbstrM: LambdaStx (annotated): Synth: not implemented"

-- ⇑Lambda (unannotated) no rule in Pierce's paper, use Siek instead
-- synthAbstrM ctx (LambdaStx arg body) =
--     do let arg' = '^':arg
--            argT = ExistT arg'
--            ctx' = insertContext ctx arg' (simpleType argT)
--            ctx'' = insertContext ctx' arg (simpleType argT)
--        (bodyT, body', ctx''') <- synthM ctx'' body
--        return (ArrowT argT bodyT, LambdaStx arg body', ctx''')

-- ⇑LetChk
synthAbstrM ctx stx@(AppStx fn@(LambdaStx x Nothing body) arg@(LambdaStx _ _ _)) =
    do let evar = ['^', toEnum (count ctx)]
           evarT = ExistT evar
           ctx' = insertContext ctx { count = count ctx + 1 } evar (simpleType evarT)

       (arg', ctx'') <- checkM evarT ctx' arg
       (rangeT, body', ctx''') <- synthM (insertContext ctx'' x (simpleType evarT)) body

       judgementM "=>LetChk"
                  ("ctx1," ++ evar |- arg <= evarT -| "ctx2  ctx2," ++ x ++ ":^a" |- body `synth` rangeT -| "ctx3")
                  ("ctx1" |- stx `synth` rangeT -| "ctx3")

       return (rangeT, AppStx (LambdaStx x Nothing body') arg', ctx''')

-- ⇑LetSyn
synthAbstrM ctx stx@(AppStx fn@(LambdaStx x Nothing body) arg) =
    do (argT, arg', ctx') <- synthM ctx arg
       (rangeT, body', ctx'') <- synthM (insertContext ctx' x (simpleType argT)) body

       judgementM "=>LetSyn"
                  ("ctx1" |- arg `synth` argT -| "ctx2  ctx2," ++ x ++ ":" ++ show argT |- body `synth` rangeT -| "ctx3")
                  ("ctx1" |- stx `synth` rangeT -| "ctx3")

       return (rangeT, AppStx (LambdaStx x Nothing body') arg', ctx'')


-- ⇑AppArrow
-- ⇑AppEvar
synthAbstrM ctx stx@(AppStx fn arg) =
    do (t, fn', ctx') <- synthForallM ctx fn
       synthApp t fn' ctx'
    where -- ⇑Forall
          synthForallM :: Context -> Stx String -> SynthM
          synthForallM ctx stx =
            do (t', stx', ctx') <- synthM ctx stx
               let (ctx'', t'') = eliminateForalls ctx' t'

               when (isForallT t') $
                 judgementM "=>Forall"
                            ("ctx1" |- stx `synth` t' -| "ctx2")
                            ("ctx1" |- stx `synth` t'' -| "ctx2,...")

               return (t'', stx', ctx'')

          synthApp t@(ArrowT argT rangeT) fn' ctx =
            do (arg', ctx') <- checkM argT ctx arg

               judgementM "=>AppArrow"
                          ("ctx1" |- fn `synth` t -| "ctx2  ctx2" |- arg <= argT -| "ctx3")
                          ("ctx1" |- stx `synth` rangeT -| "ctx3")

               return (rangeT, AppStx fn' arg', ctx')

          synthApp t@DynT fn' ctx =
            do let argT = DynT
                   rangeT = DynT
               (arg', ctx') <- checkM argT ctx arg

               judgementM "=>AppDyn"
                          ("ctx1" |- fn `synth` t -| "ctx2  ctx2" |- arg <= argT -| "ctx3")
                          ("ctx1" |- stx `synth` rangeT -| "ctx3")

               return (rangeT, AppStx fn' arg', ctx')

          synthApp t@(ExistT var) fn' ctx =
            do let (t'@(ArrowT argT rangeT), ctx') = arrowifyVar ctx var

               judgementM "=>AppEvar"
                          ("ctx1" |- fn `synth` t -| gamma' "ctx2" [var] ++ gamma'' "ctx2" [(argT, Nothing), (rangeT, Nothing), (t, Just t')] |- arg <= argT -| "ctx3")
                          ("ctx1" |- stx `synth` rangeT -| "ctx3")

               (arg', ctx'') <- checkM argT ctx' arg
               return (rangeT, AppStx fn' arg', ctx'')

-- ⇑Others
synthAbstrM ctx (CondStx ms blame) =
    do let (evarT, ctx') = genEvar ctx
       (ctx'', ts, ms') <- synthMs evarT ctx' [] ms
       return (evarT, CondStx ms' blame, ctx'')
    where synthMs _ ctx stxs [] = return (ctx, ts, stxs)
          synthMs evarT ctx stxs ((stx1, stx2):ms) =
              do (stx1', ctx') <- checkM BoolT ctx stx1
                 (stx2', ctx'') <- checkM evarT ctx' stx2
                 synthMs evarT ctx'' ((stx1', stx2'):stxs) ms

synthAbstrM ctx (DefnStx Def name body) =
    do (bodyT, body', ctx') <- synthM (insertContext ctx name (simpleType DynT)) body
       return (bodyT, DefnStx Def name body', insertContext ctx' name (simpleType bodyT))

synthAbstrM ctx (DefnStx NrDef name body) =
    do (bodyT, body', ctx') <- synthM ctx body
       let bodyT' = substituteExistTs ctx' bodyT
       return (bodyT', DefnStx NrDef name body', insertContext ctx' name (simpleType bodyT'))

synthAbstrM ctx stx@(WhereStx _ _) =
    typecheckWhereM synthM ctx stx

synthAbstrM _ stx =
    Left $ "\n\n\tsynthAbstrM: unhandled case" ++
           "\n\n\t stx = " ++ show stx ++ "\n"


-- edit: check (ExistT var) is necessary for lambda terms (Joshua said)
-- but it conflicts with the following examples
--
--  ap:1:2.0
--
-- because the derivation rules must reach the consistency relation
-- and this substitution makes it impossible.  Perhaps this rule
-- needs only to be applied around lambda terms...
checkM :: Type -> Context -> Stx String -> CheckM
-- checkM t@(ExistT var) ctx stx | not (isEmptyTypeContext ctx var) =
--   do let Right (ctx', t') = typeContext ctx var

--      let !() = judgementM "<=Subst"
--                            ("ctx1 |- " ++ showAbbrev stx ++ " <= " ++ show t' ++ " -| ctx2")
--                            ("ctx1 |- " ++ showAbbrev stx ++ " <= " ++ show t ++ " -| ctx2")

--      checkInstM t' ctx' stx

checkM t ctx stx = checkInstM t ctx stx


checkInstM :: Type -> Context -> Stx String -> CheckM

-- ⇓Forall
checkInstM t@(ForallT var forallT) ctx stx | isValueStx stx =
    do judgementM "<=Forall"
                  ("ctx1," ++ var |- stx <= forallT -| "ctx2'," ++ var ++ ",ctx2''")
                  ("ctx1" |- stx <= t -| "ctx2'")

       let ctx' = insertContext ctx var (simpleType (TvarT var))
       (stx', ctx'') <- checkM forallT ctx' stx
       return (stx', dropContext ctx'' var)

-- ⇓SeqTup
-- checkInstM t@(TupT ts) ctx (SeqStx stxs)
--   | length ts == length stxs =
--     checkSeqM ts ctx stxs
--   | otherwise =
--     Left $ "\n\n\tcheckInstM: SeqStx: different length" ++
--            "\n\n\t ts = " ++ show ts ++
--            "\n\n\t t = " ++ show t ++
--            "\n\n\t stxs = " ++ show stxs ++ "\n"
--     where checkSeqM ts ctx stxs = check [] ts ctx stxs
--               where check stxs _ ctx [] = return (SeqStx (reverse stxs), ctx) 
--                     check stxs (t:ts) ctx (stx:stxs') =
--                       do (stx', ctx') <- checkM t ctx stx
--                          check (stx':stxs) ts ctx' stxs'

checkInstM t@(TupT ts) ctx stx@(SeqStx stxs) =
    do val <- checkSeqM ts ctx stxs

       judgementM "<=SeqTup"
                  ("ctx1" |- "..." -| "ctx2  ...  ctxn" |- "..." -| "ctxn+1")
                  ("ctx1" |- stx <= t -| "ctxn+1")

       return val
    where msg =
            "\n\n\tcheckInstM: SeqStx: different length" ++
            "\n\n\t ts = " ++ show ts ++
            "\n\n\t t = " ++ show t ++
            "\n\n\t stxs = " ++ show stxs ++ "\n"

          checkSeqM ts ctx stxs = check [] ts ctx stxs
              where check stxs [] ctx [] = return (SeqStx (reverse stxs), ctx) 
                    check _ _ _ [] = Left msg
                    check _ [] _ _ = Left msg
                    check stxs (t:ts) ctx (stx:stxs') =
                      do (stx', ctx') <- checkM t ctx stx
                         check (stx':stxs) ts ctx' stxs'

-- ⇓SeqList
checkInstM t@(SeqT seqT) ctx stx@(SeqStx stxs) =
    do let t' = TupT (replicate (length stxs) seqT)

       judgementM "<=SeqList"
                  ("ctx1" |- stx <= t' -| "ctx2")
                  ("ctx1" |- stx <= t -| "ctx2")

       checkInstM t' ctx stx

-- ⇓SeqDyn
checkInstM t@DynT ctx stx@(SeqStx _) =
    do let t' = SeqT DynT

       judgementM "<=SeqDyn"
                  ("ctx1" |- stx <= t' -| "ctx2")
                  ("ctx1" |- stx <= t -| "ctx2")

       checkInstM t' ctx stx

-- C-Abs (annotated)
-- checkInstM syms (Check _) stx@(LambdaStx arg body) =
--     error "checkInstM: LambdaStx (annotated): Check: not implemented"

-- ⇓LambdaArrow
checkInstM t@(ArrowT argT rangeT) ctx stx@(LambdaStx arg Nothing body) =
    do judgementM "<=LambdaArrow"
                  ("ctx1," ++ arg ++ ":" ++ show argT |- body <= rangeT -| "ctx2'," ++ arg ++ ":" ++ show argT ++ "ctx2''")
                  ("ctx1" |- stx <= t -| "ctx2'")

       (body', ctx') <- checkM rangeT (insertContext ctx arg (simpleType argT)) body
       return (LambdaStx arg Nothing body', dropContext ctx' arg)

-- ⇓LambdaDyn
checkInstM t@DynT syms stx@(LambdaStx arg Nothing body) =
    do let t' = ArrowT t t

       judgementM "<=LambdaDyn"
                  ("ctx1" |- stx <= t' -| "ctx2")
                  ("ctx1" |- stx <= t -| "ctx2")

       (body', syms') <- checkM t (insertContext syms arg (simpleType t)) body
       return (LambdaStx arg Nothing body', syms')

-- ⇓LambdaEvar
checkInstM (ExistT var) ctx stx@(LambdaStx _ _ _) | isEmptyTypeContext ctx var =
    let (existT, ctx') = arrowifyVar ctx var in
    checkInstM existT ctx' stx

-- C-Where
checkInstM t syms stx@(WhereStx _ _) =
    typecheckWhereM (checkM t) syms stx

-- <=Sub
checkInstM t syms stx =
    do (t', stx', syms') <- synthAbstrM syms stx

       judgementM "<=Sub"
                  ("ctx1" |- stx `synth` t'  -| "ctx2  ctx2" |- t' <: t |- "ctx3")
                  ("ctx1" |- stx <= t -| "ctx3")

       (stx',) <$> subM syms' t' t


typecheckSubstitute :: Context -> Stx String -> TypecheckerM (Type, Context)
typecheckSubstitute ctx stx =
    do (t, _, ctx') <- synthM ctx stx
       let !_ | debugT ("type before the final substitution: " ++ show t) = True
       return (substituteExistTs ctx' t, ctx')


typecheckStxs :: Context -> [Stx String] -> TypecheckerM (Type, Context)
typecheckStxs ctx stxs = typecheck ctx stxs
    where typecheck ctx [stx] = typecheckSubstitute ctx stx
          typecheck ctx (stx:stxs) =
            do (_, ctx') <- typecheckSubstitute ctx stx
               typecheck ctx' stxs


typecheckNamespace :: Map String SrcFile -> [String] -> Namespace String -> Either String (Type, Map String Type)
typecheckNamespace fs deps (Namespace _ stxs) =
    do let tss = map (SrcFile.ts . (fs Map.!)) deps
           ts = nothingSyms $ foldr1 Map.union tss
       (t, ctx) <- typecheckStxs ts stxs
       
       let !_ | trace ((("    " ++) . show) $ filter (\x -> "^" `isPrefixOf` (fst x)) $ reverse $ Context.syms ctx) True = True

       return (t, Map.fromList (simpleSyms ctx))
       

typecheckSrcFile :: Map String SrcFile -> SrcFile -> Either String (Map String Type, Type)
typecheckSrcFile fs srcfile@SrcFile { deps, renNs = Just ns }
    | doTypecheck =
        do (t, ts) <- typecheckNamespace fs deps ns
           let names = Map.keys (SrcFile.symbols srcfile)
               ts' = Map.fromList $ catMaybes [ case Map.lookup name ts of
                                                  Nothing -> Nothing
                                                  Just t -> Just (name, t) | name <- names ]
           return (ts', t)
    | otherwise =
        do let ts = Map.map (const DynT) (SrcFile.symbols srcfile)
           return (ts, DynT)


typecheck :: Map String SrcFile -> SrcFile -> Either String SrcFile
typecheck _ srcfile@SrcFile { t = CoreT } =
    return srcfile

typecheck _ srcfile@SrcFile { t = SrcT, renNs = Just (Namespace _ []) } =
    return srcfile

typecheck fs srcfile@SrcFile { t = SrcT } =
    do (ts, t) <- typecheckSrcFile fs srcfile
       return srcfile { ts = ts }

typecheck fs srcfile@SrcFile { t = InteractiveT } =
    Left "Typechecker.typecheck: for interactive srcfiles use 'typecheckInteractive' instead of 'typecheck'"


typecheckInteractive :: Map String SrcFile -> SrcFile -> Either String (SrcFile, Type)
typecheckInteractive fs srcfile =
     do (ts, t) <- typecheckSrcFile interactiveFs interactiveSrcfile
        return (srcfile { ts = Map.union ts (SrcFile.ts srcfile) }, t)
    where interactiveDeps =
              SrcFile.deps srcfile ++ ["Interactive"]
            
          interactiveSrcfile =
              srcfile { deps = interactiveDeps }

          interactiveFs =
              Map.insert "Interactive" srcfile fs