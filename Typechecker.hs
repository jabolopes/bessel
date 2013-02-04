{-# LANGUAGE NamedFieldPuns, TupleSections #-}
module Typechecker where

import Data.Functor ((<$>))
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Context
import qualified Data.Context as Context
import Data.SrcFile
import qualified Data.SrcFile as SrcFile
import Data.Stx
import Data.Type


import Debug.Trace


debug = False
debugF desc = debug && trace desc False
debugT desc = (debug && trace desc True) || True


debugConsistentT = True
debugConsistentTF desc = debugConsistentT && trace desc False
debugConsistentTT desc = (debugConsistentT && trace desc True) || True


consistentT :: Context -> Type -> Type -> Maybe Context
consistentT ctx t1 t2 | debugConsistentTF ("consistentT:\n  " ++ intercalate "\n  " (map show (syms ctx)) ++ "\n  " ++ show t1 ++ " ~~ " ++ show t2 ++ "\n") = undefined

consistentT syms t1 t2
    | isAtomicT t1 && t1 == t2 = return syms

consistentT syms (TupT ts1) (TupT ts2)
    | length ts1 == length ts2 = consistentT' syms ts1 ts2
    where consistentT' syms [] [] = return syms
	  consistentT' syms (t1:ts1) (t2:ts2) =
    	      do syms' <- consistentT syms t1 t2
	      	 consistentT' syms' ts1 ts2

consistentT syms t1@(TupT ts1) (SeqT t) =
    consistentT syms t1 (TupT (replicate (length ts1) t))

consistentT syms (SeqT t) t2@(TupT ts2) =
    consistentT syms (TupT (replicate (length ts2) t)) t2

consistentT syms (SeqT t1) (SeqT t2) =
    consistentT syms t1 t2

-- →
consistentT syms (ArrowT argT1 rangeT1) (ArrowT argT2 rangeT2) =
    do syms' <- consistentT syms argT2 argT1
       consistentT syms' rangeT1 rangeT2

-- -- αRefl
-- -- info: this rule is already included in the atomic eq
-- consistentT syms (TvarT var1) (TvarT var2)
--     | var1 == var2 = return syms

-- α̂Refl
consistentT syms (ExistT var1) (ExistT var2)
    | var1 == var2 = return syms

-- α̂⁼L
consistentT syms t1 t2@(ExistT var)
    | isEmptyTypeContext syms var && isAtomicT t1 =
      return $ updateContext syms var (unifType t2 t1)

-- α̂⁼R
consistentT syms t1@(ExistT var) t2
    | isEmptyTypeContext syms var && isAtomicT t2 = 
      return $ updateContext syms var (unifType t1 t2)

-- ∀Lα̂
consistentT syms t1@(ForallT _ _) t2 =
    let (syms', t1') = synthOuterForall syms t1 in
    consistentT syms' t1' t2

-- ∀R
consistentT syms t1 (ForallT var forallT) =
    do let syms' = insertContext syms var (simpleType (TvarT var))
       syms'' <- consistentT syms' t1 forallT
       return $ dropContext syms'' var

-- →α̂L
consistentT syms (ExistT var) t2@(ArrowT _ _)
  | isEmptyTypeContext syms var =
    let (t1, syms') = arrowifyTvar syms var in
    consistentT syms' t1 t2

-- →α̂R
consistentT syms t1@(ArrowT _ _) (ExistT var)
  | isEmptyTypeContext syms var =
    let (t2, syms') = arrowifyTvar syms var in
    consistentT syms' t1 t2

-- α̂SubstL
consistentT syms t1@(ExistT var) t2
  | not (isEmptyTypeContext syms var) =
    case typeContext syms var of
      Right t1' | t1 /= t1' -> consistentT syms t1' t2

-- α̂SubstR
consistentT syms t1 t2@(ExistT var)
  | not (isEmptyTypeContext syms var) =
    case typeContext syms var of
      Right t2' | t2 /= t2' -> consistentT syms t1 t2'

consistentT syms DynT _ = return syms
consistentT syms _ DynT = return syms

consistentT _ _ _ = Nothing


(~~) = consistentT


consistentM :: Context -> Type -> Type -> Either String Context
consistentM syms t1 t2 =
  case consistentT syms t1 t2 of
    Nothing -> Left $ "\n\n\ttype inconsistency: " ++ show t1 ++ " ~~ " ++ show t2 ++ " (false)\n"
    Just syms' -> Right syms'


substituteExistTs :: Context -> Type -> Type
substituteExistTs _ t@BoolT = t
substituteExistTs _ t@IntT  = t
substituteExistTs _ t@DoubleT = t
substituteExistTs _ t@CharT = t
substituteExistTs syms (TupT ts) = TupT $ map (substituteExistTs syms) ts
substituteExistTs syms (SeqT t) = SeqT $ substituteExistTs syms t
substituteExistTs syms t@DynT = t

substituteExistTs syms (ArrowT t1 t2) =
  ArrowT (substituteExistTs syms t1) (substituteExistTs syms t2)

substituteExistTs syms t@(ExistT var) =
  case lookupContext syms var of
    Nothing -> error $ "Typechecker.substituteExistTs: " ++ show var
    Just (ExistT var', Nothing) | var == var' -> t
    Just (t', Nothing) -> substituteExistTs syms t'
    Just (_, Just t') -> substituteExistTs syms t'

substituteExistTs syms (ForallT vars t) = ForallT vars $ substituteExistTs syms t
substituteExistTs syms t@(TvarT _) = t


substituteTvarT :: Type -> String -> Type -> Type
substituteTvarT _ _ BoolT = BoolT
substituteTvarT _ _ IntT = IntT
substituteTvarT _ _ DoubleT = DoubleT
substituteTvarT _ _ CharT = CharT
substituteTvarT t var (TupT ts) = TupT $ map (substituteTvarT t var) ts
substituteTvarT t var (SeqT seqT) = SeqT $ substituteTvarT t var seqT
substituteTvarT t var DynT = DynT

substituteTvarT t var (ArrowT fnT argT) =
  ArrowT (substituteTvarT t var fnT) (substituteTvarT t var argT)

substituteTvarT _ _ t@(ExistT _) = t

substituteTvarT t var (ForallT vars forallT) =
  ForallT vars $ substituteTvarT t var forallT

substituteTvarT t var1 tvarT@(TvarT var2)
  | var1 == var2 = t
  | otherwise = tvarT


arrowifyTvar :: Context -> String -> (Type, Context)
arrowifyTvar ctx@Context { count } var =
  let
    (Context { syms = syms1 }, Context { syms = syms2 }) = splitContext ctx var
    name1 = var ++ show (count + 1)
    name2 = var ++ show (count + 2)
    a1 = (name1, (ExistT name1, Nothing))
    a2 = (name2, (ExistT name2, Nothing))
    at = ArrowT (ExistT name1) (ExistT name2)
    a  = (var, (ExistT var, Just at))
  in
   (at, ctx { syms = syms1 ++ [a, a1, a2] ++ syms2, count = count + 3 })


type TypecheckerM a = Either String a
type SynthM = TypecheckerM (Type, Stx (String, Type, Type), Context)
type CheckM = TypecheckerM (Stx (String, Type, Type), Context)


synthOuterForall :: Context -> Type -> (Context, Type)
synthOuterForall syms (ForallT var forallT) =
  let
    syms' = insertContext syms var (simpleType (ExistT var))
    forallT' = substituteTvarT (ExistT var) var forallT
  in
   synthOuterForall syms' forallT'

synthOuterForall syms t = (syms, t)


synthOuterForallM :: SynthM -> SynthM
synthOuterForallM m =
  do (t, stx, syms) <- m
     let (syms', t') = synthOuterForall syms t
     return (t', stx, syms')


synthAppFnM :: Context -> Stx String -> SynthM
synthAppFnM _ _ | debugF "synthAppFnM" = undefined
synthAppFnM syms fn =
    do (t, stx, syms') <- synthOuterForallM $ synthM syms fn
       t' <- do case t of
                  ArrowT _ _ -> return t
                  DynT -> return $ ArrowT DynT DynT
                  t -> Left $ "\n\n\tsynthAppFnM: expected arrow type" ++
                              "\n\n\t t = " ++ show t ++ "\n"
       return (t', stx, syms')


checkSeqM :: [Type] -> Context -> [Stx String] -> CheckM
checkSeqM _ _ _ | debugF "checkSeqM" = undefined
checkSeqM ts syms stxs = check [] ts syms stxs
  where check stxs ts syms [] = return (SeqStx (reverse stxs), syms) 
        check stxs (t:ts) syms (stx:stxs') =
          do (stx', syms') <- checkM t syms stx
             check (stx':stxs) ts syms' stxs'


typecheckWhereM :: (Context -> Stx String -> TypecheckerM a) -> Context -> Stx String -> TypecheckerM a
typecheckWhereM _ _ _ | debugF "typecheckWhereM" = undefined
typecheckWhereM m syms (WhereStx stx (stxs)) =
    check syms stxs
    where check syms [] = m syms stx
          check syms (stx:stxs) =
              do (_, _, syms') <- synthM syms stx
                 check syms' stxs


synthM :: Context -> Stx String -> SynthM
synthM syms stx =
  do val <- synthAbstrM syms stx
     case val of
       (t@(ExistT _), stx', syms') -> return (substituteExistTs syms' t, stx', syms')
       _ -> return val


synthAbstrM :: Context -> Stx String -> SynthM
synthAbstrM _ stx | debugF ("synthAbstrM: " ++ show stx) = undefined

-- S-Char
synthAbstrM syms (CharStx i) = Right (CharT, CharStx i, syms)

-- S-Int
synthAbstrM syms (IntStx i) = Right (IntT, IntStx i, syms)

-- S-Double
synthAbstrM syms (DoubleStx i) = Right (DoubleT, DoubleStx i, syms)

-- S-Seq
synthAbstrM syms (SeqStx stxs) =
    do (ts, stxs', syms') <- synth [] [] syms stxs
       Right (TupT ts, SeqStx stxs', syms')
    where synth ts stxs syms [] = return (reverse ts, reverse stxs, syms)
          synth ts stxs syms (stx:stxs') =
            do (t, stx', syms') <- synthM syms stx
               synth (t:ts) (stx':stxs) syms' stxs'

-- S-Var
synthAbstrM syms (IdStx name) =
    do t <- typeContext syms name
       return (t, IdStx (name, t, t), syms)

-- S-Abs (annotated)
-- synthAbstrM syms Synth stx@(LambdaStx arg body) =
--     error "synthAbstrM: LambdaStx (annotated): Synth: not implemented"

-- S-Abs (unannotated) no rule in Pierce's paper, use Siek instead
-- edit: to fix
-- synthAbstrM syms (LambdaStx arg body) =
--     do (bodyT, body', syms') <- synthM (insertContext syms arg (simpleType DynT)) body
--        Right (ArrowT (argT body') bodyT, LambdaStx arg body', syms')
--     where argT body =
--               case find (\(IdStx (name, _, _)) -> name == arg) $ idsOf body of
--                 Nothing -> DynT
--                 Just (IdStx (_, _, argT)) -> argT

-- S-App with lambda
-- edit: to fix
-- synthAbstrM syms (AppStx fn@(LambdaStx x body) arg) =
--     do (argT, arg', syms') <- synthM syms arg
--        (rangeT, body', syms'') <- synthM (insertContext syms' x (simpleType argT)) body
--        (fn', syms''') <- checkM (ArrowT argT rangeT) syms'' fn
--        Right (rangeT, AppStx fn' arg', syms''')

-- S-App
synthAbstrM syms (AppStx fn arg) =
    do (t, fn', syms') <- synthAppFnM syms fn
       synthApp t fn' syms'
    where synthApp t _ _ | debugF ("synthApp: " ++ show t) = undefined
          synthApp (ArrowT argT rangeT) fn' syms' =
            do (arg', syms'') <- checkM argT syms' arg
               return (rangeT, AppStx fn' arg', syms'')

          synthApp (ExistT var) fn syms =
            do let (ArrowT a1t a2t, syms') = arrowifyTvar syms var
               (arg', syms'') <- checkM a1t syms' arg
               return (a2t, AppStx fn arg', syms'')

-- S-Others
synthAbstrM syms stx@(WhereStx _ _) =
    typecheckWhereM synthM syms stx

synthAbstrM syms (DefnStx Def name body) =
    do (bodyT, body', syms') <- synthM (insertContext syms name (simpleType DynT)) body
       Right (bodyT, DefnStx Def name body', insertContext syms' name (simpleType bodyT))

synthAbstrM syms (DefnStx NrDef name body) =
    do (bodyT, body', syms') <- synthM syms body
       Right (bodyT, DefnStx NrDef name body', insertContext syms' name (simpleType bodyT))

synthAbstrM _ stx =
    Left $ "\n\n\tsynthAbstrM: unhandled case" ++
           "\n\n\t stx = " ++ show stx ++ "\n"


checkM :: Type -> Context -> Stx String -> CheckM
checkM (ExistT var) syms stx =
  do t <- typeContext syms var
     checkInstM t syms stx
checkM t syms stx = checkInstM t syms stx


checkInstM :: Type -> Context -> Stx String -> CheckM
checkInstM t _ _| debugF ("checkInstM: " ++ show t) = undefined

-- C-Forall-I
checkInstM (ForallT var t) syms stx | isValueStx stx =
    do let syms' = insertContext syms var (simpleType (TvarT var))
       (stx', syms'') <- checkM t syms' stx
       return (stx', dropContext syms'' var)

-- C-Int
checkInstM t syms (IntStx i) = (IntStx i,) <$> consistentM syms IntT t

-- C-Double
checkInstM t syms (DoubleStx d) = (DoubleStx d,) <$> consistentM syms DoubleT t

-- C-Char
checkInstM t syms (CharStx c) = (CharStx c,) <$> consistentM syms CharT t

-- C-Seq
checkInstM t@(TupT ts) syms (SeqStx stxs) =
    if length ts == length stxs
    then checkSeqM ts syms stxs
    else Left $ "\n\n\tcheckInstM: SeqStx: different length" ++
                "\n\n\t ts = " ++ show ts ++
                "\n\n\t t = " ++ show t ++
                "\n\n\t stxs = " ++ show stxs ++ "\n"

checkInstM (SeqT t) syms (SeqStx stxs) =
    checkSeqM (replicate (length stxs) t) syms stxs

checkInstM t@DynT syms (SeqStx stxs) =
    checkSeqM (replicate (length stxs) t) syms stxs

-- -- C-Var
-- info: this is a particular instance of the subsumption rule?
-- checkInstM t syms (IdStx name) =
--     do idT <- typeContext syms name
--        (IdStx (name, idT, t),) <$> consistentM syms idT t

-- C-Abs (annotated)
-- checkInstM syms (Check _) stx@(LambdaStx arg body) =
--     error "checkInstM: LambdaStx (annotated): Check: not implemented"

-- C-Abs (unannotated)
checkInstM (ArrowT argT rangeT) syms (LambdaStx arg body) =
    do (body', syms') <- checkM rangeT (insertContext syms arg (simpleType argT)) body
       Right (LambdaStx arg body', dropContext syms' arg)

checkInstM t@DynT syms (LambdaStx arg body) =
    do (body', syms') <- checkM t (insertContext syms arg (simpleType DynT)) body
       Right (LambdaStx arg body', syms')

checkInstM (ExistT var) syms stx@(LambdaStx _ _) =
    let (existT, syms') = arrowifyTvar syms var in
    checkInstM existT syms' stx

-- C-App
checkInstM t syms (AppStx fn arg) =
    do (ArrowT argT rangeT, fn', syms') <- synthAppFnM syms fn
       (arg', syms'') <- checkM argT syms' arg
       (AppStx fn' arg',) <$> consistentM syms'' rangeT t

-- C-Where
checkInstM t syms stx@(WhereStx _ _) =
    typecheckWhereM (checkM t) syms stx

-- C-Sub
checkInstM t syms stx =
    do (t', stx', syms') <- synthAbstrM syms stx
       (stx',) <$> consistentM syms' t' t


typecheckStxs :: Context -> [Stx String] -> TypecheckerM Context
typecheckStxs ctx stxs = typecheck ctx stxs
    where typecheck ctx [] = return ctx
          typecheck ctx (stx:stxs) =
            do (_, _, ctx') <- synthM ctx stx
               typecheck ctx' stxs


typecheckNamespace :: Map String SrcFile -> [String] -> Namespace String -> Either String (Map String Type)
typecheckNamespace fs deps (Namespace _ stxs) =
    let
        tss = map (SrcFile.ts . (fs Map.!)) deps
        ctx = nothingSyms $ foldr1 Map.union tss
    in
      simpleSyms <$> typecheckStxs ctx stxs


typecheck :: Map String SrcFile -> SrcFile -> Either String SrcFile
typecheck _ srcfile@SrcFile { deps, srcNs = Right (_, binds) } =
    return $ srcfile { ts = Map.map fst binds }

typecheck fs srcfile@SrcFile { deps, renNs = Just ns } =
    do ts <- typecheckNamespace fs deps ns
       return $ srcfile { ts = ts }


typecheckIncremental :: Map String Type -> Stx String -> TypecheckerM (Type, Map String Type)
typecheckIncremental syms stx =
    do (t, _, syms') <- synthM (nothingSyms syms) stx
       return (substituteExistTs syms' t, simpleSyms syms')


typecheckInteractive :: Map String Type -> Stx String -> Either String (Type, Map String Type)
typecheckInteractive ts stx =
    do (t, ts') <- typecheckIncremental ts stx
       return (t, ts')