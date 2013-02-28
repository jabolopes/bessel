{-# LANGUAGE BangPatterns, NamedFieldPuns,
             TupleSections #-}
module Typechecker where

import Control.Monad (liftM)
import Data.Functor ((<$>))
import Data.List (intercalate, isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)

import Data.Context
import qualified Data.Context as Context
import Data.SrcFile
import qualified Data.SrcFile as SrcFile
import Data.Exception
import Data.Type
import Data.Stx
import Data.Type

import System.IO.Unsafe
import Debug.Trace


debug = True
debugF desc = debug && trace desc False
debugT desc = (debug && trace desc True) || True


debugConsistentT = True
debugConsistentTF desc = debugConsistentT && trace desc False
debugConsistentTT desc = (debugConsistentT && trace desc True) || True


logT desc t1 t2 =
    debugConsistentTT $ desc ++ ": " ++ show t1 ++ " <: " ++ show t2


judgementM :: String -> String -> String -> ()
judgementM name str1 str2 =
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


-- types and contexts

eliminateForall :: Context -> Type -> (Context, Type)
eliminateForall ctx (ForallT var forallT) =
  let
    var' = '^':var
    existT = ExistT var'
    ctx' = insertContext ctx var' (simpleType existT)
    forallT' = substituteTvarT existT var forallT
  in
   eliminateForall ctx' forallT'

eliminateForall ctx t = (ctx, t)


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

consistentT ctx t1 t2@(ExistT var)
  | not (isEmptyTypeContext ctx var) && logT "(~~R)" t1 t2 =
    let Right (ctx', t2') = typeContext ctx var in
    case subT ctx' t1 t2' of
      Nothing -> Just $ updateContext ctx' var (unifType t2 DynT)
      x -> x

consistentT ctx t1@(ExistT var) t2
  | not (isEmptyTypeContext ctx var) && logT "(~~L)" t1 t2 =
    let Right (ctx', t1') = typeContext ctx var in
    case subT ctx' t1' t2 of
      Nothing -> Just $ updateContext ctx' var (unifType t1 DynT)
      x -> x


subT :: Context -> Type -> Type -> Maybe Context
subT ctx t1 t2
  | occursContextT ctx t1 t2 =
    throwTypecheckerException $ "occurs " ++ show t1 ++  " <: " ++ show t2

subT ctx t1 t2
    | isAtomicT t1 && t1 == t2 =
      do let !() = judgementM "<Refl"
                              ""
                              ("ctx |- " ++ show t1 ++ " <: " ++ show t2 ++ " -| ctx")
         return ctx

subT ctx t1@(TupT ts1) t2@(TupT ts2)
    | length ts1 == length ts2 && logT "(tup ~ tup)" t1 t2 =
        subT' ctx ts1 ts2
    where subT' ctx [] [] = return ctx
	  subT' ctx (t1:ts1) (t2:ts2) =
    	      do ctx' <- subT ctx t1 t2
	      	 subT' ctx' ts1 ts2

subT ctx t1@(TupT ts1) t2@(SeqT t) =
    do let t2' = TupT (replicate (length ts1) t)

       let !() = judgementM "<TupSeq"
                            ("ctx1 |- " ++ show t1 ++ " <: " ++ show t2' ++ " -| ctx2")
                            ("ctx1 |- " ++ show t1 ++ " <: " ++ show t2 ++ " -| ctx2 ")

       subT ctx t1 t2'

subT ctx t1@(SeqT seqT1) t2@(SeqT seqT2) =
     do let !() = judgementM "<SeqSeq"
                             ("ctx1 |- " ++ show seqT1 ++ " <: " ++ show seqT2 ++ " -| ctx2")
                             ("ctx1 |- " ++ show t1 ++ " <: " ++ show t2 ++ " -| ctx2")

        subT ctx seqT1 seqT2

-- →E
subT ctx t1@(ArrowT argT1 rangeT1) t2@(ArrowT argT2 rangeT2) =
  do let !() = judgementM "<Arrow"
                          ("ctx1 |- " ++ show argT2 ++ " <: " ++ show argT1 ++ " -| ctx2 |- " ++ show rangeT1 ++ " <: " ++ show rangeT2 ++ " -| ctx3")
                          ("ctx1 |- " ++ show t1 ++ " <: " ++ show t2 ++ " -| ctx3")

     ctx' <- subT ctx argT2 argT1
     subT ctx' rangeT1 rangeT2

-- αRefl
-- info: this rule is already included in the atomic eq
-- subT syms (TvarT var1) (TvarT var2)
--     | var1 == var2 = return syms

-- α̂Refl
-- info: this rule is already included in the atomic eq
-- subT ctx t1@(ExistT var1) t2@(ExistT var2)
--     | var1 == var2 = return ctx
--     -- edit: not in the paper
--     | otherwise = return $ updateContext ctx var2 (unifType t2 t1)

-- →α̂L
subT ctx t1@(ExistT var) t2@(ArrowT _ _)
  | isEmptyTypeContext ctx var =
      do let !() = judgementM "<ArrowifyL"
                              ("ctx1," ++ var ++ "1," ++ var ++ "2," ++ var ++ "=" ++ var ++ "1->" ++ var ++ "2 |- " ++ show t1 ++ " <: " ++ show t2 ++ " -| ctx2")
                              ("ctx1[" ++ var ++ "] |- " ++ show var ++ " <: " ++ show t2 ++ " -| ctx2")

         let (_, ctx') = arrowifyVar ctx var
         subT ctx' t1 t2

-- →α̂R
subT ctx t1@(ArrowT _ _) t2@(ExistT var)
  | isEmptyTypeContext ctx var =
      do let !() = judgementM "<ArrowifyR"
                              ("ctx1," ++ var ++ "1," ++ var ++ "2," ++ var ++ "=" ++ var ++ "1->" ++ var ++ "2 |- " ++ show t1 ++ " <: " ++ show t2 ++ " -| ctx2")
                              ("ctx1[" ++ var ++ "] |- " ++ show t1 ++ " <: " ++ var ++ " -| ctx2")

         let (_, ctx') = arrowifyVar ctx var
         subT ctx' t1 t2

-- ∀Lα̂
subT ctx t1@(ForallT var _) t2 =
      do let (ctx', t1') = eliminateForall ctx t1
                
         let !() = judgementM "<ForallL"
                              ("ctx1,<|," ++ var ++ " |- " ++ show t1' ++ " <: " ++ show t2 ++ " -| ctx2',<|,ctx2''")
                              ("ctx1 |- " ++ show t1 ++ " <: " ++ show t2 ++ " -| ctx2'")

         subT ctx' t1' t2

-- ∀R
subT ctx t1 t2@(ForallT var forallT) =
    do let !() = judgementM "<ForallR"
                            ("ctx1," ++ var ++ " |- " ++ show t1 ++ " <: " ++ show forallT ++ " -| ctx2'," ++ var ++ "ctx2''")
                            ("ctx1 |- " ++ show t1 ++ " <: " ++ show t2 ++ " -| ctx2''")

       let ctx' = insertContext ctx var (simpleType (TvarT var))
       ctx'' <- subT ctx' t1 forallT
       return $ dropContext ctx'' var

-- α̂SubstR
subT ctx t1 t2@(ExistT var)
  | not (isEmptyTypeContext ctx var) =
      do let Right (_, varT) = typeContext ctx var

         let !() = judgementM "<ConsistentR"
                              ("ctx1[" ++ var ++ "=" ++ show varT ++ "] |- " ++ show t1 ++ " ~ " ++ show t2 ++ " -| ctx2")
                              ("ctx1[" ++ var ++ "=" ++ show varT ++ "] |- " ++ show t1 ++ " <: " ++ show t2 ++ " -| ctx2")

         consistentT ctx t1 t2
  -- | not (isEmptyTypeContext ctx var) && logT "(aSubstR<=)" t1 t2 =
  --   case typeContext ctx var of
  --     Right (ctx', t2') | t2 /= t2' -> subT ctx' t1 t2'

-- α̂SubstL
subT ctx t1@(ExistT var) t2
  | not (isEmptyTypeContext ctx var) =
      do let Right (_, varT) = typeContext ctx var

         let !() = judgementM "<ConsistentR"
                              ("ctx1[" ++ var ++ "=" ++ show varT ++ "] |- " ++ show t1 ++ " ~ " ++ show t2 ++ " -| ctx2")
                              ("ctx1[" ++ var ++ "=" ++ show varT ++ "] |- " ++ show t1 ++ " <: " ++ show t2 ++ " -| ctx2")

         consistentT ctx t1 t2
    -- | not (isEmptyTypeContext ctx var) && logT "(aSubstL<=)" t1 t2 =
    -- case typeContext ctx var of
    --   Right (ctx', t1') | t1 /= t1' -> subT ctx' t1' t2

-- α̂⁼R
subT ctx t1 t2@(ExistT var)
    | isEmptyTypeContext ctx var && isAtomicT t1 && isWellformed ctx t2 t1 =

      do let !() = judgementM "<InstR"
                              ("ctx1 |- " ++ show t1 ++ " wf")
                              ("ctx1," ++ var ++ ",ctx2 |- " ++ show t1 ++ " <: " ++ var ++ " -| ctx1," ++ var ++ "=" ++ show t1 ++ ",ctx2")

         return $ updateContext ctx var (unifType t2 t1)

-- α̂⁼L
subT ctx t1@(ExistT var) t2
    | isEmptyTypeContext ctx var && isAtomicT t2 && isWellformed ctx t1 t2 =

      do let !() = judgementM "<InstL"
                              ("ctx1 |- " ++ show t2 ++ " wf")
                              ("ctx1," ++ var ++ ",ctx2 |- " ++ show t2 ++ " <: " ++ var ++ " -| ctx1," ++ var ++ "=" ++ show t2 ++ ",ctx2")

         return $ updateContext ctx var (unifType t1 t2)

subT ctx t1 t2@DynT =
    do let !() = judgementM "<Dyn"
                            ""
                            ("ctx |- " ++ show t1 ++ " <: " ++ show t2 ++ "-| ctx")

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


synthOuterForallM :: SynthM -> SynthM
synthOuterForallM m =
  do (t, stx, ctx) <- m
     let (ctx', t') = eliminateForall ctx t
     return (t', stx, ctx')

     -- let !() = judgementM "forallEa"
     --                      ("ctx |- " ++ show stx ++ " => " ++ show t ++ " -| ctx'")
     --                      ("ctx |- " ++ show stx ++ " => " ++ show t' ++ " -| ctx', ...")


synthAppFnM :: Context -> Stx String -> SynthM
synthAppFnM syms fn =
    do (t, stx, syms') <- synthOuterForallM $ synthM syms fn
       t' <- case t of
               ArrowT _ _ -> return t
               DynT -> return $ ArrowT DynT DynT
               t -> Left $ "\n\n\tsynthAppFnM: expected arrow type" ++
                           "\n\n\t t = " ++ show t ++ "\n"
       return (t', stx, syms')


checkSeqM :: [Type] -> Context -> [Stx String] -> CheckM
-- checkSeqM ts _ _ | debugF ("checkSeqM: " ++ show ts) = undefined
checkSeqM ts ctx stxs = check [] ts ctx stxs
  where check stxs ts ctx [] = return (SeqStx (reverse stxs), ctx) 
        check stxs (t:ts) ctx (stx:stxs') =
          do (stx', ctx') <- checkM t ctx stx
             check (stx':stxs) ts ctx' stxs'


typecheckWhereM :: (Context -> Stx String -> TypecheckerM a) -> Context -> Stx String -> TypecheckerM a
typecheckWhereM _ _ _ | debugF "typecheckWhereM" = undefined
typecheckWhereM m syms (WhereStx stx (stxs)) =
    check syms stxs
    where check syms [] = m syms stx
          check syms (stx:stxs) =
              do (_, _, syms') <- synthM syms stx
                 check syms' stxs


synthM :: Context -> Stx String -> SynthM
synthM ctx stx =
  substitute <$> synthAbstrM ctx stx
  where substitute (t@(ExistT _), stx', ctx') =
          (substituteExistTs ctx' t, stx', ctx')

        substitute val = val


synthAbstrM :: Context -> Stx String -> SynthM

-- S-Char
synthAbstrM syms (CharStx i) = Right (CharT, CharStx i, syms)

-- S-Int
synthAbstrM syms (IntStx i) = Right (IntT, IntStx i, syms)

-- S-Double
synthAbstrM syms (DoubleStx i) = Right (DoubleT, DoubleStx i, syms)

-- S-Seq
synthAbstrM syms (SeqStx stxs) =
    do (ts, stxs', syms') <- synth [] [] syms stxs
       Right (kickForalls (TupT ts), SeqStx stxs', syms')
    where synth ts stxs syms [] = return (reverse ts, reverse stxs, syms)
          synth ts stxs syms (stx:stxs') =
            do (t, stx', syms') <- synthM syms stx
               synth (t:ts) (stx':stxs) syms' stxs'

-- S-Var
synthAbstrM ctx (IdStx name) =
    do (ctx', t) <- typeContext ctx name

       let !() = judgementM "=>Var"
                            ("ctx(" ++ name ++ ") = " ++ show t)
                            ("ctx |- " ++ name ++ " => " ++ show t ++ " -| ctx")

       return (t, IdStx (name, t, t), ctx')

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

-- S-Abs (unannotated) no rule in Pierce's paper, use Siek instead
synthAbstrM ctx (LambdaStx arg body) =
    do let arg' = '^':arg
           argT = ExistT arg'
           ctx' = insertContext ctx arg' (simpleType argT)
           ctx'' = insertContext ctx' arg (simpleType argT)
       (bodyT, body', ctx''') <- synthM ctx'' body
       return (ArrowT argT bodyT, LambdaStx arg body', ctx''')

-- S-App with lambda
synthAbstrM ctx stx@(AppStx fn@(LambdaStx x body) arg) =
    do (argT, arg', ctx') <- synthM ctx arg
       (rangeT, body', ctx'') <- synthBody ctx' x argT body
       
       let lambdaT = ArrowT argT rangeT

       (fn', ctx''') <- checkLambda ctx'' lambdaT fn

       let !() = judgementM "=>Lambda"
                            ("ctx1 |- " ++ showAbbrev arg ++ " => " ++ show argT ++ " -| ctx2 |- " ++ showAbbrev body ++ " => " ++ show rangeT ++ " -| ctx3 |- " ++ showAbbrev fn ++ " <= " ++ show lambdaT ++ " -| ctx4")
                            ("ctx1 |- " ++ showAbbrev stx ++ " => " ++ show lambdaT ++ " -| ctx4")

       Right (rangeT, AppStx fn' arg', ctx''')
    where synthBody ctx arg argT body =
            let ctx' = insertContext ctx arg (simpleType argT) in
            synthM ctx' body

          checkLambda ctx lambdaT stx =
            checkM lambdaT ctx stx

-- S-App
synthAbstrM ctx stx@(AppStx fn arg) =
    do (t, fn', ctx') <- synthAppFnM ctx fn
       synthApp t fn' ctx'
    where synthApp t@(ArrowT argT rangeT) fn' ctx' =
            do (arg', ctx'') <- checkM argT ctx' arg
               
               let !() = judgementM "=>Arrow"
                                    ("ctx1 |- " ++ showAbbrev fn ++ " => " ++ show t ++ " -| ctx2 |- " ++ showAbbrev arg ++ " <= " ++ show argT ++ " -| ctx3")
                                    ("ctx1 |- " ++ showAbbrev stx ++ " => " ++ show rangeT ++ " -| ctx3")

               return (rangeT, AppStx fn' arg', ctx'')

          synthApp (ExistT var) fn ctx =
            do let (ArrowT a1t a2t, ctx') = arrowifyVar ctx var
               (arg', ctx'') <- checkM a1t ctx' arg
               return (a2t, AppStx fn arg', ctx'')

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
    throwTypecheckerException $ "\n\n\tsynthAbstrM: unhandled case" ++
                                "\n\n\t stx = " ++ show stx ++ "\n"


checkM :: Type -> Context -> Stx String -> CheckM
checkM (ExistT var) ctx stx =
  do (ctx', t) <- typeContext ctx var
     checkInstM t ctx' stx
checkM t ctx stx = checkInstM t ctx stx


checkInstM :: Type -> Context -> Stx String -> CheckM

-- C-Forall-I
checkInstM (ForallT var t) ctx stx | isValueStx stx =
    do let ctx' = insertContext ctx var (simpleType (TvarT var))
       (stx', ctx'') <- checkM t ctx' stx
       return (stx', dropContext ctx'' var)

-- C-Int
checkInstM t syms (IntStx i) = (IntStx i,) <$> subM syms IntT t

-- C-Double
checkInstM t syms (DoubleStx d) = (DoubleStx d,) <$> subM syms DoubleT t

-- C-Char
checkInstM t syms (CharStx c) = (CharStx c,) <$> subM syms CharT t

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

-- checkInstM t ctx stx@(SeqStx stxs) =
--     do (seqT, stx', ctx') <- synthM ctx stx
--        -- edit: return value Stx
--        (stx',) <$> subM ctx' seqT t

-- -- C-Var
-- info: this is a particular instance of the subsumption rule (C-Sub below)?
-- checkInstM t syms (IdStx name) =
--     do idT <- typeContext syms name
--        (IdStx (name, idT, t),) <$> subM syms idT t

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

-- edit: make sure the existential var is unassigned?
checkInstM (ExistT var) ctx stx@(LambdaStx _ _) =
    let (existT, ctx') = arrowifyVar ctx var in
    checkInstM existT ctx' stx

-- -- C-App
-- edit: Why do I need this rule?
-- checkInstM t ctx (AppStx fn arg) =
--     do (ArrowT argT rangeT, fn', ctx') <- synthAppFnM ctx fn
--        (arg', ctx'') <- checkM argT ctx' arg
--        (AppStx fn' arg',) <$> subM ctx'' rangeT t

-- C-Where
checkInstM t syms stx@(WhereStx _ _) =
    typecheckWhereM (checkM t) syms stx

-- C-Sub
checkInstM t syms stx =
    do (t', stx', syms') <- synthAbstrM syms stx

       let !() = judgementM "=>Sub"
                            ("ctx1 |- " ++ showAbbrev stx ++ " => " ++ show t' ++ " -| ctx2 |- " ++ show t' ++ " <: " ++ show t)
                            ("ctx1 |- " ++ showAbbrev stx ++ " <= " ++ show t ++ " -| ctx2")

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
typecheckSrcFile fs srcfile@SrcFile { deps, renNs = Just ns } =
    do (t, ts) <- typecheckNamespace fs deps ns
       let names = Map.keys (SrcFile.symbols srcfile)
           ts' = Map.fromList $ catMaybes [ case Map.lookup name ts of
                                               Nothing -> Nothing
                                               Just t -> Just (name, t) | name <- names ]
       return (ts', t)


typecheck :: Map String SrcFile -> SrcFile -> Either String SrcFile
typecheck _ srcfile@SrcFile { t = CoreT } =
    return srcfile

typecheck _ srcfile@SrcFile { t = SrcT, renNs = Just (Namespace _ []) } =
    return srcfile

typecheck fs srcfile@SrcFile { t = SrcT } =
    do (ts, t) <- typecheckSrcFile fs srcfile
       return $ srcfile { ts = ts }

typecheck fs srcfile@SrcFile { t = InteractiveT } =
    Left "Typechecker.typecheck: for interactive srcfiles use 'typecheckInteractive' instead of 'typecheck'"


typecheckInteractive :: Map String SrcFile -> SrcFile -> Either String (SrcFile, Type)
typecheckInteractive fs srcfile =
     do (ts, t) <- typecheckSrcFile interactiveFs interactiveSrcfile
        return (srcfile { ts = Map.union (SrcFile.ts srcfile) ts }, t)
    where interactiveDeps =
            SrcFile.deps srcfile ++ ["Interactive"]
            
          interactiveSrcfile =
            srcfile { deps = interactiveDeps }

          interactiveFs =
            Map.insert "Interactive" srcfile fs