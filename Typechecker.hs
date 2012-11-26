{-# LANGUAGE BangPatterns, NamedFieldPuns,
             ParallelListComp, TupleSections #-}
module Typechecker where

import Control.Monad.Error
import Data.Functor ((<$>))
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Data.Type
import Data.Stx


import Debug.Trace


debug = True
debugF desc = debug && trace desc False
debugT desc = (debug && trace desc True) || True
debugIfF debug desc = debug && trace desc False
debugIfT debug desc = (debug && trace desc True) || True


idsOf :: Stx a -> [Stx a]
idsOf (IntStx _) = []
idsOf (DoubleStx _) = []
idsOf (CharStx _) = []
idsOf (SeqStx stxs) = concatMap idsOf stxs
idsOf stx@(IdStx val) = [stx]
idsOf (AppStx stx1 stx2) = idsOf stx1 ++ idsOf stx2
idsOf (DefnStx _ _ body) = idsOf body
idsOf (LambdaStx _ body) = idsOf body
idsOf (WhereStx stx ("", stxs)) = idsOf stx ++ concatMap idsOf stxs


consistentT :: Context -> Type -> Type -> Maybe Context
consistentT _ t1 t2 | debugF ("consistentT: " ++ show t1 ++ " ~~ " ++ show t2) = undefined

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
      return (updateContext syms var (unifType t2 t1))

-- α̂⁼R
consistentT syms t1@(ExistT var) t2
    | isEmptyTypeContext syms var && isAtomicT t2 = 
      return (updateContext syms var (unifType t1 t2))

-- ∀Lα̂
consistentT syms (ForallT vars t1) t2 =
    let (syms', t1') = synthForall syms t1 vars in
    consistentT syms' t1' t2

-- ∀R
consistentT syms t1 (ForallT vars t2) =
    let (syms', t2') = synthForall syms t2 vars in
    do syms'' <- consistentT syms' t1 t2'
       return $ dropVars syms'' vars 

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

consistentM syms t1 t2 =
  case consistentT syms t1 t2 of
    Nothing -> Left $ "\n\ntype inconsistency: " ++ show t1 ++ "~~" ++ show t2 ++ " (false)\n"
    Just syms' -> Right syms'


data Context =
  Context { syms :: [(String, (Type, Maybe Type))]
          , count :: Int }
  deriving (Show)


insertContext :: Context -> String -> (Type, Maybe Type) -> Context
insertContext ctx@Context { syms } name t =
  ctx { syms = (name, t):syms }


updateContext :: Context -> String -> (Type, Maybe Type) -> Context
updateContext ctx name t =
  let (Context { syms = syms1 }, Context { syms = syms2 }) = splitContext ctx name in
  ctx { syms = syms1 ++ [(name, t)] ++ syms2 }


lookupContext :: Context -> String -> Maybe (Type, Maybe Type)
lookupContext Context { syms } name = lookup name syms


typeContext :: Context -> String -> Either String Type
typeContext syms name =
    case lookupContext syms name of
      Nothing -> Left $ "\n\n\ttypeContext: name " ++ show name ++ " not bound\n"
      Just (t, Nothing) -> Right t
      Just (_, Just t) -> Right t


isEmptyTypeContext :: Context -> String -> Bool
isEmptyTypeContext syms name =
    case lookupContext syms name of
      Nothing -> error $ "\n\n\tisEmptyTypeContext: name " ++ show name ++ " not bound\n"
      Just (_, Nothing) -> True
      Just _ -> False


splitContext :: Context -> String -> (Context, Context)
splitContext ctx@Context { syms } name =
  case span neqName syms of
    (_, []) -> error "splitContext: empty list"
    (syms1, _:syms2) -> (ctx { syms = syms1 }, ctx { syms = syms2 })
  where neqName (name', _) = name /= name'


dropContext :: Context -> String -> Context
dropContext ctx@Context { syms } name =
  ctx { syms = tail $ dropWhile neqName syms }
  where neqName (name', _) = name /= name'


nothingSyms :: Map String Type -> Context
nothingSyms syms = Context { syms = syms', count = 0 }
  where syms' = map (\(name, t) -> (name, (t, Nothing))) $ Map.toList syms


simpleSyms :: Context -> Map String Type
simpleSyms Context { syms } = Map.fromList $ map simple syms
  where simple (name, (t, Nothing)) = (name, t)
        simple (name, (t, Just t')) = (name, t')


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
    Nothing -> error $ "substituteExistTs: " ++ show var
    Just (ExistT var', Nothing) | var == var' -> t
    Just (t', Nothing) -> substituteExistTs syms t'
    Just (_, Just t') -> substituteExistTs syms t'

substituteExistTs syms (ForallT _ t) = substituteExistTs syms t
substituteExistTs syms t@(TvarT _) = t


synthForall syms t [] = (syms, t)
synthForall syms t (var:vars) =
  let syms' = insertContext syms var (simpleType (ExistT var)) in
  synthForall syms' (substituteT (ExistT var) var t) vars


insertVars syms [] = syms
insertVars syms (var:vars) =
    insertVars (insertContext syms var (simpleType (TvarT var))) vars


dropVars syms [] = syms
dropVars syms (var:vars) =
    dropContext (dropVars syms vars) var


dbQuantifiersM = False

synthQuantifiersM :: SynthM -> SynthM
synthQuantifiersM m =
    do (t, stx, syms) <- m
       case t of
         ForallT vars t' | debugIfT dbQuantifiersM ("synthQuantifiersM: ForallT: " ++ show t) ->
           let (syms', t'') = synthForall syms t' vars in
           synthQuantifiersM $ return (t'', stx, syms')
         _ | debugIfT dbQuantifiersM ("synthQuantifiersM: " ++ show t) ->
           return (substituteExistTs syms t, stx, syms)


synthAppFnM :: Context -> Stx String -> SynthM
synthAppFnM _ _ | debugF "synthAppFnM" = undefined
synthAppFnM syms fn =
    do (t, stx, syms') <- synthM syms fn
       t' <- do case t of
                  ArrowT _ _ -> return t
                  DynT -> return $ ArrowT DynT DynT
                  t -> Left $ "\n\n\tsynthAppFnM: expected arrow type" ++
                              "\n\n\t t = " ++ show t ++ "\n"
       synthQuantifiersM $ return (t', stx, syms')


checkSeqM :: [Type] -> Context -> [Stx String] -> CheckM
checkSeqM _ _ _ | debugF "checkSeqM" = undefined
checkSeqM ts syms stxs = check [] ts syms stxs
  where check stxs ts syms [] = return (SeqStx (reverse stxs), syms) 
        check stxs (t:ts) syms (stx:stxs') =
          do (stx', syms') <- checkM t syms stx
             check (stx':stxs) ts syms' stxs'


typecheckWhereM :: (Context -> Stx String -> TypecheckerM a) -> Context -> Stx String -> TypecheckerM a
typecheckWhereM _ _ _ | debugF "typecheckWhereM" = undefined
typecheckWhereM m syms (WhereStx stx ("", stxs)) =
    check syms stxs
    where check syms [] = m syms stx
          check syms (stx:stxs) =
              do (_, _, syms') <- synthM syms stx
                 check syms' stxs


dropM :: String -> CheckM -> CheckM
dropM name m =
  do (stx, syms) <- m
     return (stx, dropContext syms name)


simpleType :: a -> (a, Maybe b)
simpleType t = (t, Nothing)


unifType :: a -> b -> (a, Maybe b)
unifType t1 t2 = (t1, Just t2)


synthM :: Context -> Stx String -> SynthM
synthM syms stx =
  synthQuantifiersM $ synthAbstrM syms stx


synthAbstrM :: Context -> Stx String -> SynthM

-- S-Char
synthAbstrM _ (CharStx _) | debugF "synthAbstrM: CharStx" = undefined
synthAbstrM syms (CharStx i) =
    Right (CharT, CharStx i, syms)

-- S-Int
synthAbstrM _ (IntStx _) | debugF "synthAbstrM: IntStx" = undefined
synthAbstrM syms (IntStx i) =
    Right (IntT, IntStx i, syms)

-- S-Double
synthAbstrM _ (DoubleStx _) | debugF "synthAbstrM: DoubleStx" = undefined
synthAbstrM syms (DoubleStx i) =
    Right (DoubleT, DoubleStx i, syms)

-- S-Seq
synthAbstrM _ (SeqStx _) | debugF "synthAbstrM: SeqStx" = undefined
synthAbstrM syms (SeqStx stxs) =
    do (ts, stxs', syms') <- synth [] [] syms stxs
       Right (TupT ts, SeqStx stxs', syms')
    where synth ts stxs syms [] = return (reverse ts, reverse stxs, syms)
          synth ts stxs syms (stx:stxs') =
            do (t, stx', syms') <- synthM syms stx
               synth (t:ts) (stx':stxs) syms' stxs'

-- S-Var
synthAbstrM _ (IdStx name) | debugF ("synthAbstrM: IdStx: " ++ show name) = undefined
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
synthAbstrM _ (AppStx _ _) | debugF "synthAbstrM: AppStx" = undefined
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
synthAbstrM _ (WhereStx _ _) | debugF "synthAbstrM: WhereStx" = undefined
synthAbstrM syms stx@(WhereStx _ _) =
    typecheckWhereM synthM syms stx

synthAbstrM _ (DefnStx _ name _) | debugF ("synthAbstrM: DefnStx: " ++ show name) = undefined
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
checkInstM (ForallT _ _) _ _ | debugF "checkInstM: ForallT" = undefined
checkInstM (ForallT vars t) syms stx | isValueStx stx =
    dropVarsM vars $ checkM t (insertVars syms vars) stx
    where insertVars syms [] = syms
          insertVars syms (var:vars) =
            insertVars (insertContext syms var (simpleType (TvarT var))) vars

          dropVarsM [] m = m
          dropVarsM (var:vars) m =
            dropM var (dropVarsM vars m)

-- C-Int
checkInstM t _ (IntStx _) | debugF ("checkInstM: IntStx <= " ++ show t) = undefined
checkInstM t syms (IntStx i) = (IntStx i,) <$> consistentM syms IntT t

-- C-Double
checkInstM t _ (DoubleStx _) | debugF ("checkInstM: DoubleStx <= " ++ show t) = undefined
checkInstM t syms (DoubleStx d) = (DoubleStx d,) <$> consistentM syms DoubleT t

-- C-Char
checkInstM t _ (CharStx _) | debugF ("checkInstM: CharStx <= " ++ show t) = undefined
checkInstM t syms (CharStx c) = (CharStx c,) <$> consistentM syms CharT t

-- C-Seq
checkInstM t _ (SeqStx _) | debugF ("checkInstM: SeqStx <= " ++ show t) = undefined
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
-- checkInstM t _ (IdStx name) | debugF ("checkInstM: IdStx " ++ name ++ " <= " ++ show t) = undefined
-- checkInstM t syms (IdStx name) =
--     do idT <- typeContext syms name
--        (IdStx (name, idT, t),) <$> consistentM syms idT t

-- C-Abs (annotated)
-- checkInstM syms (Check _) stx@(LambdaStx arg body) =
--     error "checkInstM: LambdaStx (annotated): Check: not implemented"

-- C-Abs (unannotated)
checkInstM t _ (LambdaStx _ _) | debugF ("checkInstM: LambdaStx <= " ++ show t) = undefined
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
checkInstM t _ (AppStx _ _) | debugF ("checkInstM: AppStx <= " ++ show t) = undefined
checkInstM t syms (AppStx fn arg) =
    do (ArrowT argT rangeT, fn', syms') <- synthAppFnM syms fn
       (arg', syms'') <- checkM argT syms' arg
       (AppStx fn' arg',) <$> consistentM syms'' rangeT t

-- C-Others
checkInstM t _ (WhereStx _ _) | debugF ("checkInstM: WhereStx <= " ++ show t) = undefined
checkInstM t syms stx@(WhereStx _ _) =
    typecheckWhereM (checkM t) syms stx

-- C-Sub
checkInstM t _ _ | debugF ("checkInstM: sub: <= " ++ show t) = undefined
checkInstM t syms stx =
    do (t', stx', syms') <- synthAbstrM syms stx
       (stx',) <$> consistentM syms' t' t


typecheckIncremental :: Map String Type -> Stx String -> TypecheckerM (Type, Map String Type)
typecheckIncremental syms stx =
    do (t, _, syms') <- synthM (nothingSyms syms) stx
       return (t, simpleSyms syms')


typecheckStxs :: Map String Type -> [Stx String] -> TypecheckerM (Map String Type)
typecheckStxs syms stxs = typecheck (nothingSyms syms) stxs
    where typecheck syms [] = return $ simpleSyms syms
          typecheck syms (stx:stxs) =
            do (_, _, syms') <- synthM syms stx
               typecheck syms' stxs