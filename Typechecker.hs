{-# LANGUAGE BangPatterns, ParallelListComp, TupleSections #-}
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
debugT desc = debug && trace desc True


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

consistentT syms DynT _ = return syms
consistentT syms _ DynT = return syms

-- →
consistentT syms (ArrowT argT1 rangeT1) (ArrowT argT2 rangeT2) =
    do syms' <- consistentT syms argT2 argT1
       consistentT syms' rangeT1 rangeT2

-- αRefl
-- info: this rule is already contained in the atomic eq test rule
-- consistentT syms (TvarT var1) (TvarT var2)
--     | var1 == var2 = return syms

-- ∀Lα̂
consistentT syms (ForallT vars t1) t2 =
    let (syms', t1') = synthForall syms t1 vars in
    consistentT syms' t1' t2

-- ∀Lα̂
consistentT syms t1 (ForallT vars t2) =
    let (syms', t2') = synthForall syms t2 vars in
    do syms'' <- consistentT syms' t1 t2'
       return $ dropVars' syms'' vars 

-- α̂Refl
consistentT syms (ExistT var1) (ExistT var2)
    | var1 == var2 = return syms

-- α̂⁼L
consistentT syms t1 t2@(ExistT var)
    | isAtomicT t1 = return (updateContext syms var (unifType t2 t1))

-- α̂⁼R
consistentT syms t1@(ExistT var) t2
    | isAtomicT t2 = return (updateContext syms var (unifType t1 t2))

consistentT _ _ _ = Nothing


(~~) = consistentT


type Context = [(String, (Type, Maybe Type))]


insertContext :: [(a, b)] -> a -> b -> [(a, b)]
insertContext syms name t = (name, t):syms


updateContext :: Context -> String -> (Type, Maybe Type) -> Context
updateContext syms name t =
  case splitContext syms name of
    (syms1, syms2) -> syms1 ++ [(name, t)] ++ syms2


lookupContext :: (Eq a) => [(a, b)] -> a -> Maybe b
lookupContext syms name = lookup name syms


typeContext syms name =
    case lookupContext syms name of
      Nothing -> Left $ "\n\n\ttypeContext: name " ++ show name ++ " not bound\n"
      Just (t, Nothing) -> Right t
      Just (_, Just t) -> Right t


splitContext :: Context -> String -> (Context, Context)
splitContext syms name =
  case span neqName syms of
    (_, []) -> error "splitContext: empty list"
    (syms1, _:syms2) -> (syms1, syms2)
  where neqName (name', _) = name /= name'


dropContext :: Context -> String -> Context
dropContext syms name = tail $ dropWhile neqName syms
  where neqName (name', _) = name /= name'


nothingSyms :: Map String Type -> Context
nothingSyms syms = map (\(name, t) -> (name, (t, Nothing))) $ Map.toList syms


simpleSyms :: Context -> Map String Type
simpleSyms syms = Map.fromList $ map simple syms
  where simple (name, (t, Nothing)) = (name, t)
        simple (name, (t, Just t'))
           | debugT ("simpleSyms: " ++ show t ++ " = " ++ show t') = (name, t)


arrowifyTvar :: Context -> String -> (Type, Context)
arrowifyTvar syms tvar =
  let
    (syms1, syms2) = splitContext syms tvar
    a1 = ("a1", (ExistT "a1", Nothing))
    a2 = ("a2", (ExistT "a2", Nothing))
    at = ArrowT (ExistT "a1") (ExistT "a2")
    a  = ("a", (ExistT "a", Just at))
  in
   (at, syms1 ++ [a, a2, a1] ++ syms2)


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
substituteExistTs syms (ExistT var) = case typeContext syms var of Right t -> t
substituteExistTs syms (ForallT _ t) = substituteExistTs syms t
substituteExistTs syms t@(TvarT _) = t


synthForall syms t [] = (syms, t)
synthForall syms t (var:vars) =
  let syms' = insertContext syms var (simpleType (ExistT var)) in
  synthForall syms' (substituteT (ExistT var) var t) vars


insertVars syms [] = syms
insertVars syms (var:vars) =
    insertVars (insertContext syms var (simpleType (TvarT var))) vars


dropVars [] m = m
dropVars (var:vars) m =
    dropM var (dropVars vars m)


dropVar syms name =
     dropContext syms name


dropVars' syms [] = syms
dropVars' syms (var:vars) =
    dropVar (dropVars' syms vars) var


synthQuantifiersM :: SynthM -> SynthM
synthQuantifiersM _ | debugF "synthQuantifiersM" = undefined
synthQuantifiersM m =
    do (t, stx, syms) <- m
       case t of
         ForallT vars t' | debugT ("synthQuantifiersM(ForallT): " ++ show t) ->
           let (syms', t'') = synthForall syms t' vars in
           synthQuantifiersM $ return (t'', stx, syms')
         _ | debugT ("synthQuantifiersM: " ++ show t) ->
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


checkExistM :: Type -> [(String, (Type, Maybe Type))] -> Stx String -> CheckM
checkExistM _ _ _ | debugF "checkExistM" = undefined
checkExistM (ExistT var) syms stx =
    do t <- typeContext syms var
       checkM t syms stx


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

-- S-Char
synthM _ (CharStx _) | debugF "synthM: CharStx" = undefined
synthM syms (CharStx i) =
    Right (CharT, CharStx i, syms)

-- S-Int
synthM _ (IntStx _) | debugF "synthM: IntStx" = undefined
synthM syms (IntStx i) =
    Right (IntT, IntStx i, syms)

-- S-Double
synthM _ (DoubleStx _) | debugF "synthM: DoubleStx" = undefined
synthM syms (DoubleStx i) =
    Right (DoubleT, DoubleStx i, syms)

-- S-Seq
synthM _ (SeqStx _) | debugF "synthM: SeqStx" = undefined
synthM syms (SeqStx stxs) =
    do (ts, stxs', syms') <- synth [] [] syms stxs
       Right (TupT ts, SeqStx stxs', syms')
    where synth ts stxs syms [] = return (reverse ts, reverse stxs, syms)
          synth ts stxs syms (stx:stxs') =
            do (t, stx', syms') <- synthM syms stx
               synth (t:ts) (stx':stxs) syms' stxs'

-- S-Var
synthM _ (IdStx name) | debugF ("synthM: IdStx: " ++ show name) = undefined
synthM syms (IdStx name) =
    do t <- typeContext syms name
       (synthT, _, _) <- synthQuantifiersM $ return (t, IdStx (name, t, t), syms)
       let !_ | debugT ("synthM: IdStx: " ++ show name ++ " :: " ++ show t ++ " => " ++ show synthT) = True
       synthQuantifiersM $ return (t, IdStx (name, t, t), syms)

-- S-Abs (annotated)
-- synthM syms Synth stx@(LambdaStx arg body) =
--     error "synthM: LambdaStx (annotated): Synth: not implemented"

-- S-Abs (unannotated) no rule in Pierce's paper, use Siek instead
-- edit: to fix
-- synthM syms (LambdaStx arg body) =
--     do (bodyT, body', syms') <- synthM (insertContext syms arg (simpleType DynT)) body
--        Right (ArrowT (argT body') bodyT, LambdaStx arg body', syms')
--     where argT body =
--               case find (\(IdStx (name, _, _)) -> name == arg) $ idsOf body of
--                 Nothing -> DynT
--                 Just (IdStx (_, _, argT)) -> argT

-- S-App with lambda
-- edit: to fix
-- synthM syms (AppStx fn@(LambdaStx x body) arg) =
--     do (argT, arg', syms') <- synthM syms arg
--        (rangeT, body', syms'') <- synthM (insertContext syms' x (simpleType argT)) body
--        (fn', syms''') <- checkM (ArrowT argT rangeT) syms'' fn
--        Right (rangeT, AppStx fn' arg', syms''')

-- S-App
synthM _ (AppStx _ _) | debugF "synthM: AppStx" = undefined
synthM syms (AppStx fn arg) =
    do (t, fn', syms') <- synthAppFnM syms fn
       (t', _, _) <- synthQuantifiersM $ case t of
                                          ArrowT _ _ -> synthArrow t fn' syms'
                                          ExistT _ -> synthExist t fn' syms'
       let !_ | trace ("synthM: AppStx: " ++ show t') True = True
       synthQuantifiersM $ case t of
                             ArrowT _ _ -> synthArrow t fn' syms'
                             ExistT _ -> synthExist t fn' syms'
    where synthArrow (ArrowT argT rangeT) fn' syms' =
            do (arg', syms'') <- checkM argT syms' arg
               return (rangeT, AppStx fn' arg', syms'')

          synthExist (ExistT var) fn syms =
            do let (ArrowT a1t a2t, syms') = arrowifyTvar syms var
               (arg', syms'') <- checkM a1t syms' arg
               return (a2t, AppStx fn arg', syms'')

-- S-Others
synthM _ (WhereStx _ _) | debugF "synthM: WhereStx" = undefined
synthM syms stx@(WhereStx _ _) =
    typecheckWhereM synthM syms stx

synthM _ (DefnStx _ name _) | debugF ("synthM: DefnStx: " ++ show name) = undefined
synthM syms (DefnStx Def name body) =
    do (bodyT, body', syms') <- synthM (insertContext syms name (simpleType DynT)) body
       Right (bodyT, DefnStx Def name body', insertContext syms' name (simpleType bodyT))

synthM syms (DefnStx NrDef name body) =
    do (bodyT, body', syms') <- synthM syms body
       Right (bodyT, DefnStx NrDef name body', insertContext syms' name (simpleType bodyT))

synthM _ stx =
    Left $ "\n\n\tsynthM: unhandled case" ++
           "\n\n\t stx = " ++ show stx ++ "\n"


checkM :: Type -> Context -> Stx String -> CheckM

-- C-Forall-I
checkM (ForallT _ _) _ _ | debugF "checkM: Forall" = undefined
checkM (ForallT vars t) syms stx | isValueStx stx =
    dropVars vars $ checkM t (insertVars syms vars) stx
    where insertVars syms [] = syms
          insertVars syms (var:vars) =
            insertVars (insertContext syms var (simpleType (TvarT var))) vars

          dropVars [] m = m
          dropVars (var:vars) m =
            dropM var (dropVars vars m)

-- C-Int
checkM t _ (IntStx _) | debugF ("checkM: IntStx: " ++ show t) = undefined
checkM t syms (IntStx i) =
    case (~~) syms IntT t of
      Nothing -> Left $ "\n\n\tcheckM: IntStx: type inconsistency" ++
                        "\n\n\t\t" ++ show IntT ++ " ~~ " ++ show t ++ " (false)\n"
      Just syms' -> return (IntStx i, syms')

-- C-Double
checkM t _ (DoubleStx _) | debugF ("checkM: DoubleStx: " ++ show t) = undefined
checkM t syms (DoubleStx d) =
    case (~~) syms DoubleT t of
      Nothing -> Left $ "\n\n\tcheckM: DoubleStx: type inconsistency" ++
                        "\n\n\t\t" ++ show DoubleT ++ " ~~ " ++ show t ++ " (false)\n"
      Just syms' -> return (DoubleStx d, syms')

-- C-Char
checkM t _ (CharStx _) | debugF ("checkM: CharStx: " ++ show t) = undefined
checkM t syms (CharStx i) =
    case (~~) syms CharT t of
      Nothing -> Left $ "\n\n\tcheckM: CharStx: type inconsistency" ++
                        "\n\n\t\t" ++ show CharT ++ " ~~ " ++ show t ++ " (false)\n"
      Just syms' -> return (CharStx i, syms')

-- C-Seq
checkM t _ (SeqStx _) | debugF ("checkM: SeqStx: " ++ show t) = undefined
checkM t@(TupT ts) syms (SeqStx stxs) =
    if length ts == length stxs
    then checkSeqM ts syms stxs
    else Left $ "\n\n\tcheckM: SeqStx: different length" ++
                "\n\n\t ts = " ++ show ts ++
                "\n\n\t t = " ++ show t ++
                "\n\n\t stxs = " ++ show stxs ++ "\n"

checkM (SeqT t) syms (SeqStx stxs) =
    checkSeqM (replicate (length stxs) t) syms stxs

checkM t@DynT syms (SeqStx stxs) =
    checkSeqM (replicate (length stxs) t) syms stxs

-- edit: to be finished
-- checkM (ExistT _) syms stx@(SeqStx _) = undefined

-- C-Var
checkM t _ (IdStx name) | debugF ("checkM: IdStx: " ++ name ++ ": " ++ show t) = undefined
checkM t@(ExistT var) syms (IdStx name) =
  do idT <- typeContext syms name
     let idT' = case idT of
                  ForallT vars t -> foldr (\var t -> substituteT (ExistT var) var t) t vars
                  _ -> idT
     return (IdStx (name, idT', t), updateContext syms var (t, Just idT'))

checkM t syms (IdStx name) =
    do idT <- typeContext syms name
       case (~~) syms idT t of
         Nothing -> Left $ "\n\n\tcheckM: IdStx: type inconsistency" ++
                           "\n\n\t\t" ++ name ++ " :: " ++ show idT ++ " ~~ " ++ show t ++ " (false)\n"
         Just syms' -> return (IdStx (name, idT, t), syms)

-- C-Abs (annotated)
-- checkM syms (Check _) stx@(LambdaStx arg body) =
--     error "checkM: LambdaStx (annotated): Check: not implemented"

-- C-Abs (unannotated)
checkM t _ (LambdaStx _ _) | debugF ("checkM: LambdaStx: " ++ show t) = undefined
checkM (ArrowT argT rangeT) syms (LambdaStx arg body) =
    do (body', syms') <- checkM rangeT (insertContext syms arg (simpleType argT)) body
       Right (LambdaStx arg body', dropContext syms' arg)

checkM t@DynT syms (LambdaStx arg body) =
    do (body', syms') <- checkM t (insertContext syms arg (simpleType DynT)) body
       Right (LambdaStx arg body', syms')

checkM (ExistT var) syms stx@(LambdaStx _ _) =
    let (existT, syms') = arrowifyTvar syms var in
    checkM existT syms' stx

-- C-App
checkM t _ (AppStx _ _) | debugF ("checkM: AppStx: " ++ show t) = undefined
checkM t@(ExistT _) syms stx@(AppStx _ _) =
    checkExistM t syms stx

checkM t syms (AppStx fn arg) =
    do (ArrowT argT rangeT, fn', syms') <- synthAppFnM syms fn
       (arg', syms'') <- checkM argT syms' arg
       case (~~) syms'' rangeT t of
         Nothing -> Left $ "\n\n\tcheckM: AppStx: type inconsistency" ++
                           "\n\n\t\t" ++ show rangeT ++ " ~~ " ++ show t ++ " (false)\n"
         Just syms''' -> return (AppStx fn' arg', syms''')

-- C-Others
checkM t _ (WhereStx _ _) | debugF ("checkM: WhereStx: " ++ show t) = undefined
checkM t@(ExistT _) syms stx@(WhereStx _ _) =
    checkExistM t syms stx

checkM t syms stx@(WhereStx _ _) =
    typecheckWhereM (checkM t) syms stx

checkM t _ stx =
    Left $ "\n\n\tcheckM: unhandled case" ++
           "\n\n\t t = " ++ show t ++
           "\n\n\t stx = " ++ show stx ++ "\n"


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