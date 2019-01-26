{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving #-}
module Typechecker.Typechecker where

import Control.Applicative (Applicative)
import Control.Monad (foldM, liftM)
import Control.Monad.State.Class
import Control.Monad.Trans.State (StateT, runStateT)
import Data.Char (chr)
import qualified Data.List as List
import qualified Data.Set as Set

import Data.Expr (DefnKw(..), Expr(..))
import qualified Data.PrettyString as PrettyString
import qualified Data.Name as Name
import qualified Pretty.Data.Expr as Pretty
import qualified Pretty.Stage.Typechecker as Pretty
import Typechecker.Context (Context, ContextVar(..))
import qualified Typechecker.Context as Context
import Typechecker.Type
import qualified Typechecker.Type as Type
import Typechecker.TypeName
import qualified Typechecker.TypeName as TypeName

import System.IO.Unsafe

data TypecheckerState
  = TypecheckerState { counter :: Int }

newtype Typechecker m a
  = Typechecker { unSubtype :: StateT TypecheckerState m a }
    deriving (Applicative, Functor, Monad, MonadState TypecheckerState)

runTypechecker :: Monad m => TypecheckerState -> Typechecker m a -> m a
runTypechecker context m = fst `liftM` runStateT (unSubtype m) context

genTypeName :: Monad m => Typechecker m TypeName
genTypeName =
  do state <- get
     put state { counter = counter state + 1 }
     return . TypeName $ counter state

genExistVar :: Monad m => Typechecker m Type
genExistVar = ExistVar `liftM` genTypeName

type SubtypeInstantiate = Typechecker

isSubtypeInstantiate
  :: Monad m => Context -> Type -> Type -> SubtypeInstantiate m Context
-- InstLSolve
isSubtypeInstantiate context type1@ExistVar {} type2
  | Type.isMonotype type2 &&
    Context.containsTypeUnassigned context type1 &&
    Context.isTypeWellFormed (Context.sliceAtType context type1) type2 =
    return $ Context.assignType context type1 type2
-- InstLReach
isSubtypeInstantiate context type1@ExistVar {} type2@ExistVar {}
  | Context.containsUnassignedTypes context [type1, type2] =
    return $ Context.assignType context type2 type1
-- InstLArr
isSubtypeInstantiate context type1@ExistVar {} (Arrow arg2 body2)
  | Context.containsTypeUnassigned context type1 =
    do bodyExistVar <- genExistVar
       argExistVar <- genExistVar
       let existVarSolution = Arrow argExistVar bodyExistVar
           context' = Context.assignTypeWithExistVars context type1 [bodyExistVar, argExistVar, existVarSolution]
       context'' <- isSubtypeInstantiate context' arg2 argExistVar
       isSubtypeInstantiate context'' bodyExistVar (Context.substitute context'' body2)
-- TODO List
isSubtypeInstantiate context type1@ExistVar {} (ListT listT)
  | Context.containsTypeUnassigned context type1 =
    do listExistVar <- genExistVar
       let existVarSolution = ListT listExistVar
           context' = Context.assignTypeWithExistVars context type1 [listExistVar, existVarSolution]
       isSubtypeInstantiate context' listExistVar listT
-- TODO Tuple
isSubtypeInstantiate context type1@ExistVar {} (TupleT tupleTs)
  | Context.containsTypeUnassigned context type1 =
    do existVars <- mapM (const genExistVar) tupleTs
       let existVarSolution = TupleT existVars
           context' = Context.assignTypeWithExistVars context type1 $ existVars ++ [existVarSolution]
       foldM (\c (var, typ) -> isSubtypeInstantiate c var typ) context' $ zip existVars tupleTs
-- InstLAllR
isSubtypeInstantiate context type1@ExistVar {} (Forall name type2)
  | Context.containsTypeUnassigned context type1 =
    do let context' = context `Context.appendType` TypeVar name
       context'' <- isSubtypeInstantiate context' type1 type2
       return $ context'' `Context.sliceAtType` TypeVar name
-- InstRSolve
isSubtypeInstantiate context type1 type2@ExistVar {}
  | Type.isMonotype type1 &&
    Context.containsTypeUnassigned context type2 &&
    Context.isTypeWellFormed (Context.sliceAtType context type2) type1 =
    return $ Context.assignType context type2 type1
-- InstRReach
isSubtypeInstantiate context type1@ExistVar {} type2@ExistVar {}
  | Context.containsUnassignedTypes context [type2, type1] =
    return $ Context.assignType context type1 type2
-- InstRArr
isSubtypeInstantiate context (Arrow arg1 body1) type2@ExistVar {}
  | Context.containsTypeUnassigned context type2 =
    do bodyExistVar <- genExistVar
       argExistVar <- genExistVar
       let existVarSolution = Arrow argExistVar bodyExistVar
           context' = Context.assignTypeWithExistVars context type2 [bodyExistVar, argExistVar, existVarSolution]
       context'' <- isSubtypeInstantiate context' argExistVar arg1
       isSubtypeInstantiate context'' (Context.substitute context'' body1) bodyExistVar
-- InstRAllL
isSubtypeInstantiate context (Forall name type1) type2@ExistVar {}
  | Context.containsTypeUnassigned context type2 =
    do existVar <- genExistVar
       let context' = Context.appendWithMarker context existVar
           type1' = Type.substituteTypeVar (TypeVar name) existVar type1
       context'' <- isSubtypeInstantiate context' type1' type2
       return $ Context.sliceAtMarker context'' existVar
-- TODO List
isSubtypeInstantiate context (ListT listT) type2@ExistVar {}
  | Context.containsTypeUnassigned context type2 =
    do listExistVar <- genExistVar
       let existVarSolution = ListT listExistVar
           context' = Context.assignTypeWithExistVars context type2 [listExistVar, existVarSolution]
       isSubtypeInstantiate context' listT listExistVar
-- TODO Tuple
isSubtypeInstantiate context (TupleT tupleTs) type2@ExistVar {}
  | Context.containsTypeUnassigned context type2 =
    do existVars <- mapM (const genExistVar) tupleTs
       let existVarSolution = TupleT existVars
           context' = Context.assignTypeWithExistVars context type2 $ existVars ++ [existVarSolution]
       foldM (\c (typ, var) -> isSubtypeInstantiate c typ var) context' $ zip tupleTs existVars
-- fail
isSubtypeInstantiate context type1 type2 =
  fail $ "subtype instantiate: failed" ++ "\n" ++
         "  type1 = " ++ show type1 ++ "\n" ++
         "  type2 = " ++ show type2 ++ "\n"

type Subtype = Typechecker

isSubtype :: Monad m => Context -> Type -> Type -> Subtype m Context
-- <:Var
isSubtype context type1@TypeVar {} type2@TypeVar {}
  | Context.containsType context type1 && type1 == type2 =
    return context
-- <:Unit
isSubtype context type1 type2
  | isUnit type1 && isUnit type2 =
    return context
isSubtype context type1@PrimitiveT {} type2@PrimitiveT {}
  | type1 == type2 =
    return context
-- <:Exvar
isSubtype context type1@ExistVar {} type2@ExistVar {}
  | Context.containsTypeUnassigned context type1 && type1 == type2 =
    return context
-- <:->
isSubtype context (Arrow arg1 body1) (Arrow arg2 body2) =
  do context' <- isSubtype context arg2 arg1
     isSubtype context'
               (Context.substitute context' body1)
               (Context.substitute context' body2)
-- <:ForallL
isSubtype context (Forall name type1) type2 =
  do existVar <- genExistVar
     let context' = Context.appendWithMarker context existVar
         type1' = Type.substituteTypeVar (TypeVar name) existVar type1
     isSubtype context' type1' type2
-- <:ForallR
isSubtype context type1 (Forall name type2) =
  do let context' = context `Context.appendType` TypeVar name
     context'' <- isSubtype context' type1 type2
     return $ Context.sliceAtType context' (TypeVar name)
-- <:InstatiateL
isSubtype context type1@ExistVar {} type2
  | Context.containsTypeUnassigned context type1 &&
    type1 `Set.notMember` Type.freeVars type2 =
    isSubtypeInstantiate context type1 type2
-- <:InstantiateR
isSubtype context type1 type2@ExistVar {}
  | Context.containsTypeUnassigned context type2 &&
    type2 `Set.notMember` Type.freeVars type1 =
    isSubtypeInstantiate context type1 type2
-- TODO List
isSubtype context (ListT type1) (ListT type2) =
  isSubtype context type1 type2
-- TODO Tuple
isSubtype context (TupleT types1) (TupleT types2) =
  foldM (\c (type1, type2) -> isSubtype c type1 type2) context $ zip types1 types2
-- fail
isSubtype context type1 type2 =
  fail $ "subtype: failed" ++ "\n" ++
         "  type1 = " ++ show type1 ++ "\n" ++
         "  type2 = " ++ show type2 ++ "\n"

type Check = Typechecker

check :: Monad m => Context -> Expr -> Type -> Check m Context
-- 1I
--
-- We don't need an implementation for 1I because unit is not a term
-- in the 'Expr' language, it is a function ("mkTuple0").
check context (LiteralE literal) typ@PrimitiveT {}
  | literalT literal == typ =
    return context
-- ForallI
check context term (Forall name typ) =
  do let context' = context `Context.appendType` TypeVar name
     context'' <- check context' term typ
     return $ context'' `Context.sliceAtType` TypeVar name
-- ->I
check context (LambdaE arg body) (Arrow argType bodyType) =
  -- TODO: handle type from 'arg'
  do let context' = Context.appendTermAssign context (IdE arg) argType
     context'' <- check context' body bodyType
     return $ context'' `Context.sliceAtTermVar` IdE arg
-- Let
-- check context (LetE arg val body) typ =
--   do (context', valType) <- synthesize context val
--      let context'' = Context.appendTermAssign context' (Var arg) valType
--      check context'' body typ
-- Sub
check context term typ =
  do (context', typ') <- synthesize context term
     isSubtype context'
               (Context.substitute context' typ')
               (Context.substitute context' typ)
-- check _ term typ =
--   fail . PrettyString.toString $
--     Pretty.devIncompleteCheck (Pretty.docExpr term) (show typ)

type SynthesizeApply = Typechecker

synthesizeApply :: Monad m => Context -> Type -> Expr -> SynthesizeApply m (Context, Type)
-- ForallApp
synthesizeApply context (Forall name typ) term =
  do existVar <- genExistVar
     let context' = context `Context.appendType` existVar
         typ' = Type.substituteTypeVar (TypeVar name) existVar typ
     synthesizeApply context' typ' term
-- âApp
synthesizeApply context typ@ExistVar {} term =
  do argExistVar <- genExistVar
     bodyExistVar <- genExistVar
     arrowExistVar <- genExistVar
     let context' = context `Context.append` (ContextType bodyExistVar, Nothing)
                            `Context.append` (ContextType argExistVar, Nothing)
                            `Context.append` (ContextType arrowExistVar, Just (Arrow argExistVar bodyExistVar))
     context'' <- check context' term argExistVar
     return (context'', bodyExistVar)
-- ->App
synthesizeApply context (Arrow argType bodyType) term =
  do context' <- check context term argType
     return (context', bodyType)
-- CatchAll
synthesizeApply context typ term = do
  fail . PrettyString.toString $
    Pretty.devIncompleteSynthesizeApply (Pretty.docExpr term) (show typ)

type Synthesize = Typechecker

synthesize :: Monad m => Context -> Expr -> Synthesize m (Context, Type)
-- Var
synthesize context term@(IdE name)
  | Context.containsTermAssigned context name =
    do let Just typ = Context.lookupTerm context term
       return (context, typ)
-- Anno
synthesize context (AnnotationE term typ)
  | Context.isTypeWellFormed context typ =
    do context' <- check context term typ
       return (context', typ)
-- 1I=>
synthesize context (LiteralE literal) =
  return (context, literalT literal)
-- ->I=>
synthesize context (LambdaE arg body) =
  do argExistVar <- genExistVar
     bodyExistVar <- genExistVar
     let context' = context `Context.append` (ContextType argExistVar, Nothing)
                            `Context.append` (ContextType bodyExistVar, Nothing)
                            `Context.append` (ContextTermVar arg, Just argExistVar)
     context'' <- check context' body bodyExistVar
     return $ (context'' `Context.sliceAtTermVar` IdE arg,
               Arrow argExistVar bodyExistVar)
-- ->E
synthesize context (AppE fun arg) =
  do (context', funType) <- synthesize context fun
     unsafePerformIO $ do
       putStrLn $ show (Pretty.docExpr fun) ++ " :: " ++ show funType
       return $ return (context', funType)
     synthesizeApply context' (Context.substitute context' funType) arg
-- CondE
synthesize context (CondE matches _) =
  synthesizeFirstMatch context matches
  where
    checkMatches :: Monad m => Context -> [(Expr, Expr)] -> Type -> Check m Context
    checkMatches context [] _ = return context
    checkMatches context ((pred, body):matches) typ =
      do context' <- check context pred Type.boolT
         context'' <- check context' body typ
         checkMatches context'' matches typ

    synthesizeFirstMatch :: Monad m => Context -> [(Expr, Expr)] -> Synthesize m (Context, Type)
    synthesizeFirstMatch context ((pred, body):matches) =
      do context' <- check context pred Type.boolT
         (context'', typ) <- synthesize context' body
         context''' <- checkMatches context'' matches typ
         return (context''', typ)
-- FnDecl (NrDef)
synthesize context (FnDecl NrDef fnName fnBody) =
  do (context', fnBodyType) <- synthesize context fnBody
     let context'' = Context.appendTermAssign context' (IdE fnName) fnBodyType
     return (context'', fnBodyType)
-- FnDecl (Def)
synthesize context (FnDecl Def fnName fnBody) =
  do existVar <- genExistVar
     let context' =
           Context.appendTermAssign (context `Context.appendType` existVar) (IdE fnName) existVar
     synthesize context' fnBody
-- Let
synthesize context (LetE fnDecl body) =
  do (context', _) <- synthesize context fnDecl
     synthesize context' body
-- CatchAll
synthesize _ term =
  fail . PrettyString.toString $
    Pretty.devIncompleteSynthesize (Pretty.docExpr term)

typecheck :: Monad m => Context -> Expr -> Maybe Type -> m (Context, Type)
typecheck context term =
  \case
    Nothing ->
      do (context', typ) <- runTypechecker (TypecheckerState initialCounter) (synthesize context term)
         return (context', Type.rebuildForall $ Context.substitute context' typ)
    Just typ ->
      do context' <- runTypechecker (TypecheckerState initialCounter) (check context term typ)
         return (context', typ)
  where
    typeInt typ | isUnit typ = 0
    typeInt PrimitiveT {} = 0
    typeInt (TypeVar (TypeName x)) = x
    typeInt (ExistVar (TypeName x)) = x
    typeInt (Forall (TypeName x) _) = x
    typeInt (Arrow type1 type2) = max (typeInt type1) (typeInt type2)
    typeInt (ListT typ) = typeInt typ
    typeInt (TupleT types) = foldl max 0 $ map typeInt types

    contextInt (ContextType typ) = typeInt typ
    contextInt _ = 0

    initialCounter :: Int
    initialCounter
      | List.null context = 0
      | otherwise = maximum $ map (contextInt . fst) context
