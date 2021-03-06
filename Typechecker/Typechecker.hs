{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving #-}
module Typechecker.Typechecker where

import Prelude hiding (pred)

import Control.Applicative (Applicative)
import Control.Arrow ((***))
import Control.Monad (foldM, liftM)
import Control.Monad.State.Class
import Control.Monad.Trans.State (StateT, runStateT)
import qualified Data.List as List
import Data.Maybe (fromJust)
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

data TypecheckerState
  = TypecheckerState { counter :: Int }

newtype Typechecker m a
  = Typechecker { unTypechecker :: StateT TypecheckerState m a }
    deriving (Applicative, Functor, Monad, MonadState TypecheckerState)

runTypechecker :: Monad m => TypecheckerState -> Typechecker m a -> m a
runTypechecker context m = fst `liftM` runStateT (unTypechecker m) context

genTypeName :: Monad m => Typechecker m TypeName
genTypeName =
  do s <- get
     put s { counter = counter s + 1 }
     return . TypeName $ counter s

genExistVar :: Monad m => Typechecker m Type
genExistVar = ExistVar `liftM` genTypeName

type SubtypeInstantiate = Typechecker

isSubtypeInstantiate
  :: Monad m => Context -> Type -> Type -> SubtypeInstantiate m Context
isSubtypeInstantiate context type1 type2
  | not $ Context.isContextWellFormed context =
    fail . PrettyString.toString $
      Pretty.devContextIsSubtypeInstantiate (show context) (show type1) (show type2)
-- InstLSolve
isSubtypeInstantiate context type1@ExistVar {} type2
  | Type.isMonotype type2 &&
    Context.containsTypeUnassigned context type1 &&
    Context.isTypeWellFormed (context `Context.sliceAtType` type1) type2 =
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
  fail . PrettyString.toString $
    Pretty.devIncompleteIsSubtypeInstantiate (show context) (show type1) (show type2)

type Subtype = Typechecker

isSubtype :: Monad m => Context -> Type -> Type -> Subtype m Context
isSubtype context type1 type2
  | not $ Context.isContextWellFormed context =
    fail . PrettyString.toString $
      Pretty.devContextIsSubtype (show context) (show type1) (show type2)
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
     context'' <- isSubtype context' type1' type2
     return $ Context.sliceAtMarker context'' existVar
-- <:ForallR
isSubtype context type1 (Forall name type2) =
  do let context' = context `Context.appendType` TypeVar name
     context'' <- isSubtype context' type1 type2
     return $ Context.sliceAtType context'' (TypeVar name)
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
  fail . PrettyString.toString $
    Pretty.devIncompleteIsSubtype (show context) (show type1) (show type2)

type Check = Typechecker

check :: Monad m => Context -> Expr -> Type -> Check m Context
check context term typ
  | not $ Context.isContextWellFormed context =
    fail . PrettyString.toString $
      Pretty.devContextCheck (show context) (Pretty.docExpr term) (show typ)
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

type SynthesizeApply = Typechecker

synthesizeApply :: Monad m => Context -> Type -> Expr -> SynthesizeApply m (Context, Type)
synthesizeApply context typ term
  | not $ Context.isContextWellFormed context =
    fail . PrettyString.toString $
      Pretty.devContextSynthesizeApply (show context) (show typ) (Pretty.docExpr term)
-- ForallApp
synthesizeApply context (Forall name typ) term =
  do existVar <- genExistVar
     let context' = context `Context.appendType` existVar
         typ' = Type.substituteTypeVar (TypeVar name) existVar typ
     synthesizeApply context' typ' term
-- âApp
synthesizeApply context typ@ExistVar {} term
  | Context.containsTypeUnassigned context typ =
    do argExistVar <- genExistVar
       bodyExistVar <- genExistVar
       let existVarSolution = Arrow argExistVar bodyExistVar
           context' = Context.assignTypeWithExistVars context typ [bodyExistVar, argExistVar, existVarSolution]
       context'' <- check context' term argExistVar
       return (context'', bodyExistVar)
-- ->App
synthesizeApply context (Arrow argType bodyType) term =
  do context' <- check context term argType
     return (context', bodyType)
-- CatchAll
synthesizeApply context typ term = do
  fail . PrettyString.toString $
    Pretty.devIncompleteSynthesizeApply (show context) (show typ) (Pretty.docExpr term)

type Synthesize = Typechecker

synthesize :: Monad m => Context -> Expr -> Synthesize m (Context, Type)
synthesize context term
  | not $ Context.isContextWellFormed context =
    fail . PrettyString.toString $
      Pretty.devContextSynthesize (show context) (Pretty.docExpr term)
-- Var
synthesize context (IdE name)
  | Context.containsTermAssigned context name =
    do let Just typ = Context.lookupTerm context name
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
     synthesizeApply context' (Context.substitute context' funType) arg
-- CondE
synthesize ctx (CondE ms) =
  synthesizeFirstMatch ctx ms
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
synthesize context (FnDecl NrDef fnName fnBody)
  | Name.hasType fnName =
    do let fnBodyType = fromJust $ Name.nameType fnName
       context' <- check context fnBody fnBodyType
       let context'' = Context.appendTermAssign context' (IdE fnName) fnBodyType
       return (context'', fnBodyType)
  | otherwise =
    do (context', fnBodyType) <- synthesize context fnBody
       let context'' = Context.appendTermAssign context' (IdE fnName) fnBodyType
       return (context'', fnBodyType)
-- FnDecl (Def)
synthesize context (FnDecl Def fnName fnBody)
  | Name.hasType fnName =
    do let fnBodyType = fromJust $ Name.nameType fnName
       let context' = Context.appendTermAssign context (IdE fnName) fnBodyType
       context'' <- check context' fnBody fnBodyType
       return (context'', fnBodyType)
  | otherwise =
    do existVar <- genExistVar
       let context' =
             Context.appendTermAssign (context `Context.appendType` existVar) (IdE fnName) existVar
       synthesize context' fnBody
-- Let
synthesize context (LetE fnDecl body) =
  do (context', _) <- synthesize context fnDecl
     synthesize context' body
-- CatchAll
synthesize context term =
  fail . PrettyString.toString $
    Pretty.devIncompleteSynthesize (show context) (Pretty.docExpr term)

-- | Returns a new expr in which all names in binding positions have
-- been replaced by fully substituted names. A binding position is,
-- e.g., function definitions and lambda expressions, as opposed to
-- identifiers (IdE), which are not in binding position and therefore
-- are not substituted. This is simply done because otherwise there
-- would be too many types in the tree, but it can be considered
-- later.
annotateExpr :: Context -> Expr -> Expr
annotateExpr context = annotate
  where
    annotate :: Expr -> Expr
    annotate (AnnotationE expr typ) =
      AnnotationE (annotate expr) typ
    annotate (AppE fn arg) =
      AppE (annotate fn) (annotate arg)
    annotate (CondE matches) =
      CondE $ map (annotate *** annotate) matches
    annotate (FnDecl defkw name expr) =
      FnDecl defkw (Context.resolveTerm context name) (annotate expr)
    annotate expr@IdE {} =
      expr
    annotate (LambdaE name expr) =
      LambdaE (Context.resolveTerm context name) (annotate expr)
    annotate (LetE defn body) =
      LetE (annotate defn) (annotate body)
    annotate expr@LiteralE {} =
      expr

typecheck :: Monad m => Context -> Expr -> Maybe Type -> m (Context, Expr, Type)
typecheck context term checkType =
  do (context', typ) <- runTypecheckerWithRule checkType
     if Context.isContextWellFormed context' then
       return (context', annotateExpr context' term, typ)
     else
       fail . PrettyString.toString $
         Pretty.devContextTypecheck (show context) (Pretty.docExpr term) (show <$> checkType)
  where
    noType = -1

    maxTypeName typ | isUnit typ = noType
    maxTypeName PrimitiveT {} = noType
    maxTypeName (TypeVar (TypeName x)) = x
    maxTypeName (ExistVar (TypeName x)) = x
    maxTypeName (Forall (TypeName x) _) = x
    maxTypeName (Arrow type1 type2) = max (maxTypeName type1) (maxTypeName type2)
    maxTypeName (ListT typ) = maxTypeName typ
    maxTypeName (TupleT types) = foldl max noType $ map maxTypeName types

    maxContextTypeName (ContextType typ) = maxTypeName typ
    maxContextTypeName _ = noType

    initialCounter :: Int
    initialCounter
      | List.null (Context.scope context) = noType
      | otherwise = (1+) . maximum . map (maxContextTypeName . fst) $ Context.scope context

    runTypecheckerWithRule Nothing =
      do (context', typ) <- runTypechecker (TypecheckerState initialCounter) $ synthesize context term
         return (context', Type.rebuildForall $ Context.substitute context' typ)
    runTypecheckerWithRule (Just typ) =
      do context' <- runTypechecker (TypecheckerState initialCounter) $ check context term typ
         return (context', typ)
