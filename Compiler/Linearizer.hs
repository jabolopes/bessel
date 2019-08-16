{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Compiler.Linearizer where

import Prelude hiding (traverse)

import Control.Arrow ((***))
import Control.Monad.Writer
import qualified Data.List as List

import Data.Expr (DefnKw(..), Expr(..))
import qualified Data.Expr as Expr
import qualified Data.Name as Name
import Monad.NameM (MonadName(..))

data Position = Binding | NonBinding

deanonymizeExpr :: MonadName m => Position -> Expr -> m Expr
deanonymizeExpr _ (AnnotationE expr typ) =
  flip AnnotationE typ <$> deanonymizeExpr NonBinding expr
deanonymizeExpr _ (AppE fn arg) =
  AppE <$> deanonymizeExpr NonBinding fn <*> deanonymizeExpr NonBinding arg
deanonymizeExpr _ (CondE matches) =
  CondE <$> mapM deanonym matches
  where
    deanonym (predicate, body) =
      do predicate' <- deanonymizeExpr NonBinding predicate
         body' <- deanonymizeExpr NonBinding body
         return (predicate', body')
deanonymizeExpr _ (FnDecl kw name body) =
  FnDecl kw name <$> deanonymizeExpr Binding body
deanonymizeExpr _ expr@IdE {} =
  return expr
deanonymizeExpr bind@Binding (LambdaE arg body) =
  LambdaE arg <$> deanonymizeExpr bind body
deanonymizeExpr NonBinding (LambdaE arg body) =
  do expr' <- LambdaE arg <$> deanonymizeExpr Binding body
     fnName <- genName $ Name.untyped "lambda"
     return $ LetE (FnDecl NrDef fnName expr') (IdE fnName)
deanonymizeExpr _ (LetE defn body) =
  LetE <$> deanonymizeExpr NonBinding defn <*> deanonymizeExpr NonBinding body
deanonymizeExpr _ expr@LiteralE {} =
  return expr

deanonymize :: MonadName m => Expr -> m Expr
deanonymize = deanonymizeExpr NonBinding

substitute :: Expr -> Expr -> Expr -> Expr
substitute source target = sub
  where
    traverse (AnnotationE expr typ) =
      AnnotationE (sub expr) typ
    traverse (AppE expr1 expr2) =
      AppE (sub expr1) (sub expr2)
    traverse (CondE matches) =
      CondE $ map (sub *** sub) matches
    traverse (FnDecl kw name expr) =
      FnDecl kw name $ sub expr
    traverse expr@IdE {} =
      expr
    traverse (LambdaE arg body) =
      LambdaE arg $ sub body
    traverse (LetE defn body) =
      LetE (sub defn) (sub body)
    traverse expr@LiteralE {} =
      expr

    sub expr
      | expr == source = target
      | otherwise = traverse expr

uncapture :: MonadName m => Expr -> Expr -> m (Expr, Expr)
uncapture enclosing@(LambdaE arg body) fnDecl@(FnDecl fnKw fnName fnBody) =
  if arg `List.elem` Expr.freeVars fnDecl then
     do captureName <- genName arg
        let body' = substitute (IdE fnName) (AppE (IdE fnName) (IdE arg)) body
        let enclosing' = LambdaE arg body'
        let fnBody' =
              substitute (IdE fnName) (AppE (IdE fnName) (IdE captureName)) $
                substitute (IdE arg) (IdE captureName) $ fnBody
        let fnDecl' = FnDecl fnKw fnName (LambdaE captureName fnBody')
        return (enclosing', fnDecl')
   else
     return (enclosing, fnDecl)
uncapture enclosing nested =
  return (enclosing, nested)

data Scope = Toplevel | Local

unnestExpr :: (MonadName m, MonadWriter [Expr] m) => Scope -> Expr -> m Expr
unnestExpr scope (AnnotationE expr typ) =
  flip AnnotationE typ <$> unnestExpr scope expr
unnestExpr scope (AppE fn arg) =
  AppE <$> unnestExpr Local fn <*> unnestExpr scope arg
unnestExpr scope (CondE matches) =
  CondE <$> mapM unnestMatch matches
  where
    unnestMatch (predicate, body) =
      do predicate' <- unnestExpr scope predicate
         body' <- unnestExpr scope body
         return (predicate', body')
unnestExpr _ (FnDecl _ name body) =
  do body' <- unnestExpr Local body
     let kw | name `List.elem` Expr.freeVars body' = Def
            | otherwise = NrDef
     return $ FnDecl kw name body'
unnestExpr _ expr@IdE {} =
  return expr
unnestExpr scope (LambdaE arg body) =
  do (body', fnDecls) <- runWriterT $ unnestExpr scope body
     let expr' = LambdaE arg body'
     foldM (\enclosing nested -> do
               (enclosing', nested') <- uncapture enclosing nested
               tell [nested']
               return enclosing') expr' fnDecls
unnestExpr scope@Local (LetE defn@(FnDecl _ _ LambdaE {}) body) =
  do tell [defn]
     unnestExpr scope body
unnestExpr scope (LetE defn body) =
  LetE <$> unnestExpr Local defn <*> unnestExpr scope body
unnestExpr _ expr@LiteralE {} =
  return expr

unnest :: MonadName m => Expr -> m [Expr]
unnest expr =
  do (expr', exprs) <- runWriterT $ unnestExpr Toplevel expr
     exprs' <- concat <$> mapM unnest exprs
     return $ exprs' ++ [expr']

linearize :: MonadName m => Expr -> m [Expr]
linearize expr = (unnest =<< deanonymize expr)
