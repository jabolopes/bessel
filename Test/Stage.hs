{-# LANGUAGE FlexibleContexts #-}
module Test.Stage where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (runStateT)

import qualified Compiler.Linearizer as Linearizer
import Data.Expr (Expr)
import qualified Data.Name as Name
import Data.PrettyString (PrettyString)
import qualified Data.PrettyString as PrettyString
import Data.Source as Source (Source(..))
import Monad.NameT (MonadName)
import qualified Parser
import qualified Stage.Expander as Expander
import Stage.Renamer (RenamerState)
import qualified Stage.Renamer as Renamer
import Typechecker.Context (Context)
import qualified Typechecker.Typechecker as Typechecker

parseFile :: (MonadError PrettyString m, MonadIO m, MonadName m) => String -> m Source
parseFile filename =
  do input <- liftIO $ readFile filename
     case Parser.parseFile (Name.untyped filename) input of
       Left err -> throwError $ PrettyString.text err
       Right src -> return src

expandFile :: (MonadError PrettyString m, MonadIO m, MonadName m) => String -> m [Expr]
expandFile filename =
  do ModuleS _ _ srcs <- parseFile filename
     concat <$> mapM Expander.expand srcs

renameFile :: (MonadError PrettyString m, MonadIO m, MonadName m) => String -> RenamerState -> m [Expr]
renameFile filename initialState =
  do exprs <- expandFile filename
     renameExprs initialState exprs
  where
    renameExprs :: MonadError PrettyString m => RenamerState -> [Expr] -> m [Expr]
    renameExprs _ [] = return []
    renameExprs state (expr:exprs) =
      case runStateT (Renamer.renameM expr) state of
        Left err -> throwError err
        Right (exprs', state') -> (exprs' ++) <$> renameExprs state' exprs

typecheckFile :: (MonadError PrettyString m, MonadIO m, MonadName m) => String -> RenamerState -> Context -> m [Expr]
typecheckFile filename initialState initialContext =
  do exprs <- renameFile filename initialState
     typecheckExprs initialContext exprs
  where
    typecheckExprs _ [] = return []
    typecheckExprs context (expr:exprs) =
      case Typechecker.typecheck context expr Nothing of
        Left err -> throwError err
        Right (context', expr', _) -> (expr':) <$> typecheckExprs context' exprs

-- TODO: It should probably go through the typechecker.
linearizeFile :: (MonadError PrettyString m, MonadIO m, MonadName m) => String -> RenamerState -> m [Expr]
linearizeFile filename initialState =
  do exprs <- renameFile filename initialState
     concat <$> mapM Linearizer.linearize exprs
