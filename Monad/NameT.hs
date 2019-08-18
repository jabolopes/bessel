{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Monad.NameT where

import Control.Applicative ((<$>))
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, StateT, get, modify, runStateT)
import Control.Monad.Writer (WriterT)
import Control.Monad.Trans (MonadTrans, lift)
import qualified Data.List as List

import Data.Name (Name)
import qualified Data.Name as Name

-- | Monads that generate unique names.
class Monad m => MonadName m where
  genName :: Name -> m Name

data NameState = NameState { nameCounter :: Int }

-- | Implementation of a Monad that generates unique names.
newtype NameT m a = NameT { unNameT :: StateT NameState m a }
  deriving (Applicative, Functor, Monad, MonadState NameState, MonadTrans)

instance (MonadIO m) => MonadIO (NameT m) where
  liftIO = lift . liftIO

instance Monad m => MonadName (NameT m) where
  genName name =
    do c <- nameCounter <$> get
       modify $ \s -> s { nameCounter = nameCounter s + 1 }
       let prefix = List.takeWhile (/= '#') $ Name.nameStr name
       Name.rename name (prefix ++ "#" ++ show c)

instance MonadName m => MonadName (ExceptT e m) where
  genName = lift . genName

instance (Monoid w, MonadName m) => MonadName (WriterT w m) where
  genName = lift . genName

runNameT :: Functor m => NameT m a -> m a
runNameT m = fst <$> runStateT (unNameT m) initialNameState
  where
    initialNameState :: NameState
    initialNameState = NameState { nameCounter = 0 }
