{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Monad.NameM where

-- TODO: Renane NameM to NameT because this is a monad transformer.

import Control.Applicative ((<$>))
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.List as List

import Data.Name (Name)
import qualified Data.Name as Name

data NameState = NameState { nameCounter :: Int }

initialNameState :: NameState
initialNameState = NameState { nameCounter = 0 }

type NameM m = StateT NameState m

genNameM :: Monad m => Name -> NameM m Name
genNameM name =
  do c <- nameCounter <$> get
     modify $ \s -> s { nameCounter = nameCounter s + 1 }
     lift $ Name.rename name (Name.nameStr name ++ "#" ++ show c)

instance Monad m => MonadName (StateT NameState m) where
  genName = genNameM

runNameM :: NameM m a -> NameState -> m (a, NameState)
runNameM m s = runStateT m s

class Monad m => MonadName m where
  genName :: Name -> m Name

newtype NameT m a = NameT { runNameT :: StateT NameState m a }
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

runName :: Functor m => NameT m a -> m a
runName m = fst <$> runStateT (runNameT m) initialNameState
