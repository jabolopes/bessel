{-# LANGUAGE FlexibleInstances,
             MultiParamTypeClasses,
             UndecidableInstances #-}
module Data.Result where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State

import Data.PrettyString (PrettyString)
import qualified Data.PrettyString as PrettyString

data Result a
  = Ok a
  | Bad PrettyString

result :: (PrettyString -> b) -> (a -> b) -> Result a -> b
result fn _ (Bad err) = fn err
result _ fn (Ok x) = fn x

instance Functor Result where
  fmap = liftM

instance Applicative Result where
  pure = return
  (<*>) = ap

instance Monad Result where
  return = Ok
  Ok x >>= k = k x
  Bad err >>= _ = Bad err
  fail = Bad . PrettyString.text

instance Alternative Result where
  empty = mzero
  (<|>) = mplus

instance MonadError PrettyString Result where
  throwError = Bad
  {-# INLINE throwError #-}
  catchError x fn = result fn (const x) x
  {-# INLINE catchError #-}

instance MonadPlus Result where
  mzero = Bad $ PrettyString.text "mzero"
  x@(Ok _) `mplus` _ = x
  Bad _ `mplus` x = x

data ResultT m a = ResultT { runResultT :: m (Result a) }

resultT :: Monad m => (PrettyString -> b) -> (a -> b) -> ResultT m a -> m b
resultT l r m = return . result l r =<< runResultT m

instance Monad m => Functor (ResultT m) where
  fmap = liftM

instance Monad m => Applicative (ResultT m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (ResultT m) where
  return = ResultT . return . Ok

  x >>= k =
    ResultT $ do
      val <- runResultT x
      case val of
        Bad err -> return $ Bad err
        Ok y -> runResultT (k y)

  fail = ResultT . return . Bad . PrettyString.text

instance Monad m => Alternative (ResultT m) where
  empty = mzero
  (<|>) = mplus

instance Monad m => MonadError PrettyString (ResultT m) where
  throwError = ResultT . return . Bad
  {-# INLINE throwError #-}
  catchError x fn =
    ResultT $ result (runResultT . fn) (return . Ok) =<< runResultT x
  {-# INLINE catchError #-}

instance MonadIO m => MonadIO (ResultT m) where
  liftIO = lift . liftIO

instance Monad m => MonadPlus (ResultT m) where
  mzero = ResultT $ return mzero
  x `mplus` y =
    ResultT $ do
      val <- runResultT x
      case val of
        Ok _ -> return val
        Bad _ -> runResultT y

instance MonadTrans ResultT where
  lift = ResultT . liftM Ok

instance MonadState s m => MonadState s (ResultT m) where
  get = lift get
  put = lift . put
