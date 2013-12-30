module Monad.Utils where

import Control.Applicative ((<$>))

returnOne :: Functor m => m a -> m [a]
returnOne m = (:[]) <$> m
