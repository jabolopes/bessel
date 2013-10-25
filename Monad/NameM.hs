module Monad.NameM where

import Control.Applicative ((<$>))
import Control.Monad.State

data NameState = NameState { nameCounter :: Int }

initialNameState :: NameState
initialNameState = NameState { nameCounter = 0 }

type NameM a b = StateT NameState a b

genNameM :: (Monad a, Functor a) => String -> NameM a String
genNameM name =
  do c <- nameCounter <$> get
     modify $ \s -> s { nameCounter = nameCounter s + 1 }
     return (name ++ "#" ++ show c)

runNameM :: NameM a b -> NameState -> a (b, NameState)
runNameM m s = runStateT m s
