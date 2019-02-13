module Expander.Pattern where

import Data.Name (Name)
import qualified Data.Name as Name
import Data.Source (Source(..))
import Monad.NameM (NameM)
import qualified Monad.NameM as NameM

-- | Generates names for the given patterns.
-- @
-- genPatName x       = "x#0"
-- genPatName x@isInt = "x#0"
-- genPatName  @      = "arg#0"
-- genPatName  @isInt = "arg#0"
-- @
genPatNames :: Monad m => [Source] -> NameM m [Name]
genPatNames = mapM genPatName
  where
    genName name
      | Name.isEmptyName name = NameM.genNameM $ Name.untyped "arg"
      | otherwise = NameM.genNameM name

    genPatName (PatS binder _) = genName binder
    genPatName _ = genName Name.empty
