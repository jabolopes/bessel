module Expander.Cast where

import Data.Name (Name)
import qualified Data.Name as Name
import Data.Source (Source(..))
import qualified Data.Source as Source
import Monad.NameM (MonadName)
import qualified Monad.NameM as NameM

castCond :: MonadName m => Name -> Source -> (Name -> m Source) -> m Source
castCond _ src@(PatS binder _) genBody =
  do body <- genBody binder
     return $ CondS [([src], body)]
castCond name src genBody =
  do argName <- NameM.genName name
     castCond name (Source.bindPat argName src) genBody

castArgument :: MonadName m => Source -> (Name -> m Source) -> m Source
castArgument src genBody = castCond (Name.untyped "arg") src genBody

castResult :: MonadName m => Source -> m Source
castResult src = castCond (Name.untyped "r") src (return . IdS)

castFunction :: MonadName m => Source -> Source -> (Name -> m Source) -> m Source
castFunction argSrc resultSrc genBody =
  castArgument argSrc $ \argName ->
    do condResult <- castResult resultSrc
       body <- genBody argName
       return $ condResult `AppS` body
