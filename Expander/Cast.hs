module Expander.Cast where

import Data.Name (Name)
import qualified Data.Name as Name
import Data.Source (Source(..))
import qualified Data.Source as Source
import Monad.NameM (NameM)
import qualified Monad.NameM as NameM

castCond :: Monad m => Name -> Source -> (Name -> NameM m Source) -> NameM m Source
castCond _ src@(PatS binder _) genBody =
  do body <- genBody binder
     return $ CondS [([src], body)]
castCond name src genBody =
  do argName <- NameM.genNameM name
     castCond name (Source.bindPat argName src) genBody

castArgument :: Monad m => Source -> (Name -> NameM m Source) -> NameM m Source
castArgument src genBody = castCond (Name.untyped "arg") src genBody

castResult :: Monad m => Source -> NameM m Source
castResult src = castCond (Name.untyped "r") src (return . IdS)

castFunction :: Monad m => Source -> Source -> (Name -> NameM m Source) -> NameM m Source
castFunction argSrc resultSrc genBody =
  castArgument argSrc $ \argName ->
    do condResult <- castResult resultSrc
       body <- genBody argName
       return $ condResult `AppS` body
