{-# LANGUAGE DeriveDataTypeable #-}
module Data.Exception where

import Prelude hiding (catch)

import Control.Exception (Exception, throw, catch)
import Data.Typeable


data FlException
    = RenamerException String
    | InterpreterException String
    | ParseException String
    | SignalException String
    | TypecheckerException String
      deriving (Show, Typeable)

instance Exception FlException


throwRenamerException :: String -> a
throwRenamerException str = throw $ RenamerException str


throwInterpreterException :: String -> a
throwInterpreterException str = throw $ InterpreterException str


throwParseException :: String -> a
throwParseException str = throw $ ParseException str


throwSignalException :: String -> a
throwSignalException str = throw $ SignalException str


throwTypecheckerException :: String -> a
throwTypecheckerException str = throw $ TypecheckerException str


catchFlException :: IO a -> (FlException -> IO a) -> IO a
catchFlException m fn = catch m fn