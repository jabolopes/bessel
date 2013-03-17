{-# LANGUAGE DeriveDataTypeable #-}
module Data.Exception where

import Prelude hiding (catch)

import Control.Exception (Exception, throw, catch)
import Data.Typeable


data FlException
    = RenamerException String
    | InterpreterException String
    | LexException String
    | ParseException String
    | SignalException String
    | TypecheckerException String
      deriving (Show, Typeable)

instance Exception FlException


throwRenamerException :: String -> a
throwRenamerException = throw . RenamerException


throwInterpreterException :: String -> a
throwInterpreterException = throw . InterpreterException


throwLexException :: String -> a
throwLexException = throw . LexException


throwParseException :: String -> a
throwParseException = throw . ParseException


throwSignalException :: String -> a
throwSignalException = throw . SignalException


throwTypecheckerException :: String -> a
throwTypecheckerException = throw . TypecheckerException


catchFlException :: IO a -> (FlException -> IO a) -> IO a
catchFlException m fn = catch m fn