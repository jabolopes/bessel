{-# LANGUAGE DeriveDataTypeable #-}
module Data.Exception where

import Prelude hiding (catch)

import Control.Exception (Exception, throw, catch)
import Data.Typeable


data UserException
    = LoaderException String
    | RenamerException String
    | InterpreterException String
    | LexerException String
    | ParserException String
    | SignalException String
    | TypecheckerException String
      deriving (Show, Typeable)

instance Exception UserException


throwLoaderException :: String -> a
throwLoaderException = throw . LoaderException


throwRenamerException :: String -> a
throwRenamerException = throw . RenamerException


throwInterpreterException :: String -> a
throwInterpreterException = throw . InterpreterException


throwLexerException :: String -> a
throwLexerException = throw . LexerException


throwParserException :: String -> a
throwParserException = throw . ParserException


throwSignalException :: String -> a
throwSignalException = throw . SignalException


throwTypecheckerException :: String -> a
throwTypecheckerException = throw . TypecheckerException


catchUserException :: IO a -> (UserException -> IO a) -> IO a
catchUserException m fn = catch m fn