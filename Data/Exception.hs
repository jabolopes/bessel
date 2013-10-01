{-# LANGUAGE DeriveDataTypeable #-}
module Data.Exception where

import Prelude hiding (catch)

import Control.Exception (Exception, throw, catch)
import Data.Typeable


data UserException
    = LoaderException String
    | ExpanderException String
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


throwExpanderException :: String -> a
throwExpanderException = throw . ExpanderException


throwRenamerException :: String -> a
throwRenamerException = throw . RenamerException


throwInterpreterException :: String -> a
throwInterpreterException = throw . InterpreterException


-- | 'throwLexerException line str' throws a lexical exception where
-- @line@ is the line number and 'str' is the 'String' containing the
-- source program.
throwLexerException :: Int -> String -> a
throwLexerException n str =
  throw . LexerException $ "line " ++ show n ++ ": " ++
                           "\n\n\t " ++ (head . lines $ str)


throwParserException :: String -> a
throwParserException = throw . ParserException


throwSignalException :: String -> a
throwSignalException = throw . SignalException


throwTypecheckerException :: String -> a
throwTypecheckerException = throw . TypecheckerException


catchUserException :: IO a -> (UserException -> IO a) -> IO a
catchUserException = catch