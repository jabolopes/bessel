{-# LANGUAGE DeriveDataTypeable #-}
module Data.Exception where

import Control.Exception (Exception, throw, catch)
import Data.Typeable

import Data.PrettyString (PrettyString)

data UserException
    = LoaderException PrettyString
    | InterpreterException PrettyString
    | LexerException String
    | SignalException String
      deriving (Show, Typeable)

instance Exception UserException

throwLoaderException :: PrettyString -> a
throwLoaderException = throw . LoaderException

throwInterpreterException :: PrettyString -> a
throwInterpreterException = throw . InterpreterException

-- | 'throwLexerException line column str' throws a lexical exception where
-- @line@ is the line number, @column@ the column number, and 'str' is the
-- 'String' containing the source program.
throwLexerException :: Int -> Int -> String -> a
throwLexerException line column str =
  throw . LexerException $
    "line " ++ show line ++ ", column " ++ show column ++ ": " ++ "\n\n\t " ++ head (lines str)

throwSignalException :: String -> a
throwSignalException = throw . SignalException

catchUserException :: IO a -> (UserException -> IO a) -> IO a
catchUserException = catch
