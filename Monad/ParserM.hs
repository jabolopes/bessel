module Monad.ParserM where

import Control.Monad.Error (throwError)
import Control.Monad.State
import Data.Functor ((<$>))

import Data.LexState

data ParserState
    = ParserState { psLexerState :: LexState }

type ParserM a = StateT ParserState (Either String) a

initial :: LexState -> ParserState
initial lexerState = ParserState { psLexerState = lexerState }

failM :: String -> ParserM a
failM err =
  do f <- filename . psLexerState <$> get
     n <- beginLine . psLexerState <$> get
     throwError $ f ++ ": line " ++ show n ++ ": " ++ err
