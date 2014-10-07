module Monad.ParserM where

import Control.Monad.Except (throwError)
import Control.Monad.State
import Data.Functor ((<$>))
import Data.LexState
import Data.Source (Source)
import qualified Data.Source as Source

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

ensureExpr :: Source -> ParserM Source
ensureExpr val =
  case Source.toSource val of
    Nothing -> failM "expecting expression instead of pattern"
    Just expr -> return expr
