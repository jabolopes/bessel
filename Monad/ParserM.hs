module Monad.ParserM where

import Control.Monad.Except (throwError)
import Control.Monad.State
import Data.Functor ((<$>))
import Data.LexState
import qualified Data.PrettyString as PrettyString
import Data.Source (Source)
import qualified Data.Source as Source
import qualified Pretty.Data.Source as Pretty

data ParserState
    = ParserState { psLexerState :: LexState }

type ParserM a = StateT ParserState (Either String) a

initial :: LexState -> ParserState
initial lexerState = ParserState { psLexerState = lexerState }

failM :: String -> ParserM a
failM err =
  do f <- lexFilename . psLexerState <$> get
     n <- lexBeginLine . psLexerState <$> get
     throwError $ f ++ ": line " ++ show n ++ ": " ++ err

ensureExpr :: Source -> ParserM Source
ensureExpr val =
  case Source.toSource val of
    Nothing -> failM $ "expecting expression instead of pattern: " ++ PrettyString.toString (Pretty.docSource val)
    Just expr -> return expr
