module Monad.ParserM where

import Control.Monad.Except (throwError)
import Control.Monad.State
import Data.Functor ((<$>))
import qualified Data.PrettyString as PrettyString
import Data.Source (Source)
import qualified Data.Source as Source
import Data.Token (Srcloc(..), Token)
import qualified Data.Token as Token
import qualified Pretty.Data.Source as Pretty

data ParserState
    = ParserState { psFilename :: String
                  , psTokens :: [Token] }

type ParserM a = StateT ParserState (Either String) a

initial :: String -> [Token] -> ParserState
initial filename tokens =
  ParserState { psFilename = filename, psTokens = tokens}

failM :: Token -> ParserM a
failM token =
  do let Srcloc line column = Token.tokenSrcloc token
     f <- psFilename <$> get
     throwError $ f ++ ": line " ++ show line ++ ", column " ++ show column ++ ": " ++ show token

ensureExpr :: Source -> ParserM Source
ensureExpr val =
  case Source.toSource val of
    Nothing -> throwError $ "expecting expression instead of pattern: " ++ PrettyString.toString (Pretty.docSource val)
    Just expr -> return expr
