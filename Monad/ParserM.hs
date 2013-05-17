{-# LANGUAGE NamedFieldPuns #-}
module Monad.ParserM where

import Data.LexState
import Data.Token


type ParserM a = LexState -> Either String a


parseError :: Token -> ParserM a
parseError tk = failM (show tk)


thenM :: ParserM a -> (a -> ParserM b) -> ParserM b
m `thenM` k = \s ->
   case m s of 
       Right a -> k a s
       Left e -> Left e


returnM :: a -> ParserM a
returnM a = \s -> Right a


failM :: String -> ParserM a
failM err LexState { filename, beginLine = n } =
  Left $ filename ++ ": line " ++ show n ++ ": " ++ err


catchM :: ParserM a -> (String -> ParserM a) -> ParserM a
catchM m k = \s ->
   case m s of
      Right a -> Right a
      Left e -> k e s