{-# LANGUAGE NamedFieldPuns #-}
module Monad.ParserM where

import Control.Monad.Error (throwError)
import Control.Monad.State
import Data.Functor ((<$>))

import Data.LexState
import Data.Token


data ParserState
    = ParserState { lexerState :: LexState
                  , dependencies :: [String] }

type ParserM a = StateT ParserState (Either String) a


initial :: LexState -> ParserState
initial lexerState =
    ParserState { lexerState = lexerState
                , dependencies = [] }


addDependencyM :: String -> ParserM ()
addDependencyM dep =
    modify $ \s -> s { dependencies = dependencies s ++ [dep] }


failM :: String -> ParserM a
failM err =
    do filename <- filename . lexerState <$> get
       n <- beginLine . lexerState <$> get
       throwError $ filename ++ ": line " ++ show n ++ ": " ++ err