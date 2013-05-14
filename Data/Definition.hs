module Data.Definition where

import Data.Stx (Stx)
import Data.Symbol (Symbol)
import Data.Type (Type)
import Monad.InterpreterM (Expr)


data Definition
     = Definition { mdl :: Int
                  , name :: String
                  , symbol :: Maybe Symbol
                  , typ :: Maybe Type
                  , expr :: Maybe Expr
                  , srcStx :: Maybe (Stx String)
                  , renStx :: Maybe (Stx String)
                  , lnkStx :: Maybe (Stx String) }