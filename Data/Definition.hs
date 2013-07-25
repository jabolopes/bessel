module Data.Definition where

import Data.Stx (Stx)
import Data.Symbol (Symbol)
import Data.Type (Type)
import Monad.InterpreterM (Expr)


data Definition
     = Definition { name :: String
                  , freeNames :: [String]
                  , unprefixedUses :: [String]
                  , prefixedUses :: [(String, String)]
                  , symbol :: Maybe Symbol
                  , typ :: Maybe Type
                  , expr :: Maybe Expr
                  , srcStx :: Maybe (Stx String)
                  , expStx :: Maybe (Stx String)
                  , renStx :: Maybe (Stx String)
                  , lnkStx :: Maybe (Stx String) }
     deriving (Show)


initial :: String -> Definition
initial name =
    Definition { name = name
               , freeNames = []
               , unprefixedUses = []
               , prefixedUses = []
               , symbol = Nothing
               , typ = Nothing
               , expr = Nothing
               , srcStx = Nothing
               , expStx = Nothing
               , renStx = Nothing
               , lnkStx = Nothing }