module Data.Definition where

import Data.Expr (Expr)
import Data.Symbol (Symbol)
import Monad.InterpreterM (Val)


data Definition
  = Definition { name :: String
               , freeNames :: [String]
               , unprefixedUses :: [String]
               , prefixedUses :: [(String, String)]
               , symbol :: Maybe Symbol
               , val :: Maybe Val
               , srcExpr :: Maybe Expr
               , expExpr :: Maybe Expr
               , renExpr :: Maybe Expr }
    deriving (Show)


initial :: String -> Definition
initial name =
  Definition { name = name
             , freeNames = []
             , unprefixedUses = []
             , prefixedUses = []
             , symbol = Nothing
             , val = Nothing
             , srcExpr = Nothing
             , expExpr = Nothing
             , renExpr = Nothing }
