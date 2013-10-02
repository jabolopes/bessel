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
               , val :: Either String Val
               , srcExpr :: Maybe Expr
               , expExpr :: Maybe Expr
               , renExpr :: Either String Expr }
    deriving (Show)


initial :: String -> Definition
initial name =
  Definition { name = name
             , freeNames = []
             , unprefixedUses = []
             , prefixedUses = []
             , symbol = Nothing
             , val = Left "Data.Definition.val: field is not initialized"
             , srcExpr = Nothing
             , expExpr = Nothing
             , renExpr = Left "Data.Definition.renExpr: field is not initialized" }
