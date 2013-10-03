module Data.Definition where

import Data.Expr (Expr)
import Data.Symbol (Symbol)
import Doc.Doc (Doc)
import qualified Doc.Doc as Doc (empty)
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
               , renExpr :: Either Doc Expr }
    deriving (Show)


initial :: String -> Definition
initial name =
  Definition { name = name
             , freeNames = []
             , unprefixedUses = []
             , prefixedUses = []
             , symbol = Nothing
             , val = Left ""
             , srcExpr = Nothing
             , expExpr = Nothing
             , renExpr = Left Doc.empty }
