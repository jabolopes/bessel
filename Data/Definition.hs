module Data.Definition where

import Data.Expr (Expr)
import Data.PrettyString (PrettyString)
import qualified Data.PrettyString as PrettyString (empty)
import Data.Symbol (Symbol)
import Monad.InterpreterM (Val)


data Definition
  = Definition { defName :: String
               , defFreeNames :: [String]
               , defUnprefixedUses :: [String]
               , defPrefixedUses :: [(String, String)]
               , defSym :: Maybe Symbol
               , defVal :: Either String Val
               , defSrc :: Maybe Expr
               , defExp :: Maybe Expr
               , defRen :: Either PrettyString Expr }
    deriving (Show)


initial :: String -> Definition
initial name =
  Definition { defName = name
             , defFreeNames = []
             , defUnprefixedUses = []
             , defPrefixedUses = []
             , defSym = Nothing
             , defVal = Left ""
             , defSrc = Nothing
             , defExp = Nothing
             , defRen = Left PrettyString.empty }
