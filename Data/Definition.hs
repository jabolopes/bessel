module Data.Definition where

import Data.Expr (Expr)
import Data.Map (Map)
import qualified Data.Map as Map (empty)
import Data.PrettyString (PrettyString)
import qualified Data.PrettyString as PrettyString (empty)
import Data.Source (Source)
import Data.Symbol (Symbol)
import Monad.InterpreterM (Val)

data Result
  = ErrorResult PrettyString
  | SourceResult Source
  | ExprResult Expr
  | ValResult Val

data Definition
  = Definition { defName :: String
               , defFreeNames :: [String]
               , defUnprefixedUses :: [String]
               , defPrefixedUses :: [(String, String)]
               , defSym :: Maybe Symbol
               , defSrc :: Either PrettyString Source
               , defExp :: Either PrettyString Expr
               , defRen :: Either PrettyString Expr
               , defVal :: Either String Val
               , defResults :: Map String Result }

initial :: String -> Definition
initial name =
  Definition { defName = name
             , defFreeNames = []
             , defUnprefixedUses = []
             , defPrefixedUses = []
             , defSym = Nothing
             , defSrc = Left PrettyString.empty
             , defExp = Left PrettyString.empty
             , defRen = Left PrettyString.empty
             , defVal = Left "" 
             , defResults = Map.empty }
