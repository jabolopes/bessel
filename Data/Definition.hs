module Data.Definition where

import Data.List as List (partition)
import Data.Map (Map)
import qualified Data.Map as Map (empty)

import Data.Expr (Expr)
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
               , defSym :: Maybe Symbol
               , defSrc :: Either PrettyString Source
               , defExp :: Either PrettyString Expr
               , defRen :: Either PrettyString Expr
               , defVal :: Either String Val
               , defResults :: Map String Result
               , defUses :: [(String, String)] }

initial :: String -> Definition
initial name =
  Definition { defName = name
             , defFreeNames = []
             , defSym = Nothing
             , defSrc = Left PrettyString.empty
             , defExp = Left PrettyString.empty
             , defRen = Left PrettyString.empty
             , defVal = Left "" 
             , defResults = Map.empty
             , defUses = [] }

prefixedUses :: Definition -> [(String, String)]
prefixedUses = snd . List.partition (null . snd) . defUses

unprefixedUses :: Definition -> [String]
unprefixedUses = map fst . fst . List.partition (null . snd) . defUses
