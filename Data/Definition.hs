module Data.Definition where

import Data.IORef
import Data.List as List (partition)

import Data.Expr (Expr)
import Data.PrettyString (PrettyString)
import qualified Data.PrettyString as PrettyString (empty)
import Data.Source (Source)
import Data.Symbol (Symbol)
import Monad.InterpreterM (Val)

data Definition
  = Definition { defName :: String
               , defFreeNames :: [String]
               , defSym :: Maybe Symbol
               , defSrc :: Either PrettyString Source
               , defExp :: Either PrettyString Expr
               , defRen :: Either PrettyString Expr
               , defVal :: Either String (IORef Val)
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
             , defUses = []
             }

prefixedUses :: Definition -> [(String, String)]
prefixedUses = snd . List.partition (null . snd) . defUses

unprefixedUses :: Definition -> [String]
unprefixedUses = map fst . fst . List.partition (null . snd) . defUses
