module Data.Definition where

import Data.Expr (Expr)
import Data.PrettyString (PrettyString)
import qualified Data.PrettyString as PrettyString (empty)
import Data.Source (Source)
import Data.Symbol (Symbol)
import Monad.InterpreterM (Val)

data Definition
  = Definition { defName :: String
               , defFreeNames :: [String]
               , defUnprefixedUses :: [String]
               , defPrefixedUses :: [(String, String)]
               , defSym :: Maybe Symbol
               , defVal :: Either String Val
               , defSrc :: Either PrettyString Source
               , defExp :: Either PrettyString Expr
               , defRen :: Either PrettyString Expr }

initial :: String -> Definition
initial name =
  Definition { defName = name
             , defFreeNames = []
             , defUnprefixedUses = []
             , defPrefixedUses = []
             , defSym = Nothing
             , defVal = Left ""
             , defSrc = Left PrettyString.empty
             , defExp = Left PrettyString.empty
             , defRen = Left PrettyString.empty }
