module Data.Definition where

import Data.Expr (Expr)
import Data.Macro (Macro)
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
               , defMac :: Either PrettyString Macro
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
             , defMac = Left PrettyString.empty
             , defExp = Left PrettyString.empty
             , defRen = Left PrettyString.empty }
