module Data.Definition where

import Data.IORef
import Data.List as List (partition)

import Data.Expr (Expr)
import Data.PrettyString (PrettyString)
import qualified Data.PrettyString as PrettyString (empty)
import Data.QualName (QualName)
import Data.Source (Source)
import Data.Symbol (Symbol)
import Monad.InterpreterM (Val)

data Definition
  = Definition { defModule :: QualName
               , defName :: QualName
                 -- Each free names is the module and definition names.
               , defFreeNames :: [(QualName, QualName)]
               , defSym :: Maybe Symbol
               , defSrc :: Either PrettyString Source
               , defExp :: Either PrettyString Expr
               , defRen :: Either PrettyString Expr
               , defVal :: Either String (IORef Val)
               , defUses :: [(String, String)] }

initial :: QualName -> QualName -> Definition
initial moduleName name =
  Definition { defModule = moduleName
             , defName = name
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
