module Data.Definition where

import Data.IORef
import Data.List as List (partition)

import Data.Expr (Expr)
import Data.PrettyString (PrettyString)
import qualified Data.PrettyString as PrettyString (empty)
import Data.QualName (QualName)
import Data.Source (Source)
import Monad.InterpreterM (Val)

data Definition
  = Definition { defName :: QualName
               , defFreeNames :: [QualName]
               , defSrc :: Either PrettyString Source
               , defExp :: Either PrettyString Expr
               , defRen :: Either PrettyString Expr
               , defVal :: Either String (IORef Val)
               , defUses :: [(String, String)] }

initial :: QualName -> Definition
initial name =
  Definition { defName = name
             , defFreeNames = []
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
