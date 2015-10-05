module Data.Definition where

import Data.IORef
import Data.List as List (partition)

import Data.Expr (Expr)
import Data.PrettyString (PrettyString)
import qualified Data.PrettyString as PrettyString (empty)
import Data.Name (Name)
import qualified Data.Name as Name
import Data.Source (Source)
import Monad.InterpreterM (Val)

data Definition
  = Definition { defName :: Name
               , defFreeNames :: [Name]
               , defSrc :: Either PrettyString Source
               , defExp :: Either PrettyString Expr
               , defRen :: Either PrettyString Expr
               , defTyp :: Either PrettyString Expr
               , defVal :: Either String (IORef Val)
               , defUses :: [(Name, Name)] }

initial :: Name -> Definition
initial name =
  Definition { defName = name
             , defFreeNames = []
             , defSrc = Left PrettyString.empty
             , defExp = Left PrettyString.empty
             , defRen = Left PrettyString.empty
             , defTyp = Left PrettyString.empty
             , defVal = Left ""
             , defUses = []
             }

prefixedUses :: Definition -> [(Name, Name)]
prefixedUses = snd . List.partition (Name.isEmptyName . snd) . defUses

unprefixedUses :: Definition -> [Name]
unprefixedUses =
  map fst . fst . List.partition (Name.isEmptyName . snd) . defUses
