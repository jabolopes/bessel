module Data.SrcFile where

import Data.Map (Map)

import Data.Frame
import Data.Stx
import Data.Symbol
import Data.Type
import Monad.InterpreterM


data SrcFile
    = SrcFile { name :: String
              , deps :: [String]
              , frame :: (Maybe Frame)
              , ns :: (Either (Namespace String) ([String], Map String (Type, Expr))) }
      deriving (Show)