module Data.SrcFile where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Frame
import Data.Stx
import Data.Symbol
import Data.Type
import Monad.InterpreterM


data SrcFile
    = SrcFile String [String] (Maybe Frame) (Either (Namespace String) (Map String (Type, Expr)))
      deriving (Show)