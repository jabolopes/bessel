module Data.Symbol where

import Data.Stx


data Symbol
    = CotypeSymbol String Int [Observation]
    | FnSymbol String
    | ModuleSymbol Int
    | TypeSymbol Int
      deriving (Show)
