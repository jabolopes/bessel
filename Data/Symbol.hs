module Data.Symbol where


data Symbol
    = FnSymbol String
    | TypeSymbol String
      deriving (Show)