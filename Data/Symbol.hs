module Data.Symbol where


data Symbol
    = FnSymbol String
    | TypeSymbol String
      deriving (Show)


name :: Symbol -> String
name (FnSymbol str) = str
name (TypeSymbol str) = str