module Data.Symbol where

import Data.Stx


data Symbol
    = CotypeSymbol String Int [Observation]
    | FnSymbol String
    | ModuleSymbol Int
    | TypeSymbol Int
      deriving (Show)


isCotypeSymbol :: Symbol -> Bool
isCotypeSymbol CotypeSymbol {} = True
isCotypeSymbol _ = False


isModuleSymbol :: Symbol -> Bool
isModuleSymbol (ModuleSymbol _) = True
isModuleSymbol _ = False


isTypeSymbol :: Symbol -> Bool
isTypeSymbol (TypeSymbol _) = True
isTypeSymbol _ = False