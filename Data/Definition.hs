module Data.Definition where

import Data.Expr (Expr)
import Data.Symbol (Symbol)
import Doc.Doc (Doc)
import qualified Doc.Doc as Doc (empty)
import Monad.InterpreterM (Val)


data Definition
  = Definition { defName :: String
               , defFreeNames :: [String]
               , defUnprefixedUses :: [String]
               , defPrefixedUses :: [(String, String)]
               , defSym :: Maybe Symbol
               , defVal :: Either String Val
               , defSrc :: Maybe Expr
               , defExp :: Maybe Expr
               , defRen :: Either Doc Expr }
    deriving (Show)


initial :: String -> Definition
initial name =
  Definition { defName = name
             , defFreeNames = []
             , defUnprefixedUses = []
             , defPrefixedUses = []
             , defSym = Nothing
             , defVal = Left ""
             , defSrc = Nothing
             , defExp = Nothing
             , defRen = Left Doc.empty }
