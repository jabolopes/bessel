{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.PrettyString
  (PrettyString,
   (<>),
   (<+>),
   ($$),
   ($+$),
   PrettyPrint.cat,
   PrettyPrint.char,
   PrettyPrint.double,
   PrettyPrint.doubleQuotes,
   PrettyPrint.hcat,
   PrettyPrint.isEmpty,
   PrettyPrint.empty,
   PrettyPrint.equals,
   PrettyPrint.int,
   PrettyPrint.parens,
   PrettyPrint.quotes,
   PrettyPrint.sep,
   PrettyPrint.text,
   PrettyPrint.vcat,
   error,
   intercalate,
   nest,
   toString) where

import Prelude hiding (error)
import qualified Prelude (error)

import Control.Monad.Error

import Text.PrettyPrint (Doc, (<>), (<+>), ($$), ($+$))
import qualified Text.PrettyPrint as PrettyPrint

type PrettyString = Doc

instance Error PrettyString where
  strMsg = PrettyPrint.text

error :: String -> PrettyString -> a
error short long =
  Prelude.error .
    toString $
      PrettyPrint.text short
      $+$
      nest long

intercalate :: PrettyString -> [PrettyString] -> [PrettyString]
intercalate = PrettyPrint.punctuate

nesting :: Int
nesting = 2

nest :: PrettyString -> PrettyString
nest = PrettyPrint.nest nesting

toString :: PrettyString -> String
toString = PrettyPrint.renderStyle PrettyPrint.style
