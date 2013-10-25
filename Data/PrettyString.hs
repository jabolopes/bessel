{-# LANGUAGE TypeSynonymInstances #-}
module Data.PrettyString
  (PrettyString,
   (<>),
   (<+>),
   ($$),
   ($+$),
   PrettyPrint.cat,
   PrettyPrint.char,
   PrettyPrint.double,
   PrettyPrint.hcat,
   PrettyPrint.isEmpty,
   PrettyPrint.empty,
   PrettyPrint.equals,
   PrettyPrint.int,
   PrettyPrint.parens,
   PrettyPrint.sep,
   PrettyPrint.text,
   PrettyPrint.vcat,
   intercalate,
   nest,
   toString) where

import Control.Monad.Error

import Text.PrettyPrint (Doc, (<>), (<+>), ($$), ($+$))
import qualified Text.PrettyPrint as PrettyPrint

type PrettyString = Doc

instance Error PrettyString where
  strMsg = PrettyPrint.text

intercalate :: PrettyString -> [PrettyString] -> [PrettyString]
intercalate = PrettyPrint.punctuate

nesting :: Int
nesting = 2

nest :: PrettyString -> PrettyString
nest = PrettyPrint.nest nesting

toString :: PrettyString -> String
toString = PrettyPrint.renderStyle PrettyPrint.style
