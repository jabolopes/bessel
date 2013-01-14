module Data.Frame where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Symbol


data Frame =
    Frame { frameId :: Int
          , frameParent :: Int
          , frameSyms :: Map String Symbol
          , frameMods :: Map String Int }
    deriving (Show)


instance Eq Frame where
    Frame { frameId = id1 } == Frame { frameId = id2 } = id1 == id2


empty :: Int -> Int -> Frame
empty id parentId =
    Frame { frameId = id
          , frameParent = parentId
          , frameSyms = Map.empty
          , frameMods = Map.empty }


copy :: Int -> Int -> Frame -> Frame
copy id parentId frame =
    frame { frameId = id
          , frameParent = parentId }