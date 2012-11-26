module Data.Frame where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Symbol


data Frame =
    Frame { frameId :: Int
          , frameParent :: Int
          , frameFns :: Map String FnSymbol
          , frameTypes :: Map String TypeSymbol
          , frameMods :: Map String Int }
    deriving (Show)


instance Eq Frame where
    Frame { frameId = id1 } == Frame { frameId = id2 } = id1 == id2


emptyFrame :: Int -> Int -> Frame
emptyFrame id parentId =
    Frame { frameId = id
          , frameParent = parentId
          , frameFns = Map.empty
          , frameTypes = Map.empty
          , frameMods = Map.empty }


initialFrame :: Int -> Int -> Map String FnSymbol -> Frame
initialFrame id parentId fns =
    (emptyFrame id parentId) { frameFns = fns }


copyFrame :: Int -> Int -> Frame -> Frame
copyFrame id parentId frame =
    frame { frameId = id
          , frameParent = parentId }