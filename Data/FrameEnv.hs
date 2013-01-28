{-# LANGUAGE NamedFieldPuns #-}
module Data.FrameEnv where

import Data.Functor ((<$>))
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Frame (Frame(..))
import qualified Data.Frame as Frame

import Data.Symbol


data FrameEnv =
    FrameEnv { frames :: Map Int Frame
             , frameCount :: Int
             , rootId :: Int
             , namespaceFrames :: Map String ([Int], Map String Int) }


empty :: FrameEnv
empty =
    let frame = Frame.empty 0 0 in
    FrameEnv { frames = Map.fromList [(frameId frame, frame)]
             , frameCount = 1
             , rootId = frameId frame
             , namespaceFrames = Map.empty }


getFrame :: FrameEnv -> Int -> Frame
getFrame FrameEnv { frames } id =
    case Map.lookup id frames of
      Nothing -> error $ "FrameEnv.getFrame" ++
                         "\n\n\t id = " ++ show id ++ "\n"
      Just frame -> frame


getRootFrame :: FrameEnv -> Frame
getRootFrame frameEnv@FrameEnv { rootId } =
    getFrame frameEnv rootId


putFrame :: FrameEnv -> Frame -> FrameEnv
putFrame frameEnv@FrameEnv { frames } frame@Frame { frameId } =
    frameEnv { frames = Map.insert frameId frame frames }


addFrame :: FrameEnv -> Frame -> (FrameEnv, Frame)
addFrame frameEnv parent =
    let
        frame = Frame.empty (frameCount frameEnv) (frameId parent)
        frameEnv' = putFrame frameEnv frame
        frameEnv'' = frameEnv' { frameCount = frameCount frameEnv' + 1 }
    in
      (frameEnv'', frame)


getSymbol :: FrameEnv -> Frame -> String -> Maybe Symbol
getSymbol frameEnv frame@Frame { frameId, frameParent, frameSyms } name =
    case Map.lookup name frameSyms of
      Nothing | frameId == frameParent -> Nothing
              | otherwise -> getSymbol frameEnv (getFrame frameEnv frameParent) name
      Just sym -> Just sym


addSymbol :: FrameEnv -> Frame -> String -> Symbol -> FrameEnv
addSymbol frameEnv frame@Frame { frameSyms } name sym =
    let frame' = frame { frameSyms = Map.insert name sym frameSyms } in
    putFrame frameEnv frame'


getUnprefixedFrameIds :: FrameEnv -> String -> [Int]
getUnprefixedFrameIds frameEnv@FrameEnv { namespaceFrames } ns =
    case Map.lookup ns namespaceFrames of
      Nothing -> []
      Just (frs, _) -> frs


getUnprefixedFrames :: FrameEnv -> String -> [Frame]
getUnprefixedFrames frameEnv ns =
    map (getFrame frameEnv) $ getUnprefixedFrameIds frameEnv ns


setUnprefixedFrames :: FrameEnv -> String -> [Frame] -> FrameEnv
setUnprefixedFrames frameEnv@FrameEnv { namespaceFrames } ns frames =
    let
        prefixedFrames = getPrefixedFrames frameEnv ns
        namespaceFrames' = Map.insert ns (map frameId frames, prefixedFrames) namespaceFrames
    in
      frameEnv { namespaceFrames = namespaceFrames' }


addUnprefixedFrame :: FrameEnv -> String -> Frame -> FrameEnv
addUnprefixedFrame frameEnv ns frame =
    let unprefixedFrames = getUnprefixedFrames frameEnv ns in
    setUnprefixedFrames frameEnv ns $ unprefixedFrames ++ [frame]


getPrefixedFrames :: FrameEnv -> String -> Map String Int
getPrefixedFrames frameEnv@FrameEnv { namespaceFrames } ns =
    case Map.lookup ns namespaceFrames of
      Nothing -> Map.empty
      Just (_, mp) -> mp


setPrefixedFrames :: FrameEnv -> String -> Map String Int -> FrameEnv
setPrefixedFrames frameEnv@FrameEnv { namespaceFrames } ns frames =
    let
        unprefixedFrames = getUnprefixedFrameIds frameEnv ns
        namespaceFrames' = Map.insert ns (unprefixedFrames, frames) namespaceFrames
    in
      frameEnv { namespaceFrames = namespaceFrames' }


getPrefixedFrame :: FrameEnv -> String -> [String] -> Maybe Frame
getPrefixedFrame frameEnv ns qual =
    let
        prefixedFrames = getPrefixedFrames frameEnv ns
        qual' = intercalate "." qual
    in
      getFrame frameEnv <$> Map.lookup qual' prefixedFrames


addPrefixedFrame :: FrameEnv -> String -> String -> Frame -> FrameEnv
addPrefixedFrame frameEnv ns name Frame { frameId } =
    let prefixedFrames = Map.insert name frameId (getPrefixedFrames frameEnv ns) in
    setPrefixedFrames frameEnv ns prefixedFrames