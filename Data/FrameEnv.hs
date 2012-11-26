{-# LANGUAGE NamedFieldPuns #-}
module Data.FrameEnv where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Frame
import qualified Data.Frame as Frame

import Data.Symbol


data FrameEnv =
    FrameEnv { frames :: Map Int Frame
             , modFrames :: Map String Frame
             , rootId :: Int
             , frameCount :: Int }


empty :: FrameEnv
empty =
    let frame = emptyFrame 0 0 in
    FrameEnv { frames = Map.fromList [(frameId frame, frame)]
             , modFrames = Map.empty
             , rootId = frameId frame
             , frameCount = 1 }


initial :: Map String FnSymbol -> FrameEnv
initial frameSyms =
    let frame = initialFrame 0 0 frameSyms in
    FrameEnv { frames = Map.fromList [(frameId frame, frame)]
             , modFrames = Map.empty
             , rootId = frameId frame
             , frameCount = 1 }


getFrame :: FrameEnv -> Int -> Frame
getFrame FrameEnv { frames } id =
    case Map.lookup id frames of
      Nothing -> error $ "FrameEnv.getFrame: id " ++ show (show id)
      Just frame -> frame


putFrame :: FrameEnv -> Frame -> FrameEnv
putFrame frameEnv@FrameEnv { frames } frame@Frame { frameId } =
    frameEnv { frames = Map.insert frameId frame frames }


addFrame :: FrameEnv -> Frame -> (FrameEnv, Frame)
addFrame frameEnv parent =
    let
        frame = Frame.emptyFrame (frameCount frameEnv) (frameId parent)
        frameEnv' = putFrame frameEnv frame
        frameEnv'' = frameEnv' { frameCount = frameCount frameEnv' + 1 }
    in
      (frameEnv'', frame)


putFrameChild :: FrameEnv -> Frame -> Frame -> (FrameEnv, Frame)
putFrameChild frameEnv frame child =
    let
        child' = Frame.copyFrame (frameCount frameEnv) (frameId frame) child
        frameEnv' = putFrame frameEnv child'
        frameEnv'' = frameEnv' { frameCount = frameCount frameEnv' + 1 }
    in
      (frameEnv'', child')


putFrameParent :: FrameEnv -> Frame -> Frame -> FrameEnv
putFrameParent frameEnv frame parent@Frame { frameId } =
    let
        frame' = frame { frameParent = frameId }
        frameEnv' = putFrame frameEnv frame'
    in
      frameEnv'


getFnSymbol :: FrameEnv -> Frame -> String -> Maybe FnSymbol
getFnSymbol frameEnv frame@Frame { frameId, frameParent, frameFns } name =
    case Map.lookup name frameFns of
      Nothing | frameId == frameParent -> Nothing
              | otherwise -> getFnSymbol frameEnv (getFrame frameEnv frameParent) name
      Just symbol -> Just symbol


getTypeSymbol :: FrameEnv -> Frame -> String -> Maybe TypeSymbol
getTypeSymbol frameEnv frame@Frame { frameId, frameParent, frameTypes } name =
    case Map.lookup name frameTypes of
      Nothing | frameId == frameParent -> Nothing
              | otherwise -> getTypeSymbol frameEnv (getFrame frameEnv frameParent) name
      Just symbol -> Just symbol


addFnSymbol :: FrameEnv -> Frame -> String -> FnSymbol -> FrameEnv
addFnSymbol frameEnv frame@Frame { frameFns } name sym =
    let frame' = frame { frameFns = Map.insert name sym frameFns } in
    putFrame frameEnv frame'


addTypeSymbol :: FrameEnv -> Frame -> String -> TypeSymbol -> FrameEnv
addTypeSymbol frameEnv frame@Frame { frameTypes } name sym =
    let frame' = frame { frameTypes = Map.insert name sym frameTypes } in
    putFrame frameEnv frame'


getModuleFrame :: FrameEnv -> Frame -> [String] -> Maybe Frame
getModuleFrame frameEnv frame [] = Just frame
getModuleFrame frameEnv Frame { frameMods } (name:names) =
    case Map.lookup name frameMods of
      Nothing -> Nothing
      Just frameId -> let frame' = getFrame frameEnv frameId in
                      getModuleFrame frameEnv frame' names


addModuleFrame :: FrameEnv -> Frame -> String -> Frame -> (FrameEnv, Frame)
addModuleFrame frameEnv frame@Frame { frameMods } name Frame { frameId } =
    let
        frame' = frame { frameMods = Map.insert name frameId frameMods }
        frameEnv' = putFrame frameEnv frame'
    in
      (frameEnv', frame')