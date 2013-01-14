{-# LANGUAGE NamedFieldPuns #-}
module Data.FrameEnv where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Frame (Frame(..))
import qualified Data.Frame as Frame

import Data.Symbol


data FrameEnv =
    FrameEnv { frames :: Map Int Frame
             , frameCount :: Int
             , rootId :: Int }


empty :: FrameEnv
empty =
    let frame = Frame.empty 0 0 in
    FrameEnv { frames = Map.fromList [(frameId frame, frame)]
             , frameCount = 1
             , rootId = frameId frame }


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
        frame = Frame.empty (frameCount frameEnv) (frameId parent)
        frameEnv' = putFrame frameEnv frame
        frameEnv'' = frameEnv' { frameCount = frameCount frameEnv' + 1 }
    in
      (frameEnv'', frame)


putFrameChild :: FrameEnv -> Frame -> Frame -> (FrameEnv, Frame)
putFrameChild frameEnv frame child =
    let
        child' = Frame.copy (frameCount frameEnv) (frameId frame) child
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


getFnSymbol :: FrameEnv -> Frame -> String -> Maybe Symbol
getFnSymbol frameEnv frame@Frame { frameId, frameParent, frameSyms } name =
    case Map.lookup name frameSyms of
      Nothing | frameId == frameParent -> Nothing
              | otherwise -> getFnSymbol frameEnv (getFrame frameEnv frameParent) name
      Just sym@(FnSymbol _) -> Just sym
      _ -> error $ "FrameEnv.getFnSymbol: " ++ show name ++ " is not a function symbol"


getTypeSymbol :: FrameEnv -> Frame -> String -> Maybe Symbol
getTypeSymbol frameEnv frame@Frame { frameId, frameParent, frameSyms } name =
    case Map.lookup name frameSyms of
      Nothing | frameId == frameParent -> Nothing
              | otherwise -> getTypeSymbol frameEnv (getFrame frameEnv frameParent) name
      Just sym@(TypeSymbol _) -> Just sym
      _ -> error $ "FrameEnv.getTypeSymbol: " ++ show name ++ " is not a type symbol"


addSymbol :: FrameEnv -> Frame -> String -> Symbol -> FrameEnv
addSymbol frameEnv frame@Frame { frameSyms } name sym =
    let frame' = frame { frameSyms = Map.insert name sym frameSyms } in
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