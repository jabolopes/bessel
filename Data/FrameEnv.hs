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
             , moduleFrames :: Map String Frame
             , unprefixedEnvs :: Map String FrameEnv
             , prefixedEnvs :: Map String FrameEnv }
    deriving (Show)


empty :: FrameEnv
empty =
    let frame = Frame.empty 0 0 in
    FrameEnv { frames = Map.fromList [(frameId frame, frame)]
             , frameCount = 1
             , rootId = frameId frame
             , moduleFrames = Map.empty
             , unprefixedEnvs = Map.empty
             , prefixedEnvs = Map.empty }


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


getLexicalSymbol :: FrameEnv -> Frame -> String -> Maybe Symbol
getLexicalSymbol frameEnv frame@Frame { frameId, frameParent, frameSyms } name =
    case Map.lookup name frameSyms of
      Nothing | frameId == frameParent -> Nothing
              | otherwise -> getLexicalSymbol frameEnv (getFrame frameEnv frameParent) name
      Just sym -> Just sym


getFrameSymbol _ frame@Frame { frameSyms } name =
    Map.lookup name frameSyms


addSymbol :: FrameEnv -> Frame -> String -> Symbol -> FrameEnv
addSymbol frameEnv frame@Frame { frameSyms } name sym =
    let frame' = frame { frameSyms = Map.insert name sym frameSyms } in
    putFrame frameEnv frame'


mkQual :: [String] -> String
mkQual = intercalate "."


getModuleFrame :: FrameEnv -> [String] -> Maybe Frame
getModuleFrame frameEnv qual =
    Map.lookup (mkQual qual) (moduleFrames frameEnv)


addModuleFrame :: FrameEnv -> [String] -> Frame -> FrameEnv
addModuleFrame frameEnv qual frame =
    frameEnv { moduleFrames = Map.insert (mkQual qual) frame (moduleFrames frameEnv) }


getUnprefixedFrames :: FrameEnv -> [Frame]
getUnprefixedFrames frameEnv@FrameEnv { unprefixedEnvs } =
    map getRootFrame $ Map.elems unprefixedEnvs


addUnprefixedEnv :: FrameEnv -> String -> FrameEnv -> FrameEnv
addUnprefixedEnv frameEnv@FrameEnv { unprefixedEnvs } name env =
    frameEnv { unprefixedEnvs = Map.insert name env unprefixedEnvs }


getPrefixedFrame :: FrameEnv -> [String] -> Maybe Frame
getPrefixedFrame frameEnv@FrameEnv { prefixedEnvs } qual =
    getRootFrame <$> Map.lookup (mkQual qual) prefixedEnvs


addPrefixedEnv :: FrameEnv -> String -> FrameEnv -> FrameEnv
addPrefixedEnv frameEnv@FrameEnv { prefixedEnvs } name env =
    frameEnv { prefixedEnvs = Map.insert name env prefixedEnvs }