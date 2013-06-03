module Data.FrameTree where

import Data.Map (Map)
import qualified Data.Map as Map (empty, fromList, insert, lookup, toList)

import Data.Frame (Frame(..))
import qualified Data.Frame as Frame
import Data.Symbol (Symbol (..))


-- | 'FrameTree' represents the lexical scope tree which is composed
-- of 'Frame's.
data FrameTree =
    FrameTree { -- | 'frames' maps 'Frame' ids into 'Frame's.
                frames :: Map Int Frame
                -- | 'frameCount' is a counter for 'Frame' ids.
              , frameCount :: Int
                -- | 'rootId' is the 'Frame' id of the root 'Frame'.
              , rootId :: Int }


-- | 'empty' returns a 'FrameTree' containing only the root 'Frame'.
empty :: FrameTree
empty =
    let fid = 0 in
    FrameTree { frames = Map.fromList [(fid, Frame.empty fid fid)]
             , frameCount = fid + 1
             , rootId = fid }


-- | 'getFrame' @tree fid@ returns the 'Frame' with id 'fid'.
getFrame :: FrameTree -> Int -> Maybe Frame
getFrame tree fid = Map.lookup fid (frames tree)


-- | 'getRootFrame' @tree@ returns the root 'Frame'.
getRootFrame :: FrameTree -> Frame
getRootFrame tree =
    case Map.lookup (rootId tree) (frames tree) of
      Nothing -> error "FrameTree.getRootFrame: root frame does not exist"
      Just x -> x


-- | 'putFrame' @tree frame@ returns the updated 'FrameTree' that
-- results from replacing the 'Frame' with id @Frame.fid frame@ with
-- @frame@.
putFrame :: FrameTree -> Frame -> FrameTree
putFrame tree frame =
    tree { frames = Map.insert (Frame.fid frame) frame (frames tree) }


-- | 'addFrame' @tree parent@ creates a new 'Frame', with parent given
-- by the @parent@, and inserts it into @tree@.  'addFrame' returns the
-- updated 'FrameTree' and the newly created 'Frame'.
addFrame :: FrameTree -> Frame -> (FrameTree, Frame)
addFrame tree parent =
    let
        frame = Frame.empty (frameCount tree) (Frame.fid parent)
        tree' = putFrame tree frame
        tree'' = tree' { frameCount = frameCount tree' + 1 }
    in
      (tree'', frame)


-- | 'getLexicalSymbol' @tree frame name@ returns the 'Symbol' bound by
-- @name@ by performing a lexically scoped search in @tree@ starting at
-- @frame@ and stopping at the root 'Frame'.
getLexicalSymbol :: FrameTree -> Frame -> String -> Maybe Symbol
getLexicalSymbol tree frame name =
    case Map.lookup name (Frame.symbols frame) of
      Nothing | Frame.fid frame == Frame.parentId frame -> Nothing
              | otherwise -> let Just parent = getFrame tree (Frame.parentId frame) in
                             getLexicalSymbol tree parent name
      Just x -> Just x


-- | 'getFrameSymbol' @frame name@ returns the 'Symbol' bound by
-- @name@ by performing a search only in @frame@.  Contrary to
-- 'getLexicalSymbol', no lexically scoped search is performed.
getFrameSymbol :: Frame -> String -> Maybe Symbol
getFrameSymbol frame name = Map.lookup name (Frame.symbols frame)


-- | 'addSymbol' @tree frame name sym@ returns the updated 'FrameTree'
-- that results from binding @name@ to @sym@ in @frame@.
addSymbol :: FrameTree -> Frame -> String -> Symbol -> FrameTree
addSymbol tree frame name sym =
    let frame' = frame { symbols = Map.insert name sym (Frame.symbols frame) } in
    putFrame tree frame'


getSymbols :: FrameTree -> Map String Symbol
getSymbols tree = Map.fromList $ symbolsFrame "" $ getRootFrame tree
    where symbolsFrame prefix frame = loopSymbols prefix $ Map.toList $ Frame.symbols frame

          append "" name = name
          append prefix name = prefix ++ "." ++ name

          loopSymbols _ [] = []
          loopSymbols prefix ((name, sym):syms) =
              case sym of
                ModuleSymbol fid -> let Just frame = getFrame tree fid in
                                    loopSymbols prefix syms ++ symbolsFrame (append prefix name) frame
                sym -> (append prefix name, sym):loopSymbols prefix syms