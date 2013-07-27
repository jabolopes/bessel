module Data.FrameTree
  (FrameTree(rootId),
   empty,
   getFrame,
   getRootFrame,
   addFrame,
   getLexicalSymbol,
   addSymbol)
where

import Data.Map (Map)
import qualified Data.Map as Map (empty, fromList, insert, lookup, toList)
import Data.Maybe (fromMaybe)

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
  fromMaybe
    (error "FrameTree.getRootFrame: root frame does not exist")
    (Map.lookup (rootId tree) (frames tree))


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


lookupLexically :: (Frame -> Map String a) -> FrameTree -> Frame -> String -> Maybe a
lookupLexically fn tree frame name =
    case Map.lookup name (fn frame) of
      Nothing | Frame.fid frame == Frame.parentId frame -> Nothing
              | otherwise -> let Just parent = getFrame tree (Frame.parentId frame) in
                             lookupLexically fn tree parent name
      Just x -> Just x


-- | 'getLexicalSymbol' @tree frame name@ returns the 'Symbol' bound by
-- @name@ by performing a lexically scoped search in @tree@ starting at
-- @frame@ and stopping at the root 'Frame'.
getLexicalSymbol :: FrameTree -> Frame -> String -> Maybe Symbol
getLexicalSymbol = lookupLexically Frame.symbols


-- | 'addSymbol' @tree frame name sym@ returns the updated 'FrameTree'
-- that results from binding @name@ to @sym@ in @frame@.
addSymbol :: FrameTree -> Frame -> String -> Symbol -> FrameTree
addSymbol tree frame name sym =
    putFrame tree (addSymbol' sym)
    where addSymbol' sym =
              frame { symbols = Map.insert name sym (Frame.symbols frame) }