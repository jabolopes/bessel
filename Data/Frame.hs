module Data.Frame where

import Data.Map (Map)
import qualified Data.Map as Map (empty)

import Data.Symbol

-- | 'Frame' represents a part in the lexical scope tree.  When 'fid'
-- and 'parentId' are equal then the frame has no parent.  In other
-- words, it is the root of the lexical scope tree.
data Frame =
  Frame { -- | 'frId' is the 'Frame' id.
          frId :: Int
          -- | 'frParentId' is the 'Frame' id of the parent.
        , frParentId :: Int
          -- | 'frSymbols' maps bound names to symbols within this
          -- 'Frame'.
        , frSymbols :: Map String Symbol }

-- | 'empty' @frameId parentId@ returns a frame with the given @frameId@ and
-- @parentId@, and @symbols@ is 'Map.empty'.
empty :: Int -> Int -> Frame
empty frameId parentId =
  Frame { frId = frameId
        , frParentId = parentId
        , frSymbols = Map.empty }
