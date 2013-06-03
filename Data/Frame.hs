module Data.Frame where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Symbol


-- | 'Frame' represents a part in the lexical scope tree.  When 'fid'
-- and 'parentId' are equal then the frame has no parent.  In other
-- words, it is the root of the lexical scope tree.
data Frame =
    Frame { -- | 'fid' is the frame id.
            fid :: Int
            -- | 'parentId' is the frame id of the parent.
          , parentId :: Int
            -- | 'symbols' maps bound names to symbols within this
            -- frame.
          , symbols :: Map String Symbol
          , moduleSymbols :: Map String Symbol }


-- | 'empty' @fid parentId@ returns a frame with the given @fid@ and
-- @parentId@, and @symbols@ is 'Map.empty'.
empty :: Int -> Int -> Frame
empty fid parentId =
    Frame { fid = fid
          , parentId = parentId
          , symbols = Map.empty
          , moduleSymbols = Map.empty }