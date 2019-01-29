module Test.Diff where

import qualified Data.Algorithm.Diff as Diff
import qualified Data.Algorithm.DiffOutput as Diff

diff :: String -> String -> String
diff expected actual =
  Diff.ppDiff $ Diff.getGroupedDiff (lines expected) (lines actual)
