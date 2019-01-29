module Test.Diff where

import Control.Monad (when)
import qualified Data.Algorithm.Diff as Diff
import qualified Data.Algorithm.DiffOutput as Diff

diff :: String -> String -> String
diff expected actual =
  Diff.ppDiff $ Diff.getGroupedDiff (lines expected) (lines actual)

expectFiles :: String -> String -> Bool -> String -> String -> IO ()
expectFiles testName testCase False expectedFilename actual =
  do expected <- readFile expectedFilename
     when (expected /= actual) $
        fail $ testName ++ "\n" ++
               "In: " ++ testCase ++ "\n" ++
               "Expected: " ++ "\n" ++ expected ++ "\n" ++
               "Actual: " ++ "\n" ++ actual ++ "\n" ++
               "Diff: " ++ "\n" ++ diff expected actual
expectFiles _ _ True expectedFilename actual =
  writeFile expectedFilename actual
