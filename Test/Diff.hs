module Test.Diff where

import Control.Monad (when)
import qualified Data.Algorithm.Diff as Diff
import qualified Data.Algorithm.DiffOutput as Diff

import Data.PrettyString (PrettyString)
import qualified Data.PrettyString as PrettyString

diff :: String -> String -> String
diff expected actual =
  Diff.ppDiff $ Diff.getGroupedDiff (lines expected) (lines actual)

expectFiles :: String -> String -> Bool -> String -> Either PrettyString String -> IO ()
expectFiles testName testCase _ _ (Left err) =
  putStrLn $ testName ++ " test\n" ++
             "In: " ++ testCase ++ "\n" ++
             "Error: " ++ "\n" ++ PrettyString.toString err
expectFiles testName testCase False expectedFilename (Right actual) =
  do expected <- readFile expectedFilename
     when (expected /= actual) $
        putStrLn $ testName ++ "\n" ++
                  "In: " ++ testCase ++ "\n" ++
                  "Expected: " ++ "\n" ++ expected ++ "\n" ++
                  "Actual: " ++ "\n" ++ actual ++ "\n" ++
                  "Diff: " ++ "\n" ++ diff expected actual
expectFiles _ _ True expectedFilename (Right actual) =
  writeFile expectedFilename actual
