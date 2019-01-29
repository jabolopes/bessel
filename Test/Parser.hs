{-# LANGUAGE StandaloneDeriving #-}
module Test.Parser where

import Control.Monad (when)

import qualified Data.Name as Name
import qualified Data.PrettyString as PrettyString
import Data.Source (Source)
import qualified Parser
import qualified Pretty.Data.Source as Pretty
import qualified Test.Diff as Diff

parseTestFile :: String -> IO Source
parseTestFile filename =
  do str <- readFile filename
     case Parser.parseFile (Name.untyped filename) str of
       Left err -> fail err
       Right src -> return src

expectFiles :: Bool -> String -> String -> IO ()
expectFiles False expectedFilename filename =
  do expectedSource <- readFile expectedFilename
     actualSource <- PrettyString.toString . Pretty.docSource <$> parseTestFile filename
     when (expectedSource /= actualSource) $
        fail $ "Parser" ++ "\n" ++
               "In: " ++ filename ++ "\n" ++
               "Expected: " ++ "\n" ++ expectedSource ++ "\n" ++
               "Actual: " ++ "\n" ++ actualSource ++ "\n" ++
               "Diff: " ++ "\n" ++ Diff.diff expectedSource actualSource
expectFiles True expectedFilename filename =
  do actualSource <- PrettyString.toString . Pretty.docSource <$> parseTestFile filename
     writeFile expectedFilename actualSource

testParser :: Bool -> IO ()
testParser generateTestExpectations =
  do expect "Test/TestData1.parser" "Test/TestData1.bsl"
     expect "Test/TestData2.parser" "Test/TestData2.bsl"
     expect "Test/TestData3.parser" "Test/TestData3.bsl"
     expect "Test/TestData4.parser" "Test/TestData4.bsl"
     expect "Test/TestData5.parser" "Test/TestData5.bsl"
     expect "Test/TestData6.parser" "Test/TestData6.bsl"
     expect "Test/TestData7.parser" "Test/TestData7.bsl"
     expect "Test/TestData8.parser" "Test/TestData8.bsl"
     expect "Test/TestData9.parser" "Test/TestData9.bsl"
     expect "Test/TestData10.parser" "Test/TestData10.bsl"
  where
    expect = expectFiles generateTestExpectations
