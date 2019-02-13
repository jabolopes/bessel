{-# LANGUAGE StandaloneDeriving #-}
module Test.Parser where

import qualified Data.Name as Name
import Data.PrettyString (PrettyString)
import qualified Data.PrettyString as PrettyString
import Data.Source (Source)
import qualified Parser
import qualified Pretty.Data.Source as Pretty
import qualified Test.Diff as Diff

parseTestFile :: String -> IO (Either PrettyString Source)
parseTestFile filename =
  do str <- readFile filename
     case Parser.parseFile (Name.untyped filename) str of
       -- TODO: Remove PrettyString.text by converting Parser errors to PrettyString.
       Left err -> return . Left $ PrettyString.text err
       Right src -> return $ Right src

testParser :: Bool -> IO ()
testParser generateTestExpectations =
  do expect "Test/TestData1.parser" "Test/TestData1.bsl"
     expect "Test/TestData2.parser" "Test/TestData2.bsl"
     expect "Test/TestData3.parser" "Test/TestData3.bsl"
     expect "Test/TestData4.parser" "Test/TestData4.bsl"
     expect "Test/TestData5.parser" "Test/TestData5.bsl"
     expect "Test/TestData6.parser" "Test/TestData6.bsl"
     expect "Test/TestData7.parser" "Test/TestData7.bsl"
     expect "Test/TestData9.parser" "Test/TestData9.bsl"
     expect "Test/TestData10.parser" "Test/TestData10.bsl"
     expect "Test/Unit.parser" "Test/Unit.bsl"
     expect "Test/Variant.parser" "Test/Variant.bsl"
  where
    expect expectedFilename filename =
      do result <- parseTestFile filename
         let actual = PrettyString.toString . Pretty.docSource <$> result
         Diff.expectFiles "Parser" filename generateTestExpectations expectedFilename actual
