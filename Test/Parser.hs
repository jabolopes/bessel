{-# LANGUAGE FlexibleContexts #-}
module Test.Parser where

import Control.Monad.Except (runExceptT)

import qualified Data.PrettyString as PrettyString
import qualified Monad.NameT as NameT
import qualified Pretty.Data.Source as Pretty
import qualified Test.Diff as Diff
import qualified Test.Stage as Stage

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
      do result <- NameT.runNameT . runExceptT $ Stage.parseFile filename
         let actual = PrettyString.toString . Pretty.docSource <$> result
         Diff.expectFiles "Parser" filename generateTestExpectations expectedFilename actual
