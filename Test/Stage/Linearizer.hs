module Test.Stage.Linearizer where

import Control.Monad.Except (runExceptT)

import qualified Data.PrettyString as PrettyString
import qualified Monad.NameT as NameT
import qualified Pretty.Data.Expr as Pretty
import qualified Test.Diff as Diff
import qualified Test.Stage as Stage

testLinearizer :: Bool -> IO ()
testLinearizer generateTestExpectations =
  do expect "Test/TestData1.linearizer" "Test/TestData1.bsl"
     expect "Test/TestData2.linearizer" "Test/TestData2.bsl"
     expect "Test/TestData3.linearizer" "Test/TestData3.bsl"
     expect "Test/TestData4.linearizer" "Test/TestData4.bsl"
     expect "Test/TestData5.linearizer" "Test/TestData5.bsl"
     expect "Test/TestData6.linearizer" "Test/TestData6.bsl"
     expect "Test/TestData7.linearizer" "Test/TestData7.bsl"
     expect "Test/TestData12.linearizer" "Test/TestData12.bsl"
     expect "Test/Lambda.linearizer" "Test/Lambda.bsl"
     expect "Test/Pattern.linearizer" "Test/Pattern.bsl"
     expect "Test/ResultPattern.linearizer" "Test/ResultPattern.bsl"
     expect "Test/Tuple.linearizer" "Test/Tuple.bsl"
     expect "Test/Unit.linearizer" "Test/Unit.bsl"
     expect "Test/Variant.linearizer" "Test/Variant.bsl"
  where
    expect expectedFilename filename =
      do result <- NameT.runNameT . runExceptT $ Stage.linearizeFile filename
         let actual = PrettyString.toString . PrettyString.vcat . map Pretty.docExpr <$> result
         Diff.expectFiles "Linearizer" filename generateTestExpectations expectedFilename actual
