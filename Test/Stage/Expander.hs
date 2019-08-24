module Test.Stage.Expander where

import Control.Monad.Except (runExceptT)

import qualified Data.PrettyString as PrettyString
import qualified Monad.NameT as NameT
import qualified Pretty.Data.Expr as Pretty
import qualified Test.Diff as Diff
import qualified Test.Stage as Stage

testExpander :: Bool -> IO ()
testExpander generateTestExpectations =
  do expect "Test/TestData1.expander" "Test/TestData1.bsl"
     expect "Test/TestData2.expander" "Test/TestData2.bsl"
     expect "Test/TestData3.expander" "Test/TestData3.bsl"
     -- TODO: TestData4 will not typecheck because eqSeq is accessing
     -- x and y with different types.
     expect "Test/TestData4.expander" "Test/TestData4.bsl"
     expect "Test/TestData5.expander" "Test/TestData5.bsl"
     expect "Test/TestData6.expander" "Test/TestData6.bsl"
     expect "Test/TestData7.expander" "Test/TestData7.bsl"
     expect "Test/TestData12.expander" "Test/TestData12.bsl"
     expect "Test/Lambda.expander" "Test/Lambda.bsl"
     expect "Test/Pattern.expander" "Test/Pattern.bsl"
     expect "Test/ResultPattern.expander" "Test/ResultPattern.bsl"
     expect "Test/Tuple.expander" "Test/Tuple.bsl"
     expect "Test/Unit.expander" "Test/Unit.bsl"
     expect "Test/Variant.expander" "Test/Variant.bsl"
  where
    expect expectedFilename filename =
      do result <- NameT.runNameT . runExceptT $ Stage.expandFile filename
         let actual = PrettyString.toString . PrettyString.vcat . map Pretty.docExpr <$> result
         Diff.expectFiles "Expander" filename generateTestExpectations expectedFilename actual
