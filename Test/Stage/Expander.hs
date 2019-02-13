{-# LANGUAGE LambdaCase, StandaloneDeriving #-}
module Test.Stage.Expander where

import Data.Expr (Expr(..))
import qualified Data.Name as Name
import Data.PrettyString (PrettyString)
import qualified Data.PrettyString as PrettyString
import Data.Source (Source(..))
import qualified Parser
import qualified Pretty.Data.Expr as Pretty
import qualified Stage.Expander as Expander
import qualified Test.Diff as Diff

expandTestFile :: String -> IO (Either PrettyString [Expr])
expandTestFile filename =
  do input <- readFile filename
     return $ expandFile input
  where
    parseFile :: String -> Either PrettyString Source
    parseFile input =
      case Parser.parseFile (Name.untyped filename) input of
        Left err -> Left $ PrettyString.text err
        Right src -> return src

    expandFile :: String -> Either PrettyString [Expr]
    expandFile input =
      do ModuleS _ _ srcs <- parseFile input
         concat <$> mapM Expander.expand srcs

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
     expect "Test/Pattern.expander" "Test/Pattern.bsl"
     expect "Test/Tuple.expander" "Test/Tuple.bsl"
     expect "Test/Unit.expander" "Test/Unit.bsl"
     expect "Test/Variant.expander" "Test/Variant.bsl"
  where
    expect expectedFilename filename =
      do result <- expandTestFile filename
         let actual = PrettyString.toString . PrettyString.vcat . map Pretty.docExpr <$> result
         Diff.expectFiles "Expander" filename generateTestExpectations expectedFilename actual
