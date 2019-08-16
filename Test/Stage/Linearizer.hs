{-# LANGUAGE LambdaCase, StandaloneDeriving #-}
module Test.Stage.Linearizer where

import qualified Compiler.Linearizer as Linearizer
import Data.Expr (Expr(..))
import qualified Data.Name as Name
import Data.PrettyString (PrettyString)
import qualified Data.PrettyString as PrettyString
import Data.Source (Source(..))
import qualified Monad.NameM as NameM
import qualified Parser
import qualified Pretty.Data.Expr as Pretty
import qualified Stage.Expander as Expander
import qualified Test.Diff as Diff

linearizeTestFile :: String -> IO (Either PrettyString [Expr])
linearizeTestFile filename =
  do input <- readFile filename
     return $ linearizeFile input
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

    linearizeFile :: String -> Either PrettyString [Expr]
    linearizeFile input =
      do exprs <- expandFile input
         NameM.runName $
           concat <$> mapM Linearizer.linearize exprs

testLinearizer :: Bool -> IO ()
testLinearizer generateTestExpectations =
  do expect "Test/TestData1.linearizer" "Test/TestData1.bsl"
     expect "Test/TestData2.linearizer" "Test/TestData2.bsl"
     expect "Test/TestData3.linearizer" "Test/TestData3.bsl"
     -- TODO: TestData4 will not typecheck because eqSeq is accessing
     -- x and y with different types.
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
      do result <- linearizeTestFile filename
         let actual = PrettyString.toString . PrettyString.vcat . map Pretty.docExpr <$> result
         Diff.expectFiles "Linearizer" filename generateTestExpectations expectedFilename actual
