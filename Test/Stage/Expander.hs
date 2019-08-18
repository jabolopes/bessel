{-# LANGUAGE FlexibleContexts #-}
module Test.Stage.Expander where

import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Expr (Expr(..))
import qualified Data.Name as Name
import Data.PrettyString (PrettyString)
import qualified Data.PrettyString as PrettyString
import Data.Source (Source(..))
import Monad.NameM (MonadName)
import qualified Monad.NameM as NameM
import qualified Parser
import qualified Pretty.Data.Expr as Pretty
import qualified Stage.Expander as Expander
import qualified Test.Diff as Diff

expandTestFile :: (MonadError PrettyString m, MonadIO m, MonadName m) => String -> m [Expr]
expandTestFile filename =
  do input <- liftIO $ readFile filename
     expandFile input
  where
    parseFile :: (MonadError PrettyString m, MonadName m) => String -> m Source
    parseFile input =
      case Parser.parseFile (Name.untyped filename) input of
        Left err -> throwError $ PrettyString.text err
        Right src -> return src

    expandFile :: (MonadError PrettyString m, MonadName m) => String -> m [Expr]
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
     expect "Test/Lambda.expander" "Test/Lambda.bsl"
     expect "Test/Pattern.expander" "Test/Pattern.bsl"
     expect "Test/ResultPattern.expander" "Test/ResultPattern.bsl"
     expect "Test/Tuple.expander" "Test/Tuple.bsl"
     expect "Test/Unit.expander" "Test/Unit.bsl"
     expect "Test/Variant.expander" "Test/Variant.bsl"
  where
    expect expectedFilename filename =
      do result <- NameM.runName . runExceptT $ expandTestFile filename
         let actual = PrettyString.toString . PrettyString.vcat . map Pretty.docExpr <$> result
         Diff.expectFiles "Expander" filename generateTestExpectations expectedFilename actual
