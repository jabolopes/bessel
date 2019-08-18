{-# LANGUAGE FlexibleContexts #-}
module Test.Stage.Linearizer where

import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Compiler.Linearizer as Linearizer
import Data.Expr (Expr(..))
import qualified Data.Name as Name
import Data.PrettyString (PrettyString)
import qualified Data.PrettyString as PrettyString
import Data.Source (Source(..))
import Monad.NameT (MonadName)
import qualified Monad.NameT as NameT
import qualified Parser
import qualified Pretty.Data.Expr as Pretty
import qualified Stage.Expander as Expander
import qualified Test.Diff as Diff

linearizeTestFile :: (MonadError PrettyString m, MonadIO m, MonadName m) => String -> m [Expr]
linearizeTestFile filename =
  do input <- liftIO $ readFile filename
     linearizeFile input
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

    linearizeFile :: (MonadError PrettyString m, MonadName m) => String -> m [Expr]
    linearizeFile input =
      do exprs <- expandFile input
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
      do result <- NameT.runNameT . runExceptT $ linearizeTestFile filename
         let actual = PrettyString.toString . PrettyString.vcat . map Pretty.docExpr <$> result
         Diff.expectFiles "Linearizer" filename generateTestExpectations expectedFilename actual
