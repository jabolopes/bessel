{-# LANGUAGE FlexibleContexts #-}
module Test.Stage.Renamer where

import Control.Applicative ((<$>))
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State hiding (state)

import Data.Expr (Expr)
import Data.PrettyString (PrettyString)
import qualified Data.PrettyString as PrettyString
import Monad.NameT (MonadName)
import qualified Monad.NameT as NameT
import qualified Pretty.Data.Expr as Pretty
import Stage.Renamer (RenamerState)
import qualified Stage.Renamer as Renamer
import qualified Test.Diff as Diff
import qualified Test.Stage as Stage

initialRenamerState :: MonadError PrettyString m => m RenamerState
initialRenamerState =
  case snd <$> runStateT getRenamerState Renamer.initialRenamerState of
    Left err -> throwError err
    Right state -> return state
  where
    getRenamerState =
      do Renamer.addFnSymbolM "check#" "check#"
         Renamer.addFnSymbolM "true#" "true#"
         Renamer.addFnSymbolM "false#" "false#"
         Renamer.addFnSymbolM "+" "+"
         Renamer.addFnSymbolM "addIntReal" "addIntReal#"
         Renamer.addFnSymbolM "isInt" "isInt#"
         Renamer.addFnSymbolM "isReal" "isReal#"
         -- Tuple
         Renamer.addFnSymbolM "isTuple2" "isTuple2#"
         Renamer.addFnSymbolM "mkTuple2" "mkTuple2#"
         Renamer.addFnSymbolM "tuple2Ref0#" "tuple2Ref0#"
         Renamer.addFnSymbolM "tuple2Ref1#" "tuple2Ref1#"

renameTestFile :: (MonadError PrettyString m, MonadIO m, MonadName m) => String -> m [Expr]
renameTestFile filename =
  do initialState <- initialRenamerState
     Stage.renameFile filename initialState

testRenamer :: Bool -> IO ()
testRenamer generateTestExpectations =
  do expect "Test/TestData1.renamer" "Test/TestData1.bsl"
     expect "Test/TestData2.renamer" "Test/TestData2.bsl"
     expect "Test/Tuple.renamer" "Test/Tuple.bsl"
  where
    expect expectedFilename filename =
      do result <- NameT.runNameT $ runExceptT $ renameTestFile filename
         let actual = PrettyString.toString . PrettyString.vcat . map Pretty.docExpr <$> result
         Diff.expectFiles "Renamer" filename generateTestExpectations expectedFilename actual
