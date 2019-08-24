{-# LANGUAGE FlexibleContexts #-}
module Test.Stage.Linearizer where

import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (runStateT)

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
         Renamer.addFnSymbolM "true" "true#"
         Renamer.addFnSymbolM "false#" "false#"
         Renamer.addFnSymbolM "false" "false#"
         Renamer.addFnSymbolM "+" "+"
         Renamer.addFnSymbolM ">" ">"
         Renamer.addFnSymbolM "addIntReal" "addIntReal"
         Renamer.addFnSymbolM "isInt#" "isInt#"
         Renamer.addFnSymbolM "isInt" "isInt#"
         Renamer.addFnSymbolM "isReal#" "isReal#"
         Renamer.addFnSymbolM "isReal" "isReal#"
         Renamer.addFnSymbolM "isChar#" "isChar#"
         Renamer.addFnSymbolM "isChar" "isChar#"
         Renamer.addFnSymbolM "isList#" "isList#"
         Renamer.addFnSymbolM "isString#" "isString#"
         Renamer.addFnSymbolM "isString" "isString#"
         Renamer.addFnSymbolM "null#" "null#"
         Renamer.addFnSymbolM "null" "null#"
         Renamer.addFnSymbolM "case" "case"
         Renamer.addFnSymbolM "cons" "cons"
         Renamer.addFnSymbolM "head#" "head#"
         Renamer.addFnSymbolM "tail#" "tail#"
         Renamer.addFnSymbolM "isHeadTail#" "isHeadTail#"
         Renamer.addFnSymbolM "eqChar#" "eqChar#"
         Renamer.addFnSymbolM "eqInt#" "eqInt#"
         Renamer.addFnSymbolM "eqInt" "eqInt#"
         Renamer.addFnSymbolM "eqReal#" "eqReal#"
         Renamer.addFnSymbolM "eqString#" "eqString#"
         -- Tuple
         Renamer.addFnSymbolM "isTuple0" "isTuple0"
         Renamer.addFnSymbolM "isTuple2" "isTuple2"
         Renamer.addFnSymbolM "isTuple4" "isTuple4"
         Renamer.addFnSymbolM "mkTuple0" "mkTuple0"
         Renamer.addFnSymbolM "mkTuple2" "mkTuple2"
         Renamer.addFnSymbolM "mkTuple4" "mkTuple4"
         Renamer.addFnSymbolM "tuple2Ref0#" "tuple2Ref0#"
         Renamer.addFnSymbolM "tuple2Ref1#" "tuple2Ref1#"
         Renamer.addFnSymbolM "tuple4Ref0#" "tuple4Ref0#"
         Renamer.addFnSymbolM "tuple4Ref2#" "tuple4Ref2#"
         -- Type
         Renamer.addFnSymbolM "isType#" "isType#"
         -- Variant
         Renamer.addFnSymbolM "isVariant0#" "isVariant0#"
         Renamer.addFnSymbolM "isVariant#" "isVariant#"
         Renamer.addFnSymbolM "mkVariant0#" "mkVariant0#"
         Renamer.addFnSymbolM "mkVariant#" "mkVariant#"
         Renamer.addFnSymbolM "unVariant#" "unVariant#"
         -- Custom
         Renamer.addFnSymbolM "isMyCons" "isMyCons"
         Renamer.addFnSymbolM "mkMyCons" "mkMyCons"
         Renamer.addFnSymbolM "unMyCons" "unMyCons"
         -- Custom
         Renamer.addFnSymbolM "mkApple0" "mkApple0"
         Renamer.addFnSymbolM "unApple" "unApple"

linearizeTestFile :: (MonadError PrettyString m, MonadIO m, MonadName m) => String -> m [Expr]
linearizeTestFile filename =
  do initialState <- initialRenamerState
     Stage.linearizeFile filename initialState

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
      do result <- NameT.runNameT . runExceptT $ linearizeTestFile filename
         let actual = PrettyString.toString . PrettyString.vcat . map Pretty.docExpr <$> result
         Diff.expectFiles "Linearizer" filename generateTestExpectations expectedFilename actual
