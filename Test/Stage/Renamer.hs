{-# LANGUAGE StandaloneDeriving #-}
module Test.Stage.Renamer where

import Control.Applicative ((<$>))
import Control.Monad.State hiding (state)

import Data.Expr (Expr)
import qualified Data.Name as Name
import qualified Data.PrettyString as PrettyString
import Data.Source (Source(..))
import qualified Parser
import qualified Pretty.Data.Expr as Pretty
import qualified Stage.Expander as Expander
import Stage.Renamer (RenamerState)
import qualified Stage.Renamer as Renamer
import qualified Test.Diff as Diff

initialRenamerState :: Monad m => m RenamerState
initialRenamerState =
  case snd <$> runStateT getRenamerState Renamer.initialRenamerState of
    Left err -> fail $ show err
    Right state -> return state
  where
    getRenamerState =
      do Renamer.addFnSymbolM "true#" "true#"
         Renamer.addFnSymbolM "false#" "false#"
         Renamer.addFnSymbolM "+" "+"
         Renamer.addFnSymbolM "addIntReal" "addIntReal#"
         Renamer.addFnSymbolM "isInt" "isInt#"
         Renamer.addFnSymbolM "isReal" "isReal#"
         -- Tuple
         Renamer.addFnSymbolM "isTuple2" "isTuple2#"
         Renamer.addFnSymbolM "mkTuple2" "mkTuple2#"
         Renamer.addFnSymbolM "tuple2Ref0" "tuple2Ref0#"
         Renamer.addFnSymbolM "tuple2Ref1" "tuple2Ref1#"

renameTestFile :: String -> IO [Expr]
renameTestFile filename = renameFile
  where
    parseFile =
      do str <- readFile filename
         case Parser.parseFile (Name.untyped filename) str of
           Left err -> fail err
           Right src -> return src

    expandFile =
      do ModuleS _ _ [src] <- parseFile
         case Expander.expand src of
           Left err -> fail $ show err
           Right exprs -> return (head exprs)

    renameFile =
      do expr <- expandFile
         state <- initialRenamerState
         case fst <$> runStateT (Renamer.renameM expr) state of
           Left err -> fail $ show err
           Right expr' -> return expr'

testRenamer :: Bool -> IO ()
testRenamer generateTestExpectations =
  do expect "Test/TestData1.renamer" "Test/TestData1.bsl"
     expect "Test/TestData2.renamer" "Test/TestData2.bsl"
     expect "Test/Tuple.renamer" "Test/Tuple.bsl"
  where
    expect expectedFilename filename =
      do actual <- (PrettyString.toString . PrettyString.vcat . map Pretty.docExpr) <$> renameTestFile filename
         Diff.expectFiles "Rename" filename generateTestExpectations expectedFilename actual
