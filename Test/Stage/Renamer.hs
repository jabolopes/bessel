{-# LANGUAGE StandaloneDeriving #-}
module Test.Stage.Renamer where

import Control.Applicative ((<$>))
import Control.Monad.State hiding (state)

import Data.Expr (Expr)
import qualified Data.Name as Name
import Data.PrettyString (PrettyString)
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

renameTestFile :: String -> IO (Either PrettyString [Expr])
renameTestFile filename =
  do input <- readFile filename
     return $ renameFile input
  where
    parseFile :: String -> Either PrettyString Source
    parseFile input =
      case Parser.parseFile (Name.untyped filename) input of
        -- TODO: Remove PrettyString.text by converting Parser errors to PrettyString.
        Left err -> Left $ PrettyString.text err
        Right src -> return src

    expandFile :: String -> Either PrettyString [Expr]
    expandFile input =
      do ModuleS _ _ srcs <- parseFile input
         concat <$> mapM Expander.expand srcs

    renameExprs :: RenamerState -> [Expr] -> Either PrettyString [Expr]
    renameExprs _ [] = return []
    renameExprs state (expr:exprs) =
      case runStateT (Renamer.renameM expr) state of
        Left err -> Left err
        Right (exprs', state') -> (exprs' ++) <$> renameExprs state' exprs

    renameFile :: String -> Either PrettyString [Expr]
    renameFile input =
      do exprs <- expandFile input
         state <- initialRenamerState
         renameExprs state exprs

testRenamer :: Bool -> IO ()
testRenamer generateTestExpectations =
  do expect "Test/TestData1.renamer" "Test/TestData1.bsl"
     expect "Test/TestData2.renamer" "Test/TestData2.bsl"
     expect "Test/Tuple.renamer" "Test/Tuple.bsl"
  where
    expect expectedFilename filename =
      do result <- renameTestFile filename
         let actual = PrettyString.toString . PrettyString.vcat . map Pretty.docExpr <$> result
         Diff.expectFiles "Renamer" filename generateTestExpectations expectedFilename actual
