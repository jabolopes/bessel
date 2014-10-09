{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Stage.Renamer where

import Control.Applicative
import Control.Monad.State

import Data.Expr
import qualified Data.PrettyString as PrettyString
import Data.QualName (QualName (..))
import Data.Source
import qualified Parser
import qualified Pretty.Data.Expr as Pretty
import qualified Stage.Expander as Expander
import qualified Stage.Renamer as Renamer

deriving instance Eq DefnKw
deriving instance Eq Expr
deriving instance Eq QualName

renameTestFile :: String -> IO Expr
renameTestFile filename =
  do expr <- expandFile
     case fst <$> runState (Renamer.renameM expr) Renamer.initialRenamerState of
       Left err -> fail $ show err
       Right expr' -> return expr'
  where
    parseFile =
      do str <- readFile filename
         case Parser.parseFile filename str of
           Left err -> fail err
           Right src -> return src

    expandFile =
      do ModuleS _ _ [src] <- parseFile
         case Expander.expand src of
           Left err -> fail $ show err
           Right exprs -> return (head exprs)

-- edit: Use pretty strings instead
expect :: Expr -> String -> IO ()
expect expected filename =
  expect' =<< renameTestFile filename
  where
    expect' actual
      | expected == actual = return ()
      | otherwise =
        fail $ "In: " ++ filename ++ "\n" ++
               "Expected: " ++ "\n" ++ PrettyString.toString (Pretty.docExpr Pretty.ExpDocT expected) ++ "\n" ++
               "Actual: " ++ "\n" ++ PrettyString.toString (Pretty.docExpr Pretty.ExpDocT actual)

testRenamer :: IO ()
testRenamer =
  do expect expected1 "Test/TestData1.bsl"
     expect expected2 "Test/TestData2.bsl"
  where
    expected1 =
      FnDecl NrDef "f10"
      (LambdaE "x#01"
       (CondE [(AppE (IdE (QualName {fromQualName = "isInt"})) (IdE (QualName {fromQualName = "x#01"})),
                LambdaE "y#12"
                (CondE [(AppE (IdE (QualName {fromQualName = "isInt"})) (IdE (QualName {fromQualName = "y#12"})),
                         LetE (FnDecl NrDef "x3" (IdE (QualName {fromQualName = "x#01"})))
                         (LetE (FnDecl NrDef "y4" (IdE (QualName {fromQualName = "y#12"})))
                          (LetE
                           (FnDecl NrDef "f25"
                            (LambdaE "z#26"
                             (CondE [(AppE (LambdaE "_7" (IdE (QualName {fromQualName = "true#"}))) (IdE (QualName {fromQualName = "z#26"})),
                                      LetE (FnDecl NrDef "z8" (IdE (QualName {fromQualName = "z#26"})))
                                      (AppE (AppE (IdE (QualName {fromQualName = "+"})) (IdE (QualName {fromQualName = "z8"}))) (IdE (QualName {fromQualName = "y4"}))))]
                              "f2")))
                           (AppE (IdE (QualName {fromQualName = "f25"})) (IdE (QualName {fromQualName = "x3"}))))))]
                 "f1"))]
        "f1"))

    expected2 =
      FnDecl NrDef "f10"
      (LambdaE "x#01"
       (CondE [(AppE (IdE (QualName {fromQualName = "isInt"})) (IdE (QualName {fromQualName = "x#01"})),
                LetE (FnDecl NrDef "x2" (IdE (QualName {fromQualName = "x#01"})))
                (LetE (FnDecl NrDef "f23"
                       (LambdaE "z#14"
                        (CondE [(AppE (LambdaE "_5" (IdE (QualName {fromQualName = "true#"}))) (IdE (QualName {fromQualName = "z#14"})),
                                 LetE (FnDecl NrDef "z6" (IdE (QualName {fromQualName = "z#14"})))
                                 (AppE (AppE (IdE (QualName {fromQualName = "+"})) (IdE (QualName {fromQualName = "z6"}))) (IntE 1)))]
                         "f2")))
                 (LetE (FnDecl NrDef "y7" (IntE 0))
                  (AppE (IdE (QualName {fromQualName = "f23"})) (IdE (QualName {fromQualName = "y7"}))))))]
        "f1"))
