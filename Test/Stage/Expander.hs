{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Stage.Expander where

import Data.Expr
import Data.QualName (QualName (..))
import qualified Data.PrettyString as PrettyString
import Data.Source
import qualified Parser
import qualified Pretty.Data.Expr as Pretty
import qualified Stage.Expander as Expander

deriving instance Eq DefnKw
deriving instance Eq Expr
deriving instance Eq QualName

expandTestFile :: String -> IO Expr
expandTestFile filename =
  do ModuleS _ _ [src] <- parseFile
     case Expander.expand src of
       Left err -> fail $ show err
       Right exprs -> return (head exprs)
  where
    parseFile =
      do str <- readFile filename
         case Parser.parseFile filename str of
           Left err -> fail err
           Right src -> return src

expandSnippet :: Monad m => String -> m Expr
expandSnippet str =
  do case Parser.parseRepl "" str of
       Left err -> fail $ show err
       Right expr ->
         case Expander.expand expr of
           Left err -> fail $ show err
           Right exprs -> return (head exprs)

data Actual = File String
            | Snippet String

-- edit: Use pretty strings instead
expect :: Expr -> Actual -> IO ()
expect expected actual =
  case actual of
    File filename -> expect' filename =<< expandTestFile filename
    Snippet snippet -> expect' snippet =<< expandSnippet snippet
  where
    expect' filename expr
      | expected == expr = return ()
      | otherwise =
        fail $ "Expander" ++ "\n" ++
               "In: " ++ filename ++ "\n" ++
               "Expected: " ++ "\n" ++ PrettyString.toString (Pretty.docExpr Pretty.ExpDocT expected) ++ "\n" ++
               "Expr: " ++ "\n" ++ PrettyString.toString (Pretty.docExpr Pretty.ExpDocT expr)

testExpander :: IO ()
testExpander =
  do expect expectedSnippet1 $ Snippet "def not id = false | @  = true"
     expect expected1 $ File "Test/TestData1.bsl"
     expect expected2 $ File "Test/TestData2.bsl"
     expect expected3 $ File "Test/TestData3.bsl"
     expect expected4 $ File "Test/TestData4.bsl"
     -- expect expected5 $ File "Test/TestData5.bsl"
  where
    expectedSnippet1 =
      FnDecl NrDef "not"
      (LambdaE "arg#0"
       (CondE [(AppE (IdE (QualName {fromQualName = "id"})) (IdE (QualName {fromQualName = "arg#0"})),
                IdE (QualName {fromQualName = "false"})),
               (AppE (LambdaE "_" (IdE (QualName {fromQualName = "true#"}))) (IdE (QualName {fromQualName = "arg#0"})),
                IdE (QualName {fromQualName = "true"}))]
        "not"))

    expected1 =
      FnDecl NrDef "f1"
      (LambdaE "x#0"
       (LambdaE "y#1"
        (CondE
         [(CondE
           [(AppE (IdE (QualName {fromQualName = "isInt"})) (IdE (QualName {fromQualName = "x#0"})),
             CondE [(AppE (IdE (QualName {fromQualName = "isInt"})) (IdE (QualName {fromQualName = "y#1"})),
                     IdE (QualName {fromQualName = "true#"})),
                    (IdE (QualName {fromQualName = "true#"}),
                     IdE (QualName {fromQualName = "false#"}))]
             "irrefutable 'and' pattern"),
            (IdE (QualName {fromQualName = "true#"}),
             IdE (QualName {fromQualName = "false#"}))]
           "irrefutable 'and' pattern",
           LetE (FnDecl NrDef "x" (IdE (QualName {fromQualName = "x#0"})))
           (LetE (FnDecl NrDef "y" (IdE (QualName {fromQualName = "y#1"})))
            (LetE
             (FnDecl NrDef "f2"
              (LambdaE "z#2"
               (CondE
                [(AppE (LambdaE "_" (IdE (QualName {fromQualName = "true#"}))) (IdE (QualName {fromQualName = "z#2"})),
                  LetE (FnDecl NrDef "z" (IdE (QualName {fromQualName = "z#2"})))
                  (AppE (AppE (IdE (QualName {fromQualName = "+"})) (IdE (QualName {fromQualName = "z"}))) (IdE (QualName {fromQualName = "y"}))))]
                "f2")))
             (AppE (IdE (QualName {fromQualName = "f2"})) (IdE (QualName {fromQualName = "x"}))))))]
         "f1")))

    expected2 =
      FnDecl NrDef "f1"
      (LambdaE "x#0"
       (CondE [(AppE (IdE (QualName {fromQualName = "isInt"})) (IdE (QualName {fromQualName = "x#0"})),
                LetE (FnDecl NrDef "x" (IdE (QualName {fromQualName = "x#0"})))
                (LetE
                 (FnDecl NrDef "f2"
                  (LambdaE "z#1"
                   (CondE [(AppE (LambdaE "_" (IdE (QualName {fromQualName = "true#"}))) (IdE (QualName {fromQualName = "z#1"})),
                            LetE (FnDecl NrDef "z" (IdE (QualName {fromQualName = "z#1"})))
                            (AppE (AppE (IdE (QualName {fromQualName = "+"})) (IdE (QualName {fromQualName = "z"}))) (IntE 1)))]
                    "f2")))
                 (LetE (FnDecl NrDef "y" (IntE 0))
                  (AppE (IdE (QualName {fromQualName = "f2"})) (IdE (QualName {fromQualName = "y"}))))))]
        "f1"))

    expected3 =
      FnDecl NrDef "f"
      (LambdaE "x#0"
       (LambdaE "y#1"
        (CondE
         [(CondE
           [(AppE (IdE (QualName {fromQualName = "isInt"})) (IdE (QualName {fromQualName = "x#0"})),
             CondE [(AppE (IdE (QualName {fromQualName = "isInt"})) (IdE (QualName {fromQualName = "y#1"})),
                     IdE (QualName {fromQualName = "true#"})),
                    (IdE (QualName {fromQualName = "true#"}),
                     IdE (QualName {fromQualName = "false#"}))]
             "irrefutable 'and' pattern"),
            (IdE (QualName {fromQualName = "true#"}),
             IdE (QualName {fromQualName = "false#"}))]
           "irrefutable 'and' pattern",
           LetE (FnDecl NrDef "x" (IdE (QualName {fromQualName = "x#0"})))
           (LetE (FnDecl NrDef "y" (IdE (QualName {fromQualName = "y#1"})))
            (IdE (QualName {fromQualName = "true"})))),
          (CondE [(AppE (LambdaE "_" (IdE (QualName {fromQualName = "true#"}))) (IdE (QualName {fromQualName = "x#0"})),
                   CondE
                   [(AppE (LambdaE "_" (IdE (QualName {fromQualName = "true#"}))) (IdE (QualName {fromQualName = "y#1"})),
                     IdE (QualName {fromQualName = "true#"})),
                    (IdE (QualName {fromQualName = "true#"}),
                     IdE (QualName {fromQualName = "false#"}))]
                   "irrefutable 'and' pattern"),
                  (IdE (QualName {fromQualName = "true#"}),
                   IdE (QualName {fromQualName = "false#"}))]
           "irrefutable 'and' pattern",
           LetE (FnDecl NrDef "x" (IdE (QualName {fromQualName = "x#0"})))
           (LetE (FnDecl NrDef "y" (IdE (QualName {fromQualName = "y#1"})))
            (IdE (QualName {fromQualName = "false"}))))]
         "f")))

    expected4 =
      FnDecl Def "eq"
      (LambdaE "x#0"
       (LambdaE "y#1"
        (CondE
         [(CondE
           [(AppE (IdE (QualName {fromQualName = "isInt"})) (IdE (QualName {fromQualName = "x#0"})),
             CondE [(AppE (IdE (QualName {fromQualName = "isInt"})) (IdE (QualName {fromQualName = "y#1"})),
                     IdE (QualName {fromQualName = "true#"})),
                    (IdE (QualName {fromQualName = "true#"}),
                     IdE (QualName {fromQualName = "false#"}))]
             "irrefutable 'and' pattern"),
            (IdE (QualName {fromQualName = "true#"}),
             IdE (QualName {fromQualName = "false#"}))]
           "irrefutable 'and' pattern",
           LetE (FnDecl NrDef "x" (IdE (QualName {fromQualName = "x#0"})))
           (LetE (FnDecl NrDef "y" (IdE (QualName {fromQualName = "y#1"})))
            (LetE
             (FnDecl Def "eqSeq"
              (LambdaE "arg#2"
               (LambdaE "arg#3"
                (CondE
                 [(CondE
                   [(AppE (AppE (IdE (QualName {fromQualName = "isTuple"})) (IdE (QualName {fromQualName = "null"}))) (IdE (QualName {fromQualName = "arg#2"})),
                     CondE
                     [(AppE (AppE (IdE (QualName {fromQualName = "isTuple"})) (IdE (QualName {fromQualName = "null"}))) (IdE (QualName {fromQualName = "arg#3"})),
                       IdE (QualName {fromQualName = "true#"})),
                      (IdE (QualName {fromQualName = "true#"}),
                       IdE (QualName {fromQualName = "false#"}))]
                     "irrefutable 'and' pattern"),
                    (IdE (QualName {fromQualName = "true#"}),
                     IdE (QualName {fromQualName = "false#"}))]
                   "irrefutable 'and' pattern",
                   IdE (QualName {fromQualName = "true"})),
                  (CondE
                   [(AppE
                     (AppE (AppE (IdE (QualName {fromQualName = "isList"})) (LambdaE "_" (IdE (QualName {fromQualName = "true#"})))) (LambdaE "_" (IdE (QualName {fromQualName = "true#"})))) (IdE (QualName {fromQualName = "arg#2"})),
                     CondE
                     [(AppE (AppE (AppE (IdE (QualName {fromQualName = "isList"})) (LambdaE "_" (IdE (QualName {fromQualName = "true#"})))) (LambdaE "_" (IdE (QualName {fromQualName = "true#"})))) (IdE (QualName {fromQualName = "arg#3"})),
                       IdE (QualName {fromQualName = "true#"})),
                      (IdE (QualName {fromQualName = "true#"}),
                       IdE (QualName {fromQualName = "false#"}))]
                     "irrefutable 'and' pattern"),
                    (IdE (QualName {fromQualName = "true#"}),
                     IdE (QualName {fromQualName = "false#"}))]
                   "irrefutable 'and' pattern",
                   LetE (FnDecl NrDef "z" (AppE (IdE (QualName {fromQualName = "hd"})) (IdE (QualName {fromQualName = "arg#2"}))))
                   (LetE (FnDecl NrDef "zs" (AppE (IdE (QualName {fromQualName = "tl"})) (IdE (QualName {fromQualName = "arg#2"}))))
                    (LetE (FnDecl NrDef "w" (AppE (IdE (QualName {fromQualName = "hd"})) (IdE (QualName {fromQualName = "arg#3"}))))
                     (LetE (FnDecl NrDef "ws" (AppE (IdE (QualName {fromQualName = "tl"})) (IdE (QualName {fromQualName = "arg#3"}))))
                      (CondE
                       [(AppE (AppE (IdE (QualName {fromQualName = "eq"})) (IdE (QualName {fromQualName = "z"}))) (IdE (QualName {fromQualName = "w"})),
                         CondE
                         [(AppE (AppE (IdE (QualName {fromQualName = "eqSeq"})) (IdE (QualName {fromQualName = "zs"}))) (IdE (QualName {fromQualName = "ws"})),
                           IdE (QualName {fromQualName = "true#"})),
                          (IdE (QualName {fromQualName = "true#"}),
                           IdE (QualName {fromQualName = "false#"}))]
                         "irrefutable 'and' pattern"),
                        (IdE (QualName {fromQualName = "true#"}),
                         IdE (QualName {fromQualName = "false#"}))]
                       "irrefutable 'and' pattern"))))),
                  (CondE
                   [(AppE (LambdaE "_" (IdE (QualName {fromQualName = "true#"}))) (IdE (QualName {fromQualName = "arg#2"})),
                     CondE [(AppE (LambdaE "_" (IdE (QualName {fromQualName = "true#"}))) (IdE (QualName {fromQualName = "arg#3"})),
                             IdE (QualName {fromQualName = "true#"})),
                            (IdE (QualName {fromQualName = "true#"}),
                             IdE (QualName {fromQualName = "false#"}))]
                     "irrefutable 'and' pattern"),
                    (IdE (QualName {fromQualName = "true#"}),
                     IdE (QualName {fromQualName = "false#"}))]
                   "irrefutable 'and' pattern",
                   IdE (QualName {fromQualName = "false"}))]
                 "eqSeq"))))
             (AppE (AppE (IdE (QualName {fromQualName = "eqInt"})) (IdE (QualName {fromQualName = "x"}))) (IdE (QualName {fromQualName = "y"})))))),
          (CondE
           [(AppE (LambdaE "_" (IdE (QualName {fromQualName = "true#"}))) (IdE (QualName {fromQualName = "x#0"})),
             CondE
             [(AppE (LambdaE "_" (IdE (QualName {fromQualName = "true#"}))) (IdE (QualName {fromQualName = "y#1"})),
               IdE (QualName {fromQualName = "true#"})),
              (IdE (QualName {fromQualName = "true#"}),
               IdE (QualName {fromQualName = "false#"}))]
             "irrefutable 'and' pattern"),
            (IdE (QualName {fromQualName = "true#"}),
             IdE (QualName {fromQualName = "false#"}))]
           "irrefutable 'and' pattern",
           LetE (FnDecl NrDef "x" (IdE (QualName {fromQualName = "x#0"})))
           (LetE (FnDecl NrDef "y" (IdE (QualName {fromQualName = "y#1"})))
            (LetE
             (FnDecl Def "eqSeq"
              (LambdaE "arg#4"
               (LambdaE "arg#5"
                (CondE
                 [(CondE
                   [(AppE (AppE (IdE (QualName {fromQualName = "isTuple"})) (IdE (QualName {fromQualName = "null"}))) (IdE (QualName {fromQualName = "arg#4"})),
                     CondE
                     [(AppE (AppE (IdE (QualName {fromQualName = "isTuple"})) (IdE (QualName {fromQualName = "null"}))) (IdE (QualName {fromQualName = "arg#5"})),
                       IdE (QualName {fromQualName = "true#"})),
                      (IdE (QualName {fromQualName = "true#"}),
                       IdE (QualName {fromQualName = "false#"}))]
                     "irrefutable 'and' pattern"),
                    (IdE (QualName {fromQualName = "true#"}),
                     IdE (QualName {fromQualName = "false#"}))]
                   "irrefutable 'and' pattern",
                   IdE (QualName {fromQualName = "true"})),
                  (CondE
                   [(AppE (AppE (AppE (IdE (QualName {fromQualName = "isList"})) (LambdaE "_" (IdE (QualName {fromQualName = "true#"})))) (LambdaE "_" (IdE (QualName {fromQualName = "true#"})))) (IdE (QualName {fromQualName = "arg#4"})),
                     CondE
                     [(AppE (AppE (AppE (IdE (QualName {fromQualName = "isList"})) (LambdaE "_" (IdE (QualName {fromQualName = "true#"})))) (LambdaE "_" (IdE (QualName {fromQualName = "true#"})))) (IdE (QualName {fromQualName = "arg#5"})),
                       IdE (QualName {fromQualName = "true#"})),
                      (IdE (QualName {fromQualName = "true#"}),
                       IdE (QualName {fromQualName = "false#"}))]
                     "irrefutable 'and' pattern"),
                    (IdE (QualName {fromQualName = "true#"}),
                     IdE (QualName {fromQualName = "false#"}))]
                   "irrefutable 'and' pattern",
                   LetE
                   (FnDecl NrDef "z" (AppE (IdE (QualName {fromQualName = "hd"})) (IdE (QualName {fromQualName = "arg#4"}))))
                   (LetE (FnDecl NrDef "zs" (AppE (IdE (QualName {fromQualName = "tl"})) (IdE (QualName {fromQualName = "arg#4"}))))
                    (LetE (FnDecl NrDef "w" (AppE (IdE (QualName {fromQualName = "hd"})) (IdE (QualName {fromQualName = "arg#5"}))))
                     (LetE (FnDecl NrDef "ws" (AppE (IdE (QualName {fromQualName = "tl"})) (IdE (QualName {fromQualName = "arg#5"}))))
                      (CondE
                       [(AppE (AppE (IdE (QualName {fromQualName = "eq"})) (IdE (QualName {fromQualName = "z"}))) (IdE (QualName {fromQualName = "w"})),
                         CondE
                         [(AppE (AppE (IdE (QualName {fromQualName = "eqSeq"})) (IdE (QualName {fromQualName = "zs"}))) (IdE (QualName {fromQualName = "ws"})),
                           IdE (QualName {fromQualName = "true#"})),
                          (IdE (QualName {fromQualName = "true#"}),
                           IdE (QualName {fromQualName = "false#"}))]
                         "irrefutable 'and' pattern"),
                        (IdE (QualName {fromQualName = "true#"}),
                         IdE (QualName {fromQualName = "false#"}))]
                       "irrefutable 'and' pattern"))))),
                  (CondE
                   [(AppE (LambdaE "_" (IdE (QualName {fromQualName = "true#"}))) (IdE (QualName {fromQualName = "arg#4"})),
                     CondE
                     [(AppE (LambdaE "_" (IdE (QualName {fromQualName = "true#"}))) (IdE (QualName {fromQualName = "arg#5"})),
                       IdE (QualName {fromQualName = "true#"})),
                      (IdE (QualName {fromQualName = "true#"}),
                       IdE (QualName {fromQualName = "false#"}))]
                     "irrefutable 'and' pattern"),
                    (IdE (QualName {fromQualName = "true#"}),
                     IdE (QualName {fromQualName = "false#"}))]
                   "irrefutable 'and' pattern",
                   IdE (QualName {fromQualName = "false"}))]
                 "eqSeq"))))
             (AppE (AppE (IdE (QualName {fromQualName = "eqSeq"})) (IdE (QualName {fromQualName = "x"}))) (IdE (QualName {fromQualName = "y"}))))))]
         "eq")))

    expected5 =
      FnDecl Def "isString"
      (LambdaE "arg#0"
       (CondE
        [(AppE (AppE (IdE (QualName {fromQualName = "isTuple"})) (IdE (QualName {fromQualName = "null"}))) (IdE (QualName {fromQualName = "arg#0"})),
          IdE (QualName {fromQualName = "true"})),
         (AppE (AppE (AppE (IdE (QualName {fromQualName = "isList"})) (IdE (QualName {fromQualName = "isChar"}))) (IdE (QualName {fromQualName = "isString"}))) (IdE (QualName {fromQualName = "arg#0"})),
          IdE (QualName {fromQualName = "true"})),
         (AppE (LambdaE "_" (IdE (QualName {fromQualName = "true#"}))) (IdE (QualName {fromQualName = "arg#0"})),
          IdE (QualName {fromQualName = "false"}))]
        "isString"))
