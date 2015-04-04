{-# LANGUAGE LambdaCase, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Stage.Expander where

import Data.Expr (DefnKw(..), Expr(..))
import qualified Data.Expr as Expr
import Data.QualName (QualName (..))
import qualified Data.PrettyString as PrettyString
import Data.Source
import qualified Parser
import qualified Pretty.Data.Expr as Pretty
import qualified Stage.Expander as Expander

deriving instance Eq DefnKw
deriving instance Eq Expr
deriving instance Eq QualName

expandTestFile :: String -> IO [Expr]
expandTestFile filename =
  do ModuleS _ _ srcs <- parseFile
     case concat `fmap` mapM Expander.expand srcs of
       Left err -> fail $ show err
       Right exprs -> return exprs

  where
    parseFile =
      do str <- readFile filename
         case Parser.parseFile filename str of
           Left err -> fail err
           Right src -> return src

expandSnippet :: Monad m => String -> m [Expr]
expandSnippet str =
  do case Parser.parseRepl "" str of
       Left err -> fail $ show err
       Right expr ->
         case Expander.expand expr of
           Left err -> fail $ show err
           Right exprs -> return exprs

data Actual = File String
            | Snippet String

-- edit: Use pretty strings instead
expect :: [Expr] -> Actual -> IO ()
expect expected actual =
  case actual of
    File filename -> expect' filename =<< expandTestFile filename
    Snippet snippet -> expect' ("(Snippet) " ++ snippet) =<< expandSnippet snippet
  where
    expect' filename exprs
      | expected == exprs = return ()
      | otherwise =
        fail $ "Expander" ++ "\n" ++
               "In: " ++ filename ++ "\n" ++
               "Expected: " ++ "\n" ++ PrettyString.toString (Pretty.docExprList expected) ++ "\n" ++
               "Actual: " ++ "\n" ++ PrettyString.toString (Pretty.docExprList exprs)

testExpander :: IO ()
testExpander =
  do expect [expectedSnippet1] $ Snippet "let not\n  @id = false\n  @  = true"
     expect [expected1] $ File "Test/TestData1.bsl"
     expect [expected2] $ File "Test/TestData2.bsl"
     expect [expected3] $ File "Test/TestData3.bsl"
     expect [expected4] $ File "Test/TestData4.bsl"
     expect [expected5] $ File "Test/TestData5.bsl"
     expect [expected6] $ File "Test/TestData6.bsl"
     expect expected7 $ File "Test/TestData7.bsl"
     expect [expected8] $ File "Test/TestData8.bsl"
     expect expected9 $ File "Test/TestData9.bsl"
  where
    expectedSnippet1 =
      FnDecl NrDef "not"
      (LambdaE "arg#0"
       (CondE [(AppE (Expr.idE "id") (Expr.idE "arg#0"), Expr.idE "false"),
               (AppE (LambdaE "_" (Expr.idE "true#")) (Expr.idE "arg#0"), Expr.idE "true")]
        "not"))

    expected1 =
      FnDecl NrDef "f1"
      (LambdaE "x#0"
       (LambdaE "y#1"
        (CondE
         [(CondE
           [(AppE (Expr.idE "isInt") (Expr.idE "x#0"),
             CondE [(AppE (Expr.idE "isInt") (Expr.idE "y#1"), Expr.idE "true#"),
                    (Expr.idE "true#", Expr.idE "false#")]
             "irrefutable 'and' pattern"),
            (Expr.idE "true#", Expr.idE "false#")]
           "irrefutable 'and' pattern",
           LetE (FnDecl NrDef "x" (Expr.idE "x#0"))
           (LetE (FnDecl NrDef "y" (Expr.idE "y#1"))
            (LetE
             (FnDecl NrDef "f2"
              (LambdaE "z#2"
               (CondE
                [(AppE (LambdaE "_" (Expr.idE "true#")) (Expr.idE "z#2"),
                  LetE (FnDecl NrDef "z" (Expr.idE "z#2"))
                  (AppE (AppE (Expr.idE "+") (Expr.idE "z")) (Expr.idE "y")))]
                "f2")))
             (AppE (Expr.idE "f2") (Expr.idE "x")))))]
         "f1")))

    expected2 =
      FnDecl NrDef "f1"
      (LambdaE "x#0"
       (CondE [(AppE (Expr.idE "isInt") (Expr.idE "x#0"),
                LetE (FnDecl NrDef "x" (Expr.idE "x#0"))
                (LetE
                 (FnDecl NrDef "f2"
                  (LambdaE "z#1"
                   (CondE [(AppE (LambdaE "_" (Expr.idE "true#")) (Expr.idE "z#1"),
                            LetE (FnDecl NrDef "z" (Expr.idE "z#1"))
                            (AppE (AppE (Expr.idE "+") (Expr.idE "z")) (IntE 1)))]
                    "f2")))
                 (LetE (FnDecl NrDef "y" (IntE 0))
                  (AppE (Expr.idE "f2") (Expr.idE "y")))))]
        "f1"))

    expected3 =
      FnDecl NrDef "f"
      (LambdaE "x#0"
       (LambdaE "y#1"
        (CondE
         [(CondE
           [(AppE (Expr.idE "isInt") (Expr.idE "x#0"),
             CondE [(AppE (Expr.idE "isInt") (Expr.idE "y#1"), Expr.idE "true#"),
                    (Expr.idE "true#", Expr.idE "false#")]
             "irrefutable 'and' pattern"),
            (Expr.idE "true#",
             Expr.idE "false#")]
           "irrefutable 'and' pattern",
           LetE (FnDecl NrDef "x" (Expr.idE "x#0"))
           (LetE (FnDecl NrDef "y" (Expr.idE "y#1"))
            (Expr.idE "true"))),
          (CondE [(AppE (LambdaE "_" (Expr.idE "true#")) (Expr.idE "x#0"),
                   CondE
                   [(AppE (LambdaE "_" (Expr.idE "true#")) (Expr.idE "y#1"), Expr.idE "true#"),
                    (Expr.idE "true#", Expr.idE "false#")]
                   "irrefutable 'and' pattern"),
                  (Expr.idE "true#",
                   Expr.idE "false#")]
           "irrefutable 'and' pattern",
           LetE (FnDecl NrDef "x" (Expr.idE "x#0"))
           (LetE (FnDecl NrDef "y" (Expr.idE "y#1"))
            (Expr.idE "false")))]
         "f")))

    expected4 =
      FnDecl Def "eq"
      (LambdaE "x#0"
       (LambdaE "y#1"
        (CondE
         [(CondE
           [(AppE (Expr.idE "isInt") (Expr.idE "x#0"),
             CondE [(AppE (Expr.idE "isInt") (Expr.idE "y#1"), Expr.idE "true#"),
                    (Expr.idE "true#", Expr.idE "false#")]
             "irrefutable 'and' pattern"),
            (Expr.idE "true#",
             Expr.idE "false#")]
           "irrefutable 'and' pattern",
           LetE (FnDecl NrDef "x" (Expr.idE "x#0"))
           (LetE (FnDecl NrDef "y" (Expr.idE "y#1"))
            (LetE
             (FnDecl Def "eqSeq"
              (LambdaE "arg#2"
               (LambdaE "arg#3"
                (CondE
                 [(CondE
                   [(AppE (AppE (Expr.idE "isTuple") (Expr.idE "null")) (Expr.idE "arg#2"),
                     CondE
                     [(AppE (AppE (Expr.idE "isTuple") (Expr.idE "null")) (Expr.idE "arg#3"), Expr.idE "true#"),
                      (Expr.idE "true#", Expr.idE "false#")]
                     "irrefutable 'and' pattern"),
                    (Expr.idE "true#", Expr.idE "false#")]
                   "irrefutable 'and' pattern",
                   Expr.idE "true"),
                  (CondE
                   [(AppE
                     (AppE (AppE (Expr.idE "isList") (LambdaE "_" (Expr.idE "true#"))) (LambdaE "_" (Expr.idE "true#"))) (Expr.idE "arg#2"),
                     CondE
                     [(AppE (AppE (AppE (Expr.idE "isList") (LambdaE "_" (Expr.idE "true#"))) (LambdaE "_" (Expr.idE "true#"))) (Expr.idE "arg#3"), Expr.idE "true#"),
                      (Expr.idE "true#", Expr.idE "false#")]
                     "irrefutable 'and' pattern"),
                    (Expr.idE "true#", Expr.idE "false#")]
                   "irrefutable 'and' pattern",
                   LetE (FnDecl NrDef "z" (AppE (Expr.idE "hd") (Expr.idE "arg#2")))
                   (LetE (FnDecl NrDef "zs" (AppE (Expr.idE "tl") (Expr.idE "arg#2")))
                    (LetE (FnDecl NrDef "w" (AppE (Expr.idE "hd") (Expr.idE "arg#3")))
                     (LetE (FnDecl NrDef "ws" (AppE (Expr.idE "tl") (Expr.idE "arg#3")))
                      (CondE
                       [(AppE (AppE (Expr.idE "eq") (Expr.idE "z")) (Expr.idE "w"),
                         CondE
                         [(AppE (AppE (Expr.idE "eqSeq") (Expr.idE "zs")) (Expr.idE "ws"), Expr.idE "true#"),
                          (Expr.idE "true#", Expr.idE "false#")]
                         "irrefutable 'and' pattern"),
                        (Expr.idE "true#",
                         Expr.idE "false#")]
                       "irrefutable 'and' pattern"))))),
                  (CondE
                   [(AppE (LambdaE "_" (Expr.idE "true#")) (Expr.idE "arg#2"),
                     CondE [(AppE (LambdaE "_" (Expr.idE "true#")) (Expr.idE "arg#3"), Expr.idE "true#"),
                            (Expr.idE "true#", Expr.idE "false#")]
                     "irrefutable 'and' pattern"),
                    (Expr.idE "true#",
                     Expr.idE "false#")]
                   "irrefutable 'and' pattern",
                   Expr.idE "false")]
                 "eqSeq"))))
             (AppE (AppE (Expr.idE "eqInt") (Expr.idE "x")) (Expr.idE "y"))))),
          (CondE
           [(AppE (LambdaE "_" (Expr.idE "true#")) (Expr.idE "x#0"),
             CondE
             [(AppE (LambdaE "_" (Expr.idE "true#")) (Expr.idE "y#1"), Expr.idE "true#"),
              (Expr.idE "true#", Expr.idE "false#")]
             "irrefutable 'and' pattern"),
            (Expr.idE "true#",
             Expr.idE "false#")]
           "irrefutable 'and' pattern",
           LetE (FnDecl NrDef "x" (Expr.idE "x#0"))
           (LetE (FnDecl NrDef "y" (Expr.idE "y#1"))
            (LetE
             (FnDecl Def "eqSeq"
              (LambdaE "arg#4"
               (LambdaE "arg#5"
                (CondE
                 [(CondE
                   [(AppE (AppE (Expr.idE "isTuple") (Expr.idE "null")) (Expr.idE "arg#4"),
                     CondE
                     [(AppE (AppE (Expr.idE "isTuple") (Expr.idE "null")) (Expr.idE "arg#5"), Expr.idE "true#"),
                      (Expr.idE "true#", Expr.idE "false#")]
                     "irrefutable 'and' pattern"),
                    (Expr.idE "true#",
                     Expr.idE "false#")]
                   "irrefutable 'and' pattern",
                   Expr.idE "true"),
                  (CondE
                   [(AppE (AppE (AppE (Expr.idE "isList") (LambdaE "_" (Expr.idE "true#"))) (LambdaE "_" (Expr.idE "true#"))) (Expr.idE "arg#4"),
                     CondE
                     [(AppE (AppE (AppE (Expr.idE "isList") (LambdaE "_" (Expr.idE "true#"))) (LambdaE "_" (Expr.idE "true#"))) (Expr.idE "arg#5"), Expr.idE "true#"),
                      (Expr.idE "true#", Expr.idE "false#")]
                     "irrefutable 'and' pattern"),
                    (Expr.idE "true#",
                     Expr.idE "false#")]
                   "irrefutable 'and' pattern",
                   LetE
                   (FnDecl NrDef "z" (AppE (Expr.idE "hd") (Expr.idE "arg#4")))
                   (LetE (FnDecl NrDef "zs" (AppE (Expr.idE "tl") (Expr.idE "arg#4")))
                    (LetE (FnDecl NrDef "w" (AppE (Expr.idE "hd") (Expr.idE "arg#5")))
                     (LetE (FnDecl NrDef "ws" (AppE (Expr.idE "tl") (Expr.idE "arg#5")))
                      (CondE
                       [(AppE (AppE (Expr.idE "eq") (Expr.idE "z")) (Expr.idE "w"),
                         CondE
                         [(AppE (AppE (Expr.idE "eqSeq") (Expr.idE "zs")) (Expr.idE "ws"), Expr.idE "true#"),
                          (Expr.idE "true#", Expr.idE "false#")]
                         "irrefutable 'and' pattern"),
                        (Expr.idE "true#", Expr.idE "false#")]
                       "irrefutable 'and' pattern"))))),
                  (CondE
                   [(AppE (LambdaE "_" (Expr.idE "true#")) (Expr.idE "arg#4"),
                     CondE
                     [(AppE (LambdaE "_" (Expr.idE "true#")) (Expr.idE "arg#5"), Expr.idE "true#"),
                      (Expr.idE "true#", Expr.idE "false#")]
                     "irrefutable 'and' pattern"),
                    (Expr.idE "true#",
                     Expr.idE "false#")]
                   "irrefutable 'and' pattern",
                   Expr.idE "false")]
                 "eqSeq"))))
             (AppE (AppE (Expr.idE "eqSeq") (Expr.idE "x")) (Expr.idE "y")))))]
         "eq")))

    expected5 =
      FnDecl Def "isString"
      (LambdaE "arg#0"
       (CondE
        [(AppE (AppE (Expr.idE "isTuple") (Expr.idE "null")) (Expr.idE "arg#0"), Expr.idE "true"),
         (AppE (AppE (AppE (Expr.idE "isList") (Expr.idE "isChar")) (Expr.idE "isString")) (Expr.idE "arg#0"), Expr.idE "true"),
         (AppE (LambdaE "_" (Expr.idE "true#")) (Expr.idE "arg#0"), Expr.idE "false")]
        "isString"))

    expected6 =
      FnDecl NrDef "f"
      (LambdaE "n#0"
       (CondE
        [(AppE (LambdaE "_" (Expr.idE "true#")) (Expr.idE "n#0"),
          LetE (FnDecl NrDef "n" (Expr.idE "n#0"))
          (LetE (FnDecl NrDef "res#1"
                 (AppE (AppE (Expr.idE "cons") (IntE 1))
                  (AppE (AppE (Expr.idE "cons") (IntE 2))
                   (Expr.idE "null"))))
           (LetE (FnDecl NrDef "x"
                  (AppE (Expr.idE "hd")
                   (Expr.idE "res#1")))
            (LetE (FnDecl NrDef "y"
                   (AppE (Expr.idE "hd")
                    (AppE (Expr.idE "tl")
                     (Expr.idE "res#1"))))
             (AppE (AppE (Expr.idE "case") (Expr.idE "n"))
              (LambdaE "arg#2"
               (CondE
                [(AppE (AppE (Expr.idE ">") (IntE 1)) (Expr.idE "arg#2"), Expr.idE "x"),
                 (AppE (LambdaE "_" (Expr.idE "true#")) (Expr.idE "arg#2"), Expr.idE "y")]
                "lambda")))))))]
        "f"))

    expected7 =
      [FnDecl NrDef "res#0"
       (AppE (AppE (Expr.idE "cons") (IntE 1))
        (AppE (AppE (Expr.idE "cons") (IntE 2))
         (Expr.idE "null"))),
       FnDecl NrDef "x"
       (AppE (Expr.idE "hd")
        (Expr.idE "res#0")),
       FnDecl NrDef "y"
       (AppE (Expr.idE "hd")
        (AppE (Expr.idE "tl")
         (Expr.idE "res#0")))]

    expected8 =
      FnDecl NrDef "f8"
      (LambdaE "arg#0"
       (CondE [(AppE (Expr.idE "isApple") (Expr.idE "arg#0"),
                LetE
                (FnDecl NrDef "x" (AppE (Expr.idE "unCons#") (Expr.idE "arg#0")))
                (IntE 0))]
        "f8"))

    expected9 =
      [FnDecl NrDef "isApple" (AppE (Expr.idE "isCons#") (AppE (Expr.idE "link#") (Expr.stringE "Apple"))),
       FnDecl NrDef "Apple"
       (LambdaE "x#0"
        (CondE [(AppE (Expr.idE "isInt") (Expr.idE "x#0"),
                 LetE (FnDecl NrDef "x" (Expr.idE "x#0"))
                 (AppE
                  (AppE (Expr.idE "mkCons#") (AppE (Expr.idE "link#") (Expr.stringE "Apple")))
                  (Expr.idE "x")))]
         "Apple")),
       FnDecl NrDef "isFruit" (Expr.idE "isApple"),

       FnDecl NrDef "isOrange" (AppE (Expr.idE "isCons#") (AppE (Expr.idE "link#") (Expr.stringE "Orange"))),
       FnDecl NrDef "Orange"
       (LambdaE "x#0"
        (CondE [(AppE (LambdaE "_" (Expr.idE "true#")) (Expr.idE "x#0"),
                 LetE (FnDecl NrDef "x" (Expr.idE "x#0"))
                  (AppE
                   (AppE (Expr.idE "mkCons#") (AppE (Expr.idE "link#") (Expr.stringE "Orange")))
                   (Expr.idE "x")))]
         "Orange")),
       FnDecl NrDef "isMoreFruit" (Expr.idE "isOrange"),

       FnDecl NrDef "isBanana" (AppE (Expr.idE "isCons#") (AppE (Expr.idE "link#") (Expr.stringE "Banana"))),
       FnDecl NrDef "Banana"
       (LambdaE "arg#0#1"
        (CondE [(AppE (Expr.idE "isInt") (Expr.idE "arg#0#1"),
                 LetE (FnDecl NrDef "arg#0" (Expr.idE "arg#0#1"))
                 (AppE
                  (AppE (Expr.idE "mkCons#") (AppE (Expr.idE "link#") (Expr.stringE "Banana")))
                  (Expr.idE "arg#0")))]
         "Banana")),
       FnDecl NrDef "isEvenMoreFruit" (Expr.idE "isBanana")]
