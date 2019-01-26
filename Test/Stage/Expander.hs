{-# LANGUAGE LambdaCase, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Stage.Expander where

import Data.Expr (DefnKw(..), Expr(..))
import qualified Data.Expr as Expr
import Data.Literal (Literal(..))
import qualified Data.Name as Name
import qualified Data.PrettyString as PrettyString
import Data.Source
import qualified Parser
import qualified Pretty.Data.Expr as Pretty
import qualified Stage.Expander as Expander

deriving instance Eq DefnKw
deriving instance Eq Literal
deriving instance Eq Expr

expandTestFile :: String -> IO [Expr]
expandTestFile filename = expandFile
  where
    parseFile =
      do str <- readFile filename
         case Parser.parseFile (Name.untyped filename) str of
           Left err -> fail err
           Right src -> return src

    expandFile =
      do ModuleS _ _ srcs <- parseFile
         case concat `fmap` mapM Expander.expand srcs of
           Left err -> fail $ show err
           Right exprs -> return exprs

expandSnippet :: Monad m => String -> m [Expr]
expandSnippet str =
  do case Parser.parseRepl Name.empty str of
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
     expect [expected12] $ File "Test/TestData12.bsl"
     expect [expectedTuple] $ File "Test/Tuple.bsl"
     expect expectedUnit $ File "Test/Unit.bsl"
     expect expectedVariant $ File "Test/Variant.bsl"
  where
    expectedSnippet1 =
      FnDecl NrDef (Name.untyped "not")
      (LambdaE (Name.untyped "arg#0")
       (CondE [(AppE (Expr.idE "id") (Expr.idE "arg#0"), Expr.idE "false"),
               (AppE (LambdaE (Name.untyped "_") (Expr.idE "true#")) (Expr.idE "arg#0"), Expr.idE "true")]
        "not"))

    expected1 =
      FnDecl NrDef (Name.untyped "f1")
      (LambdaE (Name.untyped "x#0")
       (LambdaE (Name.untyped "y#1")
        (CondE
         [(CondE
           [(AppE (Expr.idE "isInt") (Expr.idE "x#0"),
             CondE [(AppE (Expr.idE "isInt") (Expr.idE "y#1"), Expr.idE "true#"),
                    (Expr.idE "true#", Expr.idE "false#")]
             "irrefutable 'and' pattern"),
            (Expr.idE "true#", Expr.idE "false#")]
           "irrefutable 'and' pattern",
           LetE (FnDecl NrDef (Name.untyped "x") (Expr.idE "x#0"))
           (LetE (FnDecl NrDef (Name.untyped "y") (Expr.idE "y#1"))
            (LetE
             (FnDecl NrDef (Name.untyped "f2")
              (LambdaE (Name.untyped "z#2")
               (CondE
                [(AppE (LambdaE (Name.untyped "_") (Expr.idE "true#")) (Expr.idE "z#2"),
                  LetE (FnDecl NrDef (Name.untyped "z") (Expr.idE "z#2"))
                  (AppE (AppE (Expr.idE "+") (Expr.idE "z")) (Expr.idE "y")))]
                "f2")))
             (AppE (Expr.idE "f2") (Expr.idE "x")))))]
         "f1")))

    expected2 =
      FnDecl NrDef (Name.untyped "f1")
      (LambdaE (Name.untyped "x#0")
       (CondE [(AppE (Expr.idE "isInt") (Expr.idE "x#0"),
                LetE (FnDecl NrDef (Name.untyped "x") (Expr.idE "x#0"))
                (LetE
                 (FnDecl NrDef (Name.untyped "f2")
                  (LambdaE (Name.untyped "z#1")
                   (CondE [(AppE (LambdaE (Name.untyped "_") (Expr.idE "true#")) (Expr.idE "z#1"),
                            LetE (FnDecl NrDef (Name.untyped "z") (Expr.idE "z#1"))
                            (AppE (AppE (Expr.idE "+") (Expr.idE "z")) (Expr.intE 1)))]
                    "f2")))
                 (LetE (FnDecl NrDef (Name.untyped "y") (Expr.intE 0))
                  (AppE (Expr.idE "f2") (Expr.idE "y")))))]
        "f1"))

    expected3 =
      FnDecl NrDef (Name.untyped "f")
      (LambdaE (Name.untyped "x#0")
       (LambdaE (Name.untyped "y#1")
        (CondE
         [(CondE
           [(AppE (Expr.idE "isInt") (Expr.idE "x#0"),
             CondE [(AppE (Expr.idE "isInt") (Expr.idE "y#1"), Expr.idE "true#"),
                    (Expr.idE "true#", Expr.idE "false#")]
             "irrefutable 'and' pattern"),
            (Expr.idE "true#",
             Expr.idE "false#")]
           "irrefutable 'and' pattern",
           LetE (FnDecl NrDef (Name.untyped "x") (Expr.idE "x#0"))
           (LetE (FnDecl NrDef (Name.untyped "y") (Expr.idE "y#1"))
            (Expr.idE "true"))),
          (CondE [(AppE (LambdaE (Name.untyped "_") (Expr.idE "true#")) (Expr.idE "x#0"),
                   CondE
                   [(AppE (LambdaE (Name.untyped "_") (Expr.idE "true#")) (Expr.idE "y#1"), Expr.idE "true#"),
                    (Expr.idE "true#", Expr.idE "false#")]
                   "irrefutable 'and' pattern"),
                  (Expr.idE "true#",
                   Expr.idE "false#")]
           "irrefutable 'and' pattern",
           LetE (FnDecl NrDef (Name.untyped "x") (Expr.idE "x#0"))
           (LetE (FnDecl NrDef (Name.untyped "y") (Expr.idE "y#1"))
            (Expr.idE "false")))]
         "f")))

    expected4 =
      FnDecl Def (Name.untyped "eq")
      (LambdaE (Name.untyped "x#0")
       (LambdaE (Name.untyped "y#1")
        (CondE
         [(CondE
           [(AppE (Expr.idE "isInt") (Expr.idE "x#0"),
             CondE [(AppE (Expr.idE "isInt") (Expr.idE "y#1"), Expr.idE "true#"),
                    (Expr.idE "true#", Expr.idE "false#")]
             "irrefutable 'and' pattern"),
            (Expr.idE "true#",
             Expr.idE "false#")]
           "irrefutable 'and' pattern",
           LetE (FnDecl NrDef (Name.untyped "x") (Expr.idE "x#0"))
           (LetE (FnDecl NrDef (Name.untyped "y") (Expr.idE "y#1"))
            (LetE
             (FnDecl Def (Name.untyped "eqSeq")
              (LambdaE (Name.untyped "arg#2")
               (LambdaE (Name.untyped "arg#3")
                (CondE
                 [(CondE
                   [(AppE (AppE (Expr.idE "isList") (Expr.idE "null")) (Expr.idE "arg#2"),
                     CondE
                     [(AppE (AppE (Expr.idE "isList") (Expr.idE "null")) (Expr.idE "arg#3"), Expr.idE "true#"),
                      (Expr.idE "true#", Expr.idE "false#")]
                     "irrefutable 'and' pattern"),
                    (Expr.idE "true#", Expr.idE "false#")]
                   "irrefutable 'and' pattern",
                   Expr.idE "true"),
                  (CondE
                   [(AppE
                     (AppE (AppE (Expr.idE "isHeadTail") (LambdaE (Name.untyped "_") (Expr.idE "true#"))) (LambdaE (Name.untyped "_") (Expr.idE "true#"))) (Expr.idE "arg#2"),
                     CondE
                     [(AppE (AppE (AppE (Expr.idE "isHeadTail") (LambdaE (Name.untyped "_") (Expr.idE "true#"))) (LambdaE (Name.untyped "_") (Expr.idE "true#"))) (Expr.idE "arg#3"), Expr.idE "true#"),
                      (Expr.idE "true#", Expr.idE "false#")]
                     "irrefutable 'and' pattern"),
                    (Expr.idE "true#", Expr.idE "false#")]
                   "irrefutable 'and' pattern",
                   LetE (FnDecl NrDef (Name.untyped "z") (AppE (Expr.idE "hd") (Expr.idE "arg#2")))
                   (LetE (FnDecl NrDef (Name.untyped "zs") (AppE (Expr.idE "tl") (Expr.idE "arg#2")))
                    (LetE (FnDecl NrDef (Name.untyped "w") (AppE (Expr.idE "hd") (Expr.idE "arg#3")))
                     (LetE (FnDecl NrDef (Name.untyped "ws") (AppE (Expr.idE "tl") (Expr.idE "arg#3")))
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
                   [(AppE (LambdaE (Name.untyped "_") (Expr.idE "true#")) (Expr.idE "arg#2"),
                     CondE [(AppE (LambdaE (Name.untyped "_") (Expr.idE "true#")) (Expr.idE "arg#3"), Expr.idE "true#"),
                            (Expr.idE "true#", Expr.idE "false#")]
                     "irrefutable 'and' pattern"),
                    (Expr.idE "true#",
                     Expr.idE "false#")]
                   "irrefutable 'and' pattern",
                   Expr.idE "false")]
                 "eqSeq"))))
             (AppE (AppE (Expr.idE "eqInt") (Expr.idE "x")) (Expr.idE "y"))))),
          (CondE
           [(AppE (LambdaE (Name.untyped "_") (Expr.idE "true#")) (Expr.idE "x#0"),
             CondE
             [(AppE (LambdaE (Name.untyped "_") (Expr.idE "true#")) (Expr.idE "y#1"), Expr.idE "true#"),
              (Expr.idE "true#", Expr.idE "false#")]
             "irrefutable 'and' pattern"),
            (Expr.idE "true#",
             Expr.idE "false#")]
           "irrefutable 'and' pattern",
           LetE (FnDecl NrDef (Name.untyped "x") (Expr.idE "x#0"))
           (LetE (FnDecl NrDef (Name.untyped "y") (Expr.idE "y#1"))
            (LetE
             (FnDecl Def (Name.untyped "eqSeq")
              (LambdaE (Name.untyped "arg#4")
               (LambdaE (Name.untyped "arg#5")
                (CondE
                 [(CondE
                   [(AppE (AppE (Expr.idE "isList") (Expr.idE "null")) (Expr.idE "arg#4"),
                     CondE
                     [(AppE (AppE (Expr.idE "isList") (Expr.idE "null")) (Expr.idE "arg#5"), Expr.idE "true#"),
                      (Expr.idE "true#", Expr.idE "false#")]
                     "irrefutable 'and' pattern"),
                    (Expr.idE "true#",
                     Expr.idE "false#")]
                   "irrefutable 'and' pattern",
                   Expr.idE "true"),
                  (CondE
                   [(AppE (AppE (AppE (Expr.idE "isHeadTail") (LambdaE (Name.untyped "_") (Expr.idE "true#"))) (LambdaE (Name.untyped "_") (Expr.idE "true#"))) (Expr.idE "arg#4"),
                     CondE
                     [(AppE (AppE (AppE (Expr.idE "isHeadTail") (LambdaE (Name.untyped "_") (Expr.idE "true#"))) (LambdaE (Name.untyped "_") (Expr.idE "true#"))) (Expr.idE "arg#5"), Expr.idE "true#"),
                      (Expr.idE "true#", Expr.idE "false#")]
                     "irrefutable 'and' pattern"),
                    (Expr.idE "true#",
                     Expr.idE "false#")]
                   "irrefutable 'and' pattern",
                   LetE
                   (FnDecl NrDef (Name.untyped "z") (AppE (Expr.idE "hd") (Expr.idE "arg#4")))
                   (LetE (FnDecl NrDef (Name.untyped "zs") (AppE (Expr.idE "tl") (Expr.idE "arg#4")))
                    (LetE (FnDecl NrDef (Name.untyped "w") (AppE (Expr.idE "hd") (Expr.idE "arg#5")))
                     (LetE (FnDecl NrDef (Name.untyped "ws") (AppE (Expr.idE "tl") (Expr.idE "arg#5")))
                      (CondE
                       [(AppE (AppE (Expr.idE "eq") (Expr.idE "z")) (Expr.idE "w"),
                         CondE
                         [(AppE (AppE (Expr.idE "eqSeq") (Expr.idE "zs")) (Expr.idE "ws"), Expr.idE "true#"),
                          (Expr.idE "true#", Expr.idE "false#")]
                         "irrefutable 'and' pattern"),
                        (Expr.idE "true#", Expr.idE "false#")]
                       "irrefutable 'and' pattern"))))),
                  (CondE
                   [(AppE (LambdaE (Name.untyped "_") (Expr.idE "true#")) (Expr.idE "arg#4"),
                     CondE
                     [(AppE (LambdaE (Name.untyped "_") (Expr.idE "true#")) (Expr.idE "arg#5"), Expr.idE "true#"),
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
      FnDecl Def (Name.untyped "isString")
      (LambdaE (Name.untyped "arg#0")
       (CondE
        [(AppE (AppE (Expr.idE "isList") (Expr.idE "null")) (Expr.idE "arg#0"), Expr.idE "true"),
         (AppE (AppE (AppE (Expr.idE "isHeadTail") (Expr.idE "isChar")) (Expr.idE "isString")) (Expr.idE "arg#0"), Expr.idE "true"),
         (AppE (LambdaE (Name.untyped "_") (Expr.idE "true#")) (Expr.idE "arg#0"), Expr.idE "false")]
        "isString"))

    expected6 =
      FnDecl NrDef (Name.untyped "f")
      (LambdaE (Name.untyped "n#0")
       (CondE
        [(AppE (LambdaE (Name.untyped "_") (Expr.idE "true#")) (Expr.idE "n#0"),
          LetE (FnDecl NrDef (Name.untyped "n") (Expr.idE "n#0"))
          (LetE (FnDecl NrDef (Name.untyped "res#1")
                 (AppE (AppE (Expr.idE "cons") (Expr.intE 1))
                  (AppE (AppE (Expr.idE "cons") (Expr.intE 2))
                   (Expr.idE "null"))))
           (LetE (FnDecl NrDef (Name.untyped "x")
                  (AppE (Expr.idE "hd")
                   (Expr.idE "res#1")))
            (LetE (FnDecl NrDef (Name.untyped "y")
                   (AppE (Expr.idE "hd")
                    (AppE (Expr.idE "tl")
                     (Expr.idE "res#1"))))
             (AppE (AppE (Expr.idE "case") (Expr.idE "n"))
              (LambdaE (Name.untyped "arg#2")
               (CondE
                [(AppE (AppE (Expr.idE ">") (Expr.intE 1)) (Expr.idE "arg#2"), Expr.idE "x"),
                 (AppE (LambdaE (Name.untyped "_") (Expr.idE "true#")) (Expr.idE "arg#2"), Expr.idE "y")]
                "lambda")))))))]
        "f"))

    expected7 =
      [FnDecl NrDef (Name.untyped "res#0")
       (AppE (AppE (Expr.idE "cons") (Expr.intE 1))
        (AppE (AppE (Expr.idE "cons") (Expr.intE 2))
         (Expr.idE "null"))),
       FnDecl NrDef (Name.untyped "x")
       (AppE (Expr.idE "hd")
        (Expr.idE "res#0")),
       FnDecl NrDef (Name.untyped "y")
       (AppE (Expr.idE "hd")
        (AppE (Expr.idE "tl")
         (Expr.idE "res#0")))]

    expected8 =
      FnDecl NrDef (Name.untyped "f8")
      (LambdaE (Name.untyped "arg#0")
       (CondE [((Expr.idE "isApple") `AppE` (Expr.idE "isInt") `AppE` (Expr.idE "arg#0"),
                LetE
                (FnDecl NrDef (Name.untyped "x") (AppE (Expr.idE "unApple") (Expr.idE "arg#0")))
                (Expr.idE "x"))]
        "f8"))

    expected12 =
      FnDecl NrDef (Name.untyped "f")
      (LambdaE (Name.untyped "arg#0")
       (CondE [(AppE (Expr.appE (Name.untyped "isList") (Expr.seqE [Expr.idE "isInt"])) (Expr.idE "arg#0"), Expr.intE 0),
               (AppE (Expr.appE (Name.untyped "isList") (Expr.seqE [Expr.idE "isReal", Expr.idE "isString"])) (Expr.idE "arg#0"), Expr.intE 1),
               (AppE Expr.constTrueE (Expr.idE "arg#0"), Expr.intE 2)]
         "f"))

    expectedTuple =
      FnDecl NrDef (Name.untyped "f1") $
       LambdaE (Name.untyped "t#0") $
        CondE [(Expr.idE "isTuple2" `AppE` (Expr.idE "mkTuple2" `AppE` Expr.idE "isInt" `AppE` Expr.idE "isReal") `AppE` Expr.idE "t#0",
                (LetE (FnDecl NrDef (Name.untyped "t") (Expr.idE "t#0"))
                 (LetE (FnDecl NrDef (Name.untyped "x") (Expr.idE "tuple2Ref0" `AppE` Expr.idE "t#0"))
                  (LetE (FnDecl NrDef (Name.untyped "y") (Expr.idE "tuple2Ref1" `AppE` Expr.idE "t#0"))
                   (Expr.idE "addIntReal" `AppE` Expr.idE "x" `AppE` Expr.idE "y")))))]
         "f1"

    expectedUnit =
      [FnDecl NrDef (Name.untyped "f1") $
        LambdaE (Name.untyped "t#0") $
         CondE [(Expr.idE "isTuple0" `AppE` Expr.idE "t#0",
                 (LetE (FnDecl NrDef (Name.untyped "t") (Expr.idE "t#0"))
                  (Expr.idE "mkTuple0")))]
         "f1",
       FnDecl NrDef (Name.untyped "f2") (Expr.idE "mkTuple0")]

    expectedVariant =
      [FnDecl NrDef (Name.untyped "isFruit") $
        Expr.idE "isType#" `AppE` Expr.stringE "Fruit",
       FnDecl NrDef (Name.untyped "isApple") $
        Expr.idE "isVariant#" `AppE` Expr.stringE "Fruit" `AppE` Expr.intE 0,
       FnDecl NrDef (Name.untyped "isBanana") $
        Expr.idE "isVariant#" `AppE` Expr.stringE "Fruit" `AppE` Expr.intE 1,
       FnDecl NrDef (Name.untyped "isFig") $
        Expr.idE "isVariant#" `AppE` Expr.stringE "Fruit" `AppE` Expr.intE 2,
       FnDecl NrDef (Name.untyped "mkApple") $
        Expr.idE "mkVariant#" `AppE` Expr.stringE "Fruit" `AppE` Expr.intE 0 `AppE` Expr.idE "mkTuple0",
       FnDecl NrDef (Name.untyped "mkBanana") $
        LambdaE (Name.untyped "x#0") $
         CondE [(Expr.idE "isInt" `AppE` Expr.idE "x#0",
                 LetE (FnDecl NrDef (Name.untyped "x") (Expr.idE "x#0")) $
                  Expr.idE "mkVariant#" `AppE` Expr.stringE "Fruit" `AppE` Expr.intE 1 `AppE` Expr.idE "x")]
         "mkBanana",
       FnDecl NrDef (Name.untyped "mkFig") $
        LambdaE (Name.untyped "arg#1") $
         CondE [(Expr.idE "isTuple2" `AppE` (Expr.idE "mkTuple2" `AppE` Expr.idE "isInt" `AppE` Expr.idE "isReal") `AppE` Expr.idE "arg#1",
                 LetE (FnDecl NrDef (Name.untyped "arg") (Expr.idE "arg#1")) $
                  Expr.idE "mkVariant#" `AppE` Expr.stringE "Fruit" `AppE` Expr.intE 2 `AppE` Expr.idE "arg")]
         "mkFig",
       FnDecl NrDef (Name.untyped "unApple") $
        LambdaE (Name.untyped "arg#2") $
         CondE [(Expr.idE "isFruit" `AppE` Expr.idE "arg#2",
                 LetE (FnDecl NrDef (Name.untyped "arg") (Expr.idE "arg#2")) $
                  AppE
                    (LambdaE (Name.untyped "r#3") $
                      CondE [(Expr.idE "isTuple0" `AppE` Expr.idE "r#3",
                               LetE (FnDecl NrDef (Name.untyped "r") (Expr.idE "r#3")) $
                               Expr.idE "r")]
                      "lambda")
                    (Expr.idE "unVariant#" `AppE` Expr.idE "arg"))]
         "unApple",
       FnDecl NrDef (Name.untyped "unBanana") $
        LambdaE (Name.untyped "arg#4") $
         CondE [(Expr.idE "isFruit" `AppE` Expr.idE "arg#4",
                 LetE (FnDecl NrDef (Name.untyped "arg") (Expr.idE "arg#4")) $
                  AppE
                    (LambdaE (Name.untyped "r#5") $
                      CondE [(Expr.idE "isInt" `AppE` Expr.idE "r#5",
                               LetE (FnDecl NrDef (Name.untyped "r") (Expr.idE "r#5")) $
                               Expr.idE "r")]
                      "lambda")
                    (Expr.idE "unVariant#" `AppE` Expr.idE "arg"))]
         "unBanana",
       FnDecl NrDef (Name.untyped "unFig") $
        LambdaE (Name.untyped "arg#6") $
         CondE [(Expr.idE "isFruit" `AppE` Expr.idE "arg#6",
                 LetE (FnDecl NrDef (Name.untyped "arg") (Expr.idE "arg#6")) $
                  AppE
                    (LambdaE (Name.untyped "r#7") $
                      CondE [(Expr.idE "isTuple2" `AppE`
                              (Expr.idE "mkTuple2" `AppE` Expr.idE "isInt" `AppE` Expr.idE "isReal") `AppE`
                                Expr.idE "r#7",
                               LetE (FnDecl NrDef (Name.untyped "r") (Expr.idE "r#7")) $
                               Expr.idE "r")]
                      "lambda")
                    (Expr.idE "unVariant#" `AppE` Expr.idE "arg"))]
         "unFig",
       FnDecl NrDef (Name.untyped "f1") $
        LambdaE (Name.untyped "a#0") $
         CondE [(Expr.idE "isApple" `AppE` Expr.idE "isTuple0" `AppE` Expr.idE "a#0",
                 (LetE (FnDecl NrDef (Name.untyped "a") (Expr.idE "a#0"))
                  (Expr.intE 0))),
                (Expr.idE "isBanana" `AppE` Expr.idE "isInt" `AppE` Expr.idE "a#0",
                 (LetE (FnDecl NrDef (Name.untyped "b") (Expr.idE "a#0"))
                  (LetE (FnDecl NrDef (Name.untyped "x") (Expr.idE "unBanana" `AppE` Expr.idE "a#0"))
                   (Expr.intE 1)))),
                 (Expr.idE "isFig" `AppE` (Expr.idE "isTuple2" `AppE` (Expr.idE "mkTuple2" `AppE` Expr.idE "isInt" `AppE` Expr.idE "isReal")) `AppE` Expr.idE "a#0",
                  (LetE (FnDecl NrDef (Name.untyped "c") (Expr.idE "a#0"))
                   (LetE (FnDecl NrDef (Name.untyped "x") (Expr.idE "tuple2Ref0" `AppE` (Expr.idE "unFig" `AppE` Expr.idE "a#0")))
                    (LetE (FnDecl NrDef (Name.untyped "y") (Expr.idE "tuple2Ref1" `AppE` (Expr.idE "unFig" `AppE` Expr.idE "a#0")))
                     (Expr.intE 2)))))]
         "f1"
      ]
