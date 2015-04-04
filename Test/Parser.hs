{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Parser where

import Data.QualName (QualName(..))
import qualified Data.PrettyString as PrettyString
import Data.Source (Source(..))
import qualified Data.Source as Source
import qualified Parser
import qualified Pretty.Data.Source as Pretty

deriving instance Eq QualName
deriving instance Eq Source
deriving instance Show Source

parseTestFile :: String -> IO Source
parseTestFile filename =
  do str <- readFile filename
     case Parser.parseFile filename str of
       Left err -> fail err
       Right src -> return src

parseSnippet :: Monad m => String -> m Source
parseSnippet str =
  do case Parser.parseRepl "" str of
       Left err -> fail $ show err
       Right src -> return src

data Actual = File String
            | Snippet String

-- edit: Use pretty strings instead
expect :: Source -> Actual -> IO ()
expect expected actual =
  case actual of
    File filename -> expect' filename =<< parseTestFile filename
    Snippet snippet -> expect' ("(Snippet) " ++ snippet) =<< parseSnippet snippet
  where
    expect' filename src
      | expected == src = return ()
      | otherwise =
        fail $ "Parser" ++ "\n" ++
               "In: " ++ filename ++ "\n" ++
               "Expected: " ++ "\n" ++ PrettyString.toString (Pretty.docSource expected) ++ "\n" ++
               "Actual: " ++ "\n" ++ PrettyString.toString (Pretty.docSource src)

testParser :: IO ()
testParser =
  do -- expect expectedSnippet1 $ Snippet "let not @id = false | @ = true"
     expect expected1 $ File "Test/TestData1.bsl"
     expect expected2 $ File "Test/TestData2.bsl"
     expect expected3 $ File "Test/TestData3.bsl"
     expect expected4 $ File "Test/TestData4.bsl"
     expect expected5 $ File "Test/TestData5.bsl"
     expect expected6 $ File "Test/TestData6.bsl"
     expect expected7 $ File "Test/TestData7.bsl"
     expect expected8 $ File "Test/TestData8.bsl"
     expect expected9 $ File "Test/TestData9.bsl"
     expect expected10 $ File "Test/TestData10.bsl"
  where
    expectedSnippet1 =
      FnDefS (PatS "not" Nothing)
      (CondS [([Source.idS "id"], Source.idS "false"),
              ([PatS "" Nothing], Source.idS "true")])

    expected1 =
      ModuleS "Test.TestData1" []
        [FnDefS (PatS "f1" Nothing)
         (WhereS
          (CondS [([PatS "x" (Just (Source.idS "isInt")),
                    PatS "y" (Just (Source.idS "isInt"))],
                   AppS (Source.idS "f2") (Source.idS "x"))])
          [FnDefS (PatS "f2" Nothing)
           (CondS
            [([PatS "z" Nothing], BinOpS "+" (Source.idS "z") (Source.idS "y"))])])]

    expected2 =
      ModuleS "Test.TestData2" []
        [FnDefS (PatS "f1" Nothing)
         (WhereS
          (CondS [([PatS "x" (Just (Source.idS "isInt"))],
                   LetS [FnDefS (PatS "y" Nothing) (IntS 0)]
                   (AppS (Source.idS "f2") (Source.idS "y")))])
          [FnDefS (PatS "f2" Nothing)
           (CondS
            [([PatS "z" Nothing], BinOpS "+" (Source.idS "z") (IntS 1))])])]

    expected3 =
      ModuleS "Test.TestData3" []
        [FnDefS (PatS "f" Nothing)
         (CondS [([PatS "x" (Just (Source.idS "isInt")), PatS "y" (Just (Source.idS "isInt"))], Source.idS "true"),
                 ([PatS "x" Nothing, PatS "y" Nothing], Source.idS "false")])]

    expected4 =
      ModuleS "Test.TestData4" []
        [FnDefS (PatS "eq" Nothing)
         (WhereS
          (CondS [([PatS "x" (Just (Source.idS "isInt")), PatS "y" (Just (Source.idS "isInt"))],
                   AppS (AppS (Source.idS "eqInt") (Source.idS "x")) (Source.idS "y")),
                  ([PatS "x" Nothing, PatS "y" Nothing],
                   AppS (AppS (Source.idS "eqSeq") (Source.idS "x")) (Source.idS "y"))])
          [FnDefS (PatS "eqSeq" Nothing)
           (CondS [([SeqS [], SeqS []], Source.idS "true"),
                   ([BinOpS "+>" (PatS "z" Nothing) (PatS "zs" Nothing), BinOpS "+>" (PatS "w" Nothing) (PatS "ws" Nothing)],
                    AndS
                      (AppS (AppS (Source.idS "eq") (Source.idS "z")) (Source.idS "w"))
                      (AppS (AppS (Source.idS "eqSeq") (Source.idS "zs")) (Source.idS "ws"))),
                   ([PatS "" Nothing, PatS "" Nothing], Source.idS "false")])])]

    expected5 =
      ModuleS "Test.TestData5" []
        [FnDefS (PatS "isString" Nothing)
         (CondS [([SeqS []],Source.idS "true"),
                 ([BinOpS "+>" (Source.idS "isChar") (Source.idS "isString")],Source.idS "true"),
                 ([PatS "" Nothing],Source.idS "false")])]

    expected6 =
      ModuleS "Test.TestData6" []
        [FnDefS (PatS "f" Nothing)
         (CondS
          [([PatS "n" Nothing],
            LetS [FnDefS (SeqS [PatS "x" Nothing,PatS "y" Nothing]) (SeqS [IntS 1,IntS 2])]
            (AppS (AppS (Source.idS "case") (Source.idS "n"))
             (CondS [([AppS (PatS ">" Nothing) (IntS 1)], Source.idS "x"),
                     ([PatS "" Nothing], Source.idS "y")])))])]

    expected7 =
      ModuleS "Test.TestData7" []
        [FnDefS
         (SeqS [PatS "x" Nothing, PatS "y" Nothing])
         (SeqS [IntS 1,IntS 2])]

    expected8 =
      ModuleS "Test.TestData8" []
        [FnDefS (PatS "f8" Nothing)
         (CondS [([AppS (PatS "Apple" Nothing) (PatS "x" (Just (Source.idS "isInt")))],
                  IntS 0)])]

    expected9 =
      ModuleS "Test.TestData9" []
        [TypeDeclS (QualName {fromQualName = "Fruit"}) [(QualName {fromQualName = "Apple"},PatS "x" (Just (Source.idS "isInt")))],
         TypeDeclS (QualName {fromQualName = "MoreFruit"}) [(QualName {fromQualName = "Orange"},PatS "x" Nothing)],
         TypeDeclS (QualName {fromQualName = "EvenMoreFruit"}) [(QualName {fromQualName = "Banana"},Source.idS "isInt")]]

    expected10 =
      ModuleS "Test.TestData10" []
        [FnDefS (PatS "f1" Nothing)
         (WhereS
          (CondS [([PatS "x" (Just (Source.idS "isInt"))],AppS (Source.idS "f2") (Source.idS "x"))])
          [FnDefS (PatS "f2" Nothing)
           (WhereS
            (CondS [([PatS "y" Nothing],AppS (Source.idS "f3") (Source.idS "y"))])
            [FnDefS (PatS "f3" Nothing)
             (CondS [([PatS "w" Nothing],BinOpS "+" (Source.idS "w") (Source.idS "x"))])])])]
