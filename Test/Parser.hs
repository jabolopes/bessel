{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Parser where

import qualified Data.Name as Name
import qualified Data.PrettyString as PrettyString
import Data.Source (Source(..))
import qualified Data.Source as Source
import qualified Parser
import qualified Pretty.Data.Source as Pretty

deriving instance Eq Source
deriving instance Show Source

patS :: String -> Maybe Source -> Source
patS binder = PatS (Name.untyped binder)

parseTestFile :: String -> IO Source
parseTestFile filename =
  do str <- readFile filename
     case Parser.parseFile (Name.untyped filename) str of
       Left err -> fail err
       Right src -> return src

parseSnippet :: Monad m => String -> m Source
parseSnippet str =
  do case Parser.parseRepl Name.empty str of
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
  do expect expectedSnippet1 $ Snippet "let not\n  @id = false\n  @ = true"
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
      FnDefS (patS "not" Nothing)
      (CondS [([Source.idS "id"], Source.idS "false"),
              ([patS "" Nothing], Source.idS "true")])

    expected1 =
      ModuleS (Name.untyped "Test.TestData1") []
        [FnDefS (patS "f1" Nothing)
         (WhereS
          (CondS [([patS "x" (Just (Source.idS "isInt")),
                    patS "y" (Just (Source.idS "isInt"))],
                   AppS (Source.idS "f2") (Source.idS "x"))])
          [FnDefS (patS "f2" Nothing)
           (CondS
            [([patS "z" Nothing], BinOpS "+" (Source.idS "z") (Source.idS "y"))])])]

    expected2 =
      ModuleS (Name.untyped "Test.TestData2") []
        [FnDefS (patS "f1" Nothing)
         (WhereS
          (CondS [([patS "x" (Just (Source.idS "isInt"))],
                   LetS [FnDefS (patS "y" Nothing) (IntS 0)]
                   (AppS (Source.idS "f2") (Source.idS "y")))])
          [FnDefS (patS "f2" Nothing)
           (CondS
            [([patS "z" Nothing], BinOpS "+" (Source.idS "z") (IntS 1))])])]

    expected3 =
      ModuleS (Name.untyped "Test.TestData3") []
        [FnDefS (patS "f" Nothing)
         (CondS [([patS "x" (Just (Source.idS "isInt")), patS "y" (Just (Source.idS "isInt"))], Source.idS "true"),
                 ([patS "x" Nothing, patS "y" Nothing], Source.idS "false")])]

    expected4 =
      ModuleS (Name.untyped "Test.TestData4") []
        [FnDefS (patS "eq" Nothing)
         (WhereS
          (CondS [([patS "x" (Just (Source.idS "isInt")), patS "y" (Just (Source.idS "isInt"))],
                   AppS (AppS (Source.idS "eqInt") (Source.idS "x")) (Source.idS "y")),
                  ([patS "x" Nothing, patS "y" Nothing],
                   AppS (AppS (Source.idS "eqSeq") (Source.idS "x")) (Source.idS "y"))])
          [FnDefS (patS "eqSeq" Nothing)
           (CondS [([SeqS [], SeqS []], Source.idS "true"),
                   ([BinOpS "+>" (patS "z" Nothing) (patS "zs" Nothing), BinOpS "+>" (patS "w" Nothing) (patS "ws" Nothing)],
                    AndS
                      (AppS (AppS (Source.idS "eq") (Source.idS "z")) (Source.idS "w"))
                      (AppS (AppS (Source.idS "eqSeq") (Source.idS "zs")) (Source.idS "ws"))),
                   ([patS "" Nothing, patS "" Nothing], Source.idS "false")])])]

    expected5 =
      ModuleS (Name.untyped "Test.TestData5") []
        [FnDefS (patS "isString" Nothing)
         (CondS [([SeqS []],Source.idS "true"),
                 ([BinOpS "+>" (Source.idS "isChar") (Source.idS "isString")],Source.idS "true"),
                 ([patS "" Nothing],Source.idS "false")])]

    expected6 =
      ModuleS (Name.untyped "Test.TestData6") []
        [FnDefS (patS "f" Nothing)
         (CondS
          [([patS "n" Nothing],
            LetS [FnDefS (SeqS [patS "x" Nothing,patS "y" Nothing]) (SeqS [IntS 1,IntS 2])]
            (AppS (AppS (Source.idS "case") (Source.idS "n"))
             (CondS [([AppS (patS ">" Nothing) (IntS 1)], Source.idS "x"),
                     ([patS "" Nothing], Source.idS "y")])))])]

    expected7 =
      ModuleS (Name.untyped "Test.TestData7") []
        [FnDefS
         (SeqS [patS "x" Nothing, patS "y" Nothing])
         (SeqS [IntS 1,IntS 2])]

    expected8 =
      ModuleS (Name.untyped "Test.TestData8") []
        [FnDefS (patS "f8" Nothing)
         (CondS [([AppS (patS "Apple" Nothing) (patS "x" (Just (Source.idS "isInt")))],
                  IntS 0)])]

    expected9 =
      ModuleS (Name.untyped "Test.TestData9") []
       [TypeDeclS (Name.untyped "Fruit") [(Name.untyped "Apple", patS "x" (Just (Source.idS "isInt")))],
        TypeDeclS (Name.untyped "MoreFruit") [(Name.untyped "Orange", patS "x" Nothing)],
        TypeDeclS (Name.untyped "EvenMoreFruit") [(Name.untyped "Banana", Source.idS "isInt"),
                                                  (Name.untyped "Kiwi", Source.idS "isReal")]]

    expected10 =
      ModuleS (Name.untyped "Test.TestData10") []
        [FnDefS (patS "f1" Nothing)
         (WhereS
          (CondS [([patS "x" (Just (Source.idS "isInt"))],AppS (Source.idS "f2") (Source.idS "x"))])
          [FnDefS (patS "f2" Nothing)
           (WhereS
            (CondS [([patS "y" Nothing],AppS (Source.idS "f3") (Source.idS "y"))])
            [FnDefS (patS "f3" Nothing)
             (CondS [([patS "w" Nothing],BinOpS "+" (Source.idS "w") (Source.idS "x"))])])])]
