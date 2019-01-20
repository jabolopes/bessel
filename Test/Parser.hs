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

-- | Convenience function that creates a function definition without
-- type annotation and where clause.
fnDefS :: Source -> Source -> Source
fnDefS pat body = FnDefS pat Nothing body []

-- | Convenience function that creates a function definition without
-- type annotation.
fnDefSWhere :: Source -> Source -> [Source] -> Source
fnDefSWhere pat body whereClause = FnDefS pat Nothing body whereClause

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
    expectedSnippet1 :: Source
    expectedSnippet1 =
      fnDefS (patS "not" Nothing)
      (CondS [([Source.idS "id"], Source.idS "false"),
              ([patS "" Nothing], Source.idS "true")])

    expected1 =
      ModuleS (Name.untyped "Test.TestData1") []
        [fnDefSWhere (patS "f1" Nothing)
         (CondS [([patS "x" (Just (Source.idS "isInt")),
                   patS "y" (Just (Source.idS "isInt"))],
                  AppS (Source.idS "f2") (Source.idS "x"))])
         [fnDefS (patS "f2" Nothing)
          (CondS
           [([patS "z" Nothing], BinOpS "+" (Source.idS "z") (Source.idS "y"))])]]

    expected2 =
      ModuleS (Name.untyped "Test.TestData2") []
        [fnDefSWhere (patS "f1" Nothing)
         (CondS [([patS "x" (Just (Source.idS "isInt"))],
                   LetS [fnDefS (patS "y" Nothing) (IntS 0)]
                   (AppS (Source.idS "f2") (Source.idS "y")))])
         [fnDefS (patS "f2" Nothing)
          (CondS
           [([patS "z" Nothing], BinOpS "+" (Source.idS "z") (IntS 1))])]]

    expected3 =
      ModuleS (Name.untyped "Test.TestData3") []
        [fnDefS (patS "f" Nothing)
         (CondS [([patS "x" (Just (Source.idS "isInt")), patS "y" (Just (Source.idS "isInt"))], Source.idS "true"),
                 ([patS "x" Nothing, patS "y" Nothing], Source.idS "false")])]

    expected4 =
      ModuleS (Name.untyped "Test.TestData4") []
        [fnDefSWhere (patS "eq" Nothing)
         (CondS [([patS "x" (Just (Source.idS "isInt")), patS "y" (Just (Source.idS "isInt"))],
                   AppS (AppS (Source.idS "eqInt") (Source.idS "x")) (Source.idS "y")),
                  ([patS "x" Nothing, patS "y" Nothing],
                   AppS (AppS (Source.idS "eqSeq") (Source.idS "x")) (Source.idS "y"))])
         [fnDefS (patS "eqSeq" Nothing)
          (CondS [([SeqS [], SeqS []], Source.idS "true"),
                  ([BinOpS "+>" (patS "z" Nothing) (patS "zs" Nothing), BinOpS "+>" (patS "w" Nothing) (patS "ws" Nothing)],
                   AndS
                     (AppS (AppS (Source.idS "eq") (Source.idS "z")) (Source.idS "w"))
                     (AppS (AppS (Source.idS "eqSeq") (Source.idS "zs")) (Source.idS "ws"))),
                  ([patS "" Nothing, patS "" Nothing], Source.idS "false")])]]

    expected5 =
      ModuleS (Name.untyped "Test.TestData5") []
        [fnDefS (patS "isString" Nothing)
         (CondS [([SeqS []],Source.idS "true"),
                 ([BinOpS "+>" (Source.idS "isChar") (Source.idS "isString")],Source.idS "true"),
                 ([patS "" Nothing],Source.idS "false")])]

    expected6 =
      ModuleS (Name.untyped "Test.TestData6") []
        [fnDefS (patS "f" Nothing)
         (CondS
          [([patS "n" Nothing],
            LetS [fnDefS (SeqS [patS "x" Nothing,patS "y" Nothing]) (SeqS [IntS 1,IntS 2])]
            (AppS (AppS (Source.idS "case") (Source.idS "n"))
             (CondS [([AppS (patS ">" Nothing) (IntS 1)], Source.idS "x"),
                     ([patS "" Nothing], Source.idS "y")])))])]

    expected7 =
      ModuleS (Name.untyped "Test.TestData7") []
        [fnDefS (SeqS [patS "x" Nothing, patS "y" Nothing])
         (SeqS [IntS 1, IntS 2])]

    expected8 =
      ModuleS (Name.untyped "Test.TestData8") []
        [fnDefS (patS "f8" Nothing)
         (CondS [([AppS (Source.idS "Apple") (patS "x" (Just (Source.idS "isInt")))],
                  Source.idS "x")])]

    expected9 =
      ModuleS (Name.untyped "Test.TestData9") []
       [TypeDeclS (Name.untyped "Fruit") [(Name.untyped "Apple", patS "x" (Just (Source.idS "isInt")))],
        TypeDeclS (Name.untyped "MoreFruit") [(Name.untyped "Orange", patS "x" Nothing)],
        TypeDeclS (Name.untyped "EvenMoreFruit") [(Name.untyped "Banana", Source.idS "isInt"),
                                                  (Name.untyped "Kiwi", Source.idS "isReal")]]

    expected10 :: Source
    expected10 =
      ModuleS (Name.untyped "Test.TestData10") []
        [fnDefSWhere (patS "f1" Nothing)
         (CondS [([patS "x" (Just (Source.idS "isInt"))],AppS (Source.idS "f2") (Source.idS "x"))])
         [fnDefSWhere (patS "f2" Nothing)
          (CondS [([patS "y" Nothing],AppS (Source.idS "f3") (Source.idS "y"))])
          [fnDefS (patS "f3" Nothing)
           (CondS [([patS "w" Nothing],BinOpS "+" (Source.idS "w") (Source.idS "x"))])]]]
