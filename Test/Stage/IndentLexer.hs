{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Stage.IndentLexer where

import Data.Token (Srcloc(..), Token(..))
import qualified Data.Name as Name
import qualified Stage.IndentLexer as IndentLexer

deriving instance Eq Srcloc
deriving instance Eq Token

lexTestFile :: String -> IO [Token]
lexTestFile filename =
  do str <- readFile filename
     return $ IndentLexer.indentLex (Name.untyped filename) str

lexSnippet :: String -> [Token]
lexSnippet = IndentLexer.indentLex (Name.untyped "(Snippet)")

data Actual = File String
            | Snippet String

expect :: [Token] -> Actual -> IO ()
expect expected actual =
  case actual of
    File filename -> expect' filename =<< lexTestFile filename
    Snippet snippet -> expect' ("(Snippet) " ++ snippet) $ lexSnippet snippet
  where
    expect' filename tokens
      | expected == tokens = return ()
      | otherwise =
        fail $ "IndentLexer" ++ "\n" ++
               "In: " ++ filename ++ "\n" ++
               "Expected: " ++ "\n" ++ show expected ++ "\n" ++
               "Actual: " ++ "\n" ++ show tokens

testIndentLexer :: IO ()
testIndentLexer =
  do expect expected1 $ File "Test/TestData1.bsl"
     expect expected2 $ File "Test/TestData2.bsl"
     expect expected3 $ File "Test/TestData3.bsl"
     expect expected4 $ File "Test/TestData4.bsl"
     expect expected6 $ File "Test/TestData6.bsl"
     expect expected9 $ File "Test/TestData9.bsl"
     expect expected10 $ File "Test/TestData10.bsl"
     expect expected11 $ File "Test/TestData11.bsl"
  where
    expected1 =
      [TokenMe (Srcloc 1 1),
       TokenTypeId (Srcloc 1 4) "Test",
       TokenDot (Srcloc 1 8),
       TokenTypeId (Srcloc 1 9) "TestData1",
       TokenLet (Srcloc 3 1),
       TokenId (Srcloc 3 5) "f1",
       TokenId (Srcloc 3 8) "x",
       TokenAt (Srcloc 3 9),
       TokenId (Srcloc 3 10) "isInt",
       TokenId (Srcloc 3 16) "y",
       TokenAt (Srcloc 3 17),
       TokenId (Srcloc 3 18) "isInt",
       TokenEquiv (Srcloc 3 24),
       TokenLEnvParen,
       TokenId (Srcloc 3 26) "f2",
       TokenId (Srcloc 3 29) "x",
       TokenREnvParen,
       TokenWhere (Srcloc 4 1),
       TokenLEnvParen,
       TokenLet (Srcloc 5 1),
       TokenId (Srcloc 5 5) "f2",
       TokenId (Srcloc 5 8) "z",
       TokenEquiv (Srcloc 5 10),
       TokenLEnvParen,
       TokenId (Srcloc 5 12) "z",
       TokenAdd (Srcloc 5 14) "+",
       TokenId (Srcloc 5 16) "y",
       TokenREnvParen,
       TokenREnvParen]

    expected2 =
      [TokenMe (Srcloc 1 1),
       TokenTypeId (Srcloc 1 4) "Test",
       TokenDot (Srcloc 1 8),
       TokenTypeId (Srcloc 1 9) "TestData2",
       TokenLet (Srcloc 3 1),
       TokenId (Srcloc 3 5) "f1",
       TokenId (Srcloc 3 8) "x",
       TokenAt (Srcloc 3 9),
       TokenId (Srcloc 3 10) "isInt",
       TokenEquiv (Srcloc 3 16),
       TokenLEnvParen,
       TokenLet (Srcloc 4 1),
       TokenId (Srcloc 4 5) "y",
       TokenEquiv (Srcloc 4 7),
       TokenLEnvParen,
       TokenInt (Srcloc 4 9) 0,
       TokenREnvParen,
       TokenIn (Srcloc 4 11),
       TokenId (Srcloc 5 1) "f2",
       TokenId (Srcloc 5 4) "y",
       TokenREnvParen,
       TokenWhere (Srcloc 6 1),
       TokenLEnvParen,
       TokenLet (Srcloc 7 1),
       TokenId (Srcloc 7 5) "f2",
       TokenId (Srcloc 7 8) "z",
       TokenEquiv (Srcloc 7 10),
       TokenLEnvParen,
       TokenId (Srcloc 7 12) "z",
       TokenAdd (Srcloc 7 14) "+",
       TokenInt (Srcloc 7 16) 1,
       TokenREnvParen,
       TokenREnvParen]

    expected3 =
      [TokenMe (Srcloc 1 1),
       TokenTypeId (Srcloc 1 4) "Test",
       TokenDot (Srcloc 1 8),
       TokenTypeId (Srcloc 1 9) "TestData3",
       TokenLet (Srcloc 3 1),
       TokenId (Srcloc 3 5) "f",
       TokenLEnvParen,
       TokenId (Srcloc 4 1) "x",
       TokenAt (Srcloc 4 2),
       TokenId (Srcloc 4 3) "isInt",
       TokenId (Srcloc 4 9) "y",
       TokenAt (Srcloc 4 10),
       TokenId (Srcloc 4 11) "isInt",
       TokenEquiv (Srcloc 4 17),
       TokenLEnvParen,
       TokenId (Srcloc 4 19) "true",
       TokenREnvParen,
       TokenId (Srcloc 5 1) "x",
       TokenId (Srcloc 5 9) "y",
       TokenEquiv (Srcloc 5 17),
       TokenLEnvParen,
       TokenId (Srcloc 5 19) "false",
       TokenREnvParen,
       TokenREnvParen]

    expected4 =
      [TokenMe (Srcloc 1 1),
       TokenTypeId (Srcloc 1 4) "Test",
       TokenDot (Srcloc 1 8),
       TokenTypeId (Srcloc 1 9) "TestData4",
       TokenLet (Srcloc 3 1),
       TokenId (Srcloc 3 5) "eq",
       TokenLEnvParen,
       TokenId (Srcloc 4 1) "x",
       TokenAt (Srcloc 4 2),
       TokenId (Srcloc 4 3) "isInt",
       TokenId (Srcloc 4 9) "y",
       TokenAt (Srcloc 4 10),
       TokenId (Srcloc 4 11) "isInt",
       TokenEquiv (Srcloc 4 17),
       TokenLEnvParen,
       TokenId (Srcloc 4 19) "eqInt",
       TokenId (Srcloc 4 25) "x",
       TokenId (Srcloc 4 27) "y",
       TokenREnvParen,
       TokenId (Srcloc 5 1) "x",
       TokenId (Srcloc 5 9) "y",
       TokenEquiv (Srcloc 5 17),
       TokenLEnvParen,
       TokenId (Srcloc 6 1) "eqSeq",
       TokenId (Srcloc 6 7) "x",
       TokenId (Srcloc 6 9) "y",
       TokenREnvParen,
       TokenREnvParen,
       TokenWhere (Srcloc 7 1),
       TokenLEnvParen,
       TokenLet (Srcloc 8 1),
       TokenId (Srcloc 8 5) "eqSeq",
       TokenLEnvParen,
       TokenLConsParen (Srcloc 9 1),
       TokenRConsParen (Srcloc 9 2),
       TokenLConsParen (Srcloc 9 4),
       TokenRConsParen (Srcloc 9 5),
       TokenEquiv (Srcloc 9 7),
       TokenLEnvParen,
       TokenId (Srcloc 9 9) "true",
       TokenREnvParen,
       TokenLParen (Srcloc 10 1),
       TokenId (Srcloc 10 2) "z",
       TokenAdd (Srcloc 10 4) "+>",
       TokenId (Srcloc 10 7) "zs",
       TokenRParen (Srcloc 10 9),
       TokenLParen (Srcloc 10 11),
       TokenId (Srcloc 10 12) "w",
       TokenAdd (Srcloc 10 14) "+>",
       TokenId (Srcloc 10 17) "ws",
       TokenRParen (Srcloc 10 19),
       TokenEquiv (Srcloc 10 21),
       TokenLEnvParen,
       TokenId (Srcloc 10 23) "eq",
       TokenId (Srcloc 10 26) "z",
       TokenId (Srcloc 10 28) "w",
       TokenAnd (Srcloc 10 30) "&&",
       TokenId (Srcloc 10 33) "eqSeq",
       TokenId (Srcloc 10 39) "zs",
       TokenId (Srcloc 10 42) "ws",
       TokenREnvParen,
       TokenAt (Srcloc 11 1),
       TokenAtSpace (Srcloc 11 2),
       TokenEquiv (Srcloc 11 5),
       TokenLEnvParen,
       TokenId (Srcloc 11 7) "false",
       TokenREnvParen,
       TokenREnvParen,
       TokenREnvParen]

    expected6 =
      [TokenMe (Srcloc 1 1),
       TokenTypeId (Srcloc 1 4) "Test",
       TokenDot (Srcloc 1 8),
       TokenTypeId (Srcloc 1 9) "TestData6",
       TokenLet (Srcloc 3 1),
       TokenId (Srcloc 3 5) "f",
       TokenId (Srcloc 3 7) "n",
       TokenEquiv (Srcloc 3 9),
       TokenLEnvParen,
       TokenLet (Srcloc 4 1),
       TokenLConsParen (Srcloc 4 5),
       TokenId (Srcloc 4 6) "x",
       TokenComma (Srcloc 4 7),
       TokenId (Srcloc 4 9) "y",
       TokenRConsParen (Srcloc 4 10),
       TokenEquiv (Srcloc 4 12),
       TokenLEnvParen,
       TokenLConsParen (Srcloc 4 14),
       TokenInt (Srcloc 4 15) 1,
       TokenComma (Srcloc 4 16),
       TokenInt (Srcloc 4 18) 2,
       TokenRConsParen (Srcloc 4 19),
       TokenREnvParen,
       TokenIn (Srcloc 4 21),
       TokenId (Srcloc 5 1) "case",
       TokenId (Srcloc 5 6) "n",
       TokenLEnvParen,
       TokenLParen (Srcloc 6 1),
       TokenLParen (Srcloc 6 2),
       TokenGt (Srcloc 6 3) ">",
       TokenRParen (Srcloc 6 4),
       TokenInt (Srcloc 6 6) 1,
       TokenRParen (Srcloc 6 7),
       TokenEquiv (Srcloc 6 9),
       TokenLEnvParen,
       TokenId (Srcloc 6 11) "x",
       TokenREnvParen,
       TokenAt (Srcloc 7 1),
       TokenEquiv (Srcloc 7 9),
       TokenLEnvParen,
       TokenId (Srcloc 7 11) "y",
       TokenREnvParen,
       TokenREnvParen,
       TokenREnvParen]

    expected9 =
      [TokenMe (Srcloc 1 1),
       TokenTypeId (Srcloc 1 4) "Test",
       TokenDot (Srcloc 1 8),
       TokenTypeId (Srcloc 1 9) "TestData9",
       TokenType (Srcloc 3 1),
       TokenTypeId (Srcloc 3 6) "Fruit",
       TokenEquiv (Srcloc 3 12),
       TokenLEnvParen,
       TokenTypeId (Srcloc 3 14) "Apple",
       TokenId (Srcloc 3 20) "x",
       TokenAt (Srcloc 3 21),
       TokenId (Srcloc 3 22) "isInt",
       TokenREnvParen,
       TokenType (Srcloc 4 1),
       TokenTypeId (Srcloc 4 6) "MoreFruit",
       TokenEquiv (Srcloc 4 16),
       TokenLEnvParen,
       TokenTypeId (Srcloc 4 18) "Orange",
       TokenId (Srcloc 4 25) "x",
       TokenREnvParen,
       TokenType (Srcloc 5 1),
       TokenTypeId (Srcloc 5 6) "EvenMoreFruit",
       TokenEquiv (Srcloc 5 20),
       TokenLEnvParen,
       TokenTypeId (Srcloc 6 1) "Banana",
       TokenAtSpace (Srcloc 6 7),
       TokenId (Srcloc 6 9) "isInt",
       TokenAlternative (Srcloc 7 1),
       TokenTypeId (Srcloc 7 3) "Kiwi",
       TokenAtSpace (Srcloc 7 7),
       TokenId (Srcloc 7 9) "isReal",
       TokenREnvParen]

    expected10 =
      [TokenMe (Srcloc 1 1),
       TokenTypeId (Srcloc 1 4) "Test",
       TokenDot (Srcloc 1 8),
       TokenTypeId (Srcloc 1 9) "TestData10",
       TokenLet (Srcloc 3 1),
       TokenId (Srcloc 3 5) "f1",
       TokenId (Srcloc 3 8) "x",
       TokenAt (Srcloc 3 9),
       TokenId (Srcloc 3 10) "isInt",
       TokenEquiv (Srcloc 3 16),
       TokenLEnvParen,
       TokenId (Srcloc 3 18) "f2",
       TokenId (Srcloc 3 21) "x",
       TokenREnvParen,
       TokenWhere (Srcloc 4 1),
       TokenLEnvParen,
       TokenLet (Srcloc 5 1),
       TokenId (Srcloc 5 5) "f2",
       TokenId (Srcloc 5 8) "y",
       TokenEquiv (Srcloc 5 10),
       TokenLEnvParen,
       TokenId (Srcloc 6 1) "f3",
       TokenId (Srcloc 6 4) "y",
       TokenREnvParen,
       TokenWhere (Srcloc 7 1),
       TokenLEnvParen,
       TokenLet (Srcloc 8 1),
       TokenId (Srcloc 8 5) "f3",
       TokenId (Srcloc 8 8) "w",
       TokenEquiv (Srcloc 8 10),
       TokenLEnvParen,
       TokenId (Srcloc 8 12) "w",
       TokenAdd (Srcloc 8 14) "+",
       TokenId (Srcloc 8 16) "x",
       TokenREnvParen,
       TokenREnvParen,
       TokenREnvParen]

    expected11 =
      [TokenMe (Srcloc 1 1),
       TokenTypeId (Srcloc 1 4) "Test",
       TokenDot (Srcloc 1 8),
       TokenTypeId (Srcloc 1 9) "TestData11",
       TokenLet (Srcloc 3 1),
       TokenId (Srcloc 3 5) "f11",
       TokenId (Srcloc 3 9) "x",
       TokenEquiv (Srcloc 3 11),
       TokenLEnvParen,
       TokenLParen (Srcloc 4 1),
       TokenId (Srcloc 4 2) "isInt",
       TokenEquiv (Srcloc 4 8),
       TokenLEnvParen,
       TokenId (Srcloc 4 10) "true",
       TokenREnvParen,
       TokenAt (Srcloc 5 1),
       TokenEquiv (Srcloc 5 8),
       TokenLEnvParen,
       TokenId (Srcloc 5 10) "false",
       TokenREnvParen,
       TokenRParen (Srcloc 5 15),
       TokenId (Srcloc 5 17) "x",
       TokenREnvParen]
