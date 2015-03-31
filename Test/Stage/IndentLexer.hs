{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Stage.IndentLexer where

import Data.Token (Srcloc(..), Token(..))
import qualified Data.LexState as LexState
import qualified Lexer
import qualified Stage.IndentLexer as IndentLexer

deriving instance Eq Srcloc
deriving instance Eq Token

lexTestFile :: String -> IO [Token]
lexTestFile filename =
  do str <- readFile filename
     return $ IndentLexer.indentLex str

lexSnippet :: String -> [Token]
lexSnippet = IndentLexer.indentLex

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
        fail $ "Lexer" ++ "\n" ++
               "In: " ++ filename ++ "\n" ++
               "Expected: " ++ "\n" ++ show expected ++ "\n" ++
               "Actual: " ++ "\n" ++ show tokens

testIndentLexer :: IO ()
testIndentLexer =
  do expect expected1 $ File "Test/TestData1.bsl"
     expect expected2 $ File "Test/TestData2.bsl"
     expect expected3 $ File "Test/TestData3.bsl"
     expect expected4 $ File "Test/TestData4.bsl"
     expect expected10 $ File "Test/TestData10.bsl"
  where
    expected1 =
      [TokenMe (Srcloc 1 1),
       TokenTypeId (Srcloc 1 4) "Test",
       TokenDot (Srcloc 1 8),
       TokenTypeId (Srcloc 1 9) "TestData1",
       TokenLet (Srcloc 1 1),
       TokenId (Srcloc 1 5) "f1",
       TokenId (Srcloc 1 8) "x",
       TokenAt (Srcloc 1 9),
       TokenId (Srcloc 1 10) "isInt",
       TokenId (Srcloc 1 16) "y",
       TokenAt (Srcloc 1 17),
       TokenId (Srcloc 1 18) "isInt",
       TokenEquiv (Srcloc 1 24),
       TokenLEnvParen (Srcloc 0 0),
       TokenId (Srcloc 1 26) "f2",
       TokenId (Srcloc 1 29) "x",
       TokenREnvParen (Srcloc 0 0),
       TokenWhere (Srcloc 1 1),
       TokenLEnvParen (Srcloc 0 0),
       TokenLet (Srcloc 1 1),
       TokenId (Srcloc 1 5) "f2",
       TokenId (Srcloc 1 8) "z",
       TokenEquiv (Srcloc 1 10),
       TokenLEnvParen (Srcloc 0 0),
       TokenId (Srcloc 1 12) "z",
       TokenAdd (Srcloc 1 14) "+",
       TokenId (Srcloc 1 16) "y",
       TokenREnvParen (Srcloc 0 0),
       TokenREnvParen (Srcloc 0 0)]

    expected2 =
      [TokenMe (Srcloc 1 1),
       TokenTypeId (Srcloc 1 4) "Test",
       TokenDot (Srcloc 1 8),
       TokenTypeId (Srcloc 1 9) "TestData2",
       TokenLet (Srcloc 1 1),
       TokenId (Srcloc 1 5) "f1",
       TokenId (Srcloc 1 8) "x",
       TokenAt (Srcloc 1 9),
       TokenId (Srcloc 1 10) "isInt",
       TokenEquiv (Srcloc 1 16),
       TokenLEnvParen (Srcloc 0 0),
       TokenLet (Srcloc 1 1),
       TokenId (Srcloc 1 5) "y",
       TokenEquiv (Srcloc 1 7),
       TokenLEnvParen (Srcloc 0 0),
       TokenInt (Srcloc 1 9) 0,
       TokenREnvParen (Srcloc 0 0),
       TokenIn (Srcloc 1 11),
       TokenId (Srcloc 1 1) "f2",
       TokenId (Srcloc 1 4) "y",
       TokenREnvParen (Srcloc 0 0),
       TokenWhere (Srcloc 1 1),
       TokenLEnvParen (Srcloc 0 0),
       TokenLet (Srcloc 1 1),
       TokenId (Srcloc 1 5) "f2",
       TokenId (Srcloc 1 8) "z",
       TokenEquiv (Srcloc 1 10),
       TokenLEnvParen (Srcloc 0 0),
       TokenId (Srcloc 1 12) "z",
       TokenAdd (Srcloc 1 14) "+",
       TokenInt (Srcloc 1 16) 1,
       TokenREnvParen (Srcloc 0 0),
       TokenREnvParen (Srcloc 0 0)]

    expected3 =
      [TokenMe (Srcloc 1 1),
       TokenTypeId (Srcloc 1 4) "Test",
       TokenDot (Srcloc 1 8),
       TokenTypeId (Srcloc 1 9) "TestData3",
       TokenLet (Srcloc 1 1),
       TokenId (Srcloc 1 5) "f",
       TokenLEnvParen (Srcloc 0 0),
       TokenId (Srcloc 1 1) "x",
       TokenAt (Srcloc 1 2),
       TokenId (Srcloc 1 3) "isInt",
       TokenId (Srcloc 1 9) "y",
       TokenAt (Srcloc 1 10),
       TokenId (Srcloc 1 11) "isInt",
       TokenEquiv (Srcloc 1 17),
       TokenLEnvParen (Srcloc 0 0),
       TokenId (Srcloc 1 19) "true",
       TokenREnvParen (Srcloc 0 0),
       TokenId (Srcloc 1 1) "x",
       TokenId (Srcloc 1 9) "y",
       TokenEquiv (Srcloc 1 17),
       TokenLEnvParen (Srcloc 0 0),
       TokenId (Srcloc 1 19) "false",
       TokenREnvParen (Srcloc 0 0),
       TokenREnvParen (Srcloc 0 0)]

    expected4 =
      [TokenMe (Srcloc 1 1),
       TokenTypeId (Srcloc 1 4) "Test",
       TokenDot (Srcloc 1 8),
       TokenTypeId (Srcloc 1 9) "TestData4",
       TokenLet (Srcloc 1 1),
       TokenId (Srcloc 1 5) "eq",
       TokenLEnvParen (Srcloc 0 0),
       TokenId (Srcloc 1 1) "x",
       TokenAt (Srcloc 1 2),
       TokenId (Srcloc 1 3) "isInt",
       TokenId (Srcloc 1 9) "y",
       TokenAt (Srcloc 1 10),
       TokenId (Srcloc 1 11) "isInt",
       TokenEquiv (Srcloc 1 17),
       TokenLEnvParen (Srcloc 0 0),
       TokenId (Srcloc 1 19) "eqInt",
       TokenId (Srcloc 1 25) "x",
       TokenId (Srcloc 1 27) "y",
       TokenREnvParen (Srcloc 0 0),
       TokenId (Srcloc 1 1) "x",
       TokenId (Srcloc 1 9) "y",
       TokenEquiv (Srcloc 1 17),
       TokenLEnvParen (Srcloc 0 0),
       TokenId (Srcloc 1 1) "eqSeq",
       TokenId (Srcloc 1 7) "x",
       TokenId (Srcloc 1 9) "y",
       TokenREnvParen (Srcloc 0 0),
       TokenREnvParen (Srcloc 0 0),
       TokenWhere (Srcloc 1 1),
       TokenLEnvParen (Srcloc 0 0),
       TokenLet (Srcloc 1 1),
       TokenId (Srcloc 1 5) "eqSeq",
       TokenLEnvParen (Srcloc 0 0),
       TokenLConsParen (Srcloc 1 1),
       TokenRConsParen (Srcloc 1 2),
       TokenLConsParen (Srcloc 1 4),
       TokenRConsParen (Srcloc 1 5),
       TokenEquiv (Srcloc 1 7),
       TokenLEnvParen (Srcloc 0 0),
       TokenId (Srcloc 1 9) "true",
       TokenREnvParen (Srcloc 0 0),
       TokenLParen (Srcloc 1 1),
       TokenId (Srcloc 1 2) "z",
       TokenAdd (Srcloc 1 4) "+>",
       TokenId (Srcloc 1 7) "zs",
       TokenRParen (Srcloc 1 9),
       TokenLParen (Srcloc 1 11),
       TokenId (Srcloc 1 12) "w",
       TokenAdd (Srcloc 1 14) "+>",
       TokenId (Srcloc 1 17) "ws",
       TokenRParen (Srcloc 1 19),
       TokenEquiv (Srcloc 1 21),
       TokenLEnvParen (Srcloc 0 0),
       TokenId (Srcloc 1 23) "eq",
       TokenId (Srcloc 1 26) "z",
       TokenId (Srcloc 1 28) "w",
       TokenAnd (Srcloc 1 30) "&&",
       TokenId (Srcloc 1 33) "eqSeq",
       TokenId (Srcloc 1 39) "zs",
       TokenId (Srcloc 1 42) "ws",
       TokenREnvParen (Srcloc 0 0),
       TokenAt (Srcloc 1 1),
       TokenAtSpace (Srcloc 1 2),
       TokenEquiv (Srcloc 1 5),
       TokenLEnvParen (Srcloc 0 0),
       TokenId (Srcloc 1 7) "false",
       TokenREnvParen (Srcloc 0 0),
       TokenREnvParen (Srcloc 0 0),
       TokenREnvParen (Srcloc 0 0)]

    expected6 =
      [TokenMe (Srcloc 1 1),
       TokenTypeId (Srcloc 1 4) "Test",
       TokenDot (Srcloc 1 8),
       TokenTypeId (Srcloc 1 9) "TestData6",
       TokenLet (Srcloc 1 1),
       TokenId (Srcloc 1 5) "f",
       TokenId (Srcloc 1 7) "n",
       TokenEquiv (Srcloc 1 9),
       TokenLEnvParen (Srcloc 0 0),
       TokenLet (Srcloc 1 1),
       TokenLConsParen (Srcloc 1 5),
       TokenId (Srcloc 1 6) "x",
       TokenComma (Srcloc 1 7),
       TokenId (Srcloc 1 9) "y",
       TokenRConsParen (Srcloc 1 10),
       TokenEquiv (Srcloc 1 12),
       TokenLEnvParen (Srcloc 0 0),
       TokenLConsParen (Srcloc 1 14),
       TokenInt (Srcloc 1 15) 1,
       TokenComma (Srcloc 1 16),
       TokenInt (Srcloc 1 18) 2,
       TokenRConsParen (Srcloc 1 19),
       TokenREnvParen (Srcloc 0 0),
       TokenIn (Srcloc 1 21),
       TokenId (Srcloc 1 1) "case",
       TokenId (Srcloc 1 6) "n",
       TokenLEnvParen (Srcloc 0 0),
       TokenLParen (Srcloc 1 1),
       TokenLParen (Srcloc 1 2),
       TokenGt (Srcloc 1 3) ">",
       TokenRParen (Srcloc 1 4),
       TokenInt (Srcloc 1 6) 1,
       TokenRParen (Srcloc 1 7),
       TokenEquiv (Srcloc 1 9),
       TokenLEnvParen (Srcloc 0 0),
       TokenId (Srcloc 1 11) "x",
       TokenREnvParen (Srcloc 0 0),
       TokenAt (Srcloc 1 1),
       TokenEquiv (Srcloc 1 9),
       TokenLEnvParen (Srcloc 0 0),
       TokenId (Srcloc 1 11) "y",
       TokenREnvParen (Srcloc 0 0),
       TokenREnvParen (Srcloc 0 0),
       TokenREnvParen (Srcloc 0 0)]

    expected10 =
      [TokenMe (Srcloc 1 1),
       TokenTypeId (Srcloc 1 4) "Test",
       TokenDot (Srcloc 1 8),
       TokenTypeId (Srcloc 1 9) "TestData10",
       TokenLet (Srcloc 1 1),
       TokenId (Srcloc 1 5) "f1",
       TokenId (Srcloc 1 8) "x",
       TokenAt (Srcloc 1 9),
       TokenId (Srcloc 1 10) "isInt",
       TokenEquiv (Srcloc 1 16),
       TokenLEnvParen (Srcloc 0 0),
       TokenId (Srcloc 1 18) "f2",
       TokenId (Srcloc 1 21) "x",
       TokenREnvParen (Srcloc 0 0),
       TokenLEnvParen (Srcloc 0 0),
       TokenWhere (Srcloc 1 1),
       TokenLEnvParen (Srcloc 0 0),
       TokenLet (Srcloc 1 1),
       TokenId (Srcloc 1 5) "f2",
       TokenId (Srcloc 1 8) "y",
       TokenEquiv (Srcloc 1 10),
       TokenLEnvParen (Srcloc 0 0),
       TokenId (Srcloc 1 1) "f3",
       TokenId (Srcloc 1 4) "y",
       TokenWhere (Srcloc 1 1),
       TokenLEnvParen (Srcloc 0 0),
       TokenLet (Srcloc 1 1),
       TokenId (Srcloc 1 5) "f3",
       TokenId (Srcloc 1 8) "w",
       TokenEquiv (Srcloc 1 10),
       TokenLEnvParen (Srcloc 0 0),
       TokenId (Srcloc 1 12) "w",
       TokenAdd (Srcloc 1 14) "+",
       TokenId (Srcloc 1 16) "x",
       TokenREnvParen (Srcloc 0 0),
       TokenREnvParen (Srcloc 0 0),
       TokenREnvParen (Srcloc 0 0),
       TokenREnvParen (Srcloc 0 0),
       TokenREnvParen (Srcloc 0 0)]
