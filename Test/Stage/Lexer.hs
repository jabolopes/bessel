{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Stage.Lexer where

import Data.Token (Srcloc(..), Token(..))
import qualified Data.LexState as LexState
import qualified Lexer

deriving instance Eq Srcloc
deriving instance Eq Token

lexTokens :: String -> String -> [Token]
lexTokens filename str =
  yield (Lexer.lex (LexState.lexState filename str))
  where
    yield (TokenEOF, _) = []
    yield (token, state) = token:yield (Lexer.lex state)

lexTestFile :: String -> IO [Token]
lexTestFile filename =
  do str <- readFile filename
     return $ lexTokens filename str

lexSnippet :: String -> [Token]
lexSnippet = lexTokens "(Snippet)"

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

testLexer :: IO ()
testLexer =
  do expect expectedSnippet1 $ Snippet "let not @id = false | @ = true"
     expect expected1 $ File "Test/TestData1.bsl"
  where
    expectedSnippet1 =
      [TokenLet (Srcloc 1 1),
       TokenId (Srcloc 1 5) "not",
       TokenAtSpace (Srcloc 1 8),
       TokenId (Srcloc 1 10) "id",
       TokenEquiv (Srcloc 1 13),
       TokenId (Srcloc 1 15) "false",
       TokenBar (Srcloc 1 21),
       TokenAtSpace (Srcloc 1 22),
       TokenEquiv (Srcloc 1 25),
       TokenId (Srcloc 1 27) "true"]

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
       TokenId (Srcloc 3 26) "f2",
       TokenId (Srcloc 3 29) "x",
       TokenWhere (Srcloc 4 3),
       TokenLEnvParen (Srcloc 4 9),
       TokenLet (Srcloc 5 5),
       TokenId (Srcloc 5 9) "f2",
       TokenId (Srcloc 5 12) "z",
       TokenEquiv (Srcloc 5 14),
       TokenId (Srcloc 5 16) "z",
       TokenAdd (Srcloc 5 18) "+",
       TokenId (Srcloc 5 20) "y",
       TokenREnvParen (Srcloc 6 3)]
