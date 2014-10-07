module Data.Token where

data Token
  -- punctuation
  = TokenAt
  | TokenAtSpace
  | TokenBar
  | TokenComma
  | TokenDot
  | TokenEquiv

  -- grouping
  | TokenLParen
  | TokenRParen
  | TokenLConsParen
  | TokenRConsParen
  | TokenLEnvParen
  | TokenREnvParen

  -- keywords
  | TokenAs
  | TokenLet
  | TokenMe
  | TokenIn
  | TokenType
  | TokenUse
  | TokenWhere

  -- literals
  | TokenChar   Char
  | TokenInt    Int
  | TokenDouble Double
  | TokenString String

  -- operators
  | TokenComposition String

  | TokenMult String
  | TokenDiv String
  | TokenAdd String
  | TokenSub String

  | TokenEq String
  | TokenNeq String
  | TokenLt String
  | TokenGt String
  | TokenLe String
  | TokenGe String

  | TokenCons String
  | TokenSnoc String

  | TokenAnd String
  | TokenOr String

  -- identifier
  | TokenId String
  | TokenQuotedId String
  | TokenTypeId String

  -- eof
  | TokenEOF
    deriving (Show)

operatorFixity :: String -> Token
operatorFixity op =
  let
    c = case take 2 op of
          "+>" -> TokenCons
          "<+" -> TokenSnoc
          "/=" -> TokenNeq
          _ -> case head op of
                 '*' -> TokenMult
                 '/' -> TokenDiv
                 '+' -> TokenAdd
                 '-' -> TokenSub
                 '=' -> TokenEq
                 '<' -> TokenLt
                 '>' -> TokenGt
                 _ -> TokenComposition
    in
      c op
