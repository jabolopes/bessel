module Data.Token where

-- | Source location contains the begin line, end line, and begin column.
data Srcloc
  = Srcloc !Int !Int !Int
  deriving (Show)

data Token
  -- punctuation
  = TokenAt Srcloc
  | TokenAtSpace Srcloc
  | TokenBar Srcloc
  | TokenComma Srcloc
  | TokenDot Srcloc
  | TokenEquiv Srcloc

  -- grouping
  | TokenLParen Srcloc
  | TokenRParen Srcloc
  | TokenLConsParen Srcloc
  | TokenRConsParen Srcloc
  | TokenLEnvParen Srcloc
  | TokenREnvParen Srcloc

  -- keywords
  | TokenAs Srcloc
  | TokenLet Srcloc
  | TokenMe Srcloc
  | TokenIn Srcloc
  | TokenType Srcloc
  | TokenUse Srcloc
  | TokenWhere Srcloc

  -- literals
  | TokenChar   Srcloc Char
  | TokenInt    Srcloc Int
  | TokenDouble Srcloc Double
  | TokenString Srcloc String

  -- operators
  | TokenComposition Srcloc String

  | TokenMult Srcloc String
  | TokenDiv Srcloc String
  | TokenAdd Srcloc String
  | TokenSub Srcloc String

  | TokenEq Srcloc String
  | TokenNeq Srcloc String
  | TokenLt Srcloc String
  | TokenGt Srcloc String
  | TokenLe Srcloc String
  | TokenGe Srcloc String

  | TokenCons Srcloc String
  | TokenSnoc Srcloc String

  | TokenAnd Srcloc String
  | TokenOr Srcloc String

  -- identifier
  | TokenId Srcloc String
  | TokenQuotedId Srcloc String
  | TokenTypeId Srcloc String

  -- eof
  | TokenEOF
    deriving (Show)

operatorFixity :: Srcloc -> String -> Token
operatorFixity srcloc op =
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
      c srcloc op
