module Data.Token where

-- | Source location contains the begin line and begin column.
data Srcloc
  = Srcloc !Int !Int
  deriving (Show)

data Token
  -- punctuation
  = TokenAt Srcloc
  | TokenAtSpace Srcloc
  | TokenComma Srcloc
  | TokenDot Srcloc
  | TokenEquiv Srcloc

  -- grouping
  | TokenLParen Srcloc
  | TokenRParen Srcloc
  | TokenLConsParen Srcloc
  | TokenRConsParen Srcloc
  | TokenLEnvParen
  | TokenREnvParen

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

  -- types
  | TokenArrow Srcloc

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

-- | Returns the 'Srcloc' for the given 'Token'. Returns an invalid 'Srcloc' for
-- 'TokenEOF'
tokenSrcloc :: Token -> Srcloc
tokenSrcloc (TokenAt srcloc) = srcloc
tokenSrcloc (TokenAtSpace srcloc) = srcloc
tokenSrcloc (TokenComma srcloc) = srcloc
tokenSrcloc (TokenDot srcloc) = srcloc
tokenSrcloc (TokenEquiv srcloc) = srcloc
tokenSrcloc (TokenLParen srcloc) = srcloc
tokenSrcloc (TokenRParen srcloc) = srcloc
tokenSrcloc (TokenLConsParen srcloc) = srcloc
tokenSrcloc (TokenRConsParen srcloc) = srcloc
tokenSrcloc TokenLEnvParen = Srcloc 0 0
tokenSrcloc TokenREnvParen = Srcloc 0 0
tokenSrcloc (TokenAs srcloc) = srcloc
tokenSrcloc (TokenLet srcloc) = srcloc
tokenSrcloc (TokenMe srcloc) = srcloc
tokenSrcloc (TokenIn srcloc) = srcloc
tokenSrcloc (TokenType srcloc) = srcloc
tokenSrcloc (TokenUse srcloc) = srcloc
tokenSrcloc (TokenWhere srcloc) = srcloc
tokenSrcloc (TokenChar srcloc _) = srcloc
tokenSrcloc (TokenInt srcloc _) = srcloc
tokenSrcloc (TokenDouble srcloc _) = srcloc
tokenSrcloc (TokenString srcloc _) = srcloc
tokenSrcloc (TokenComposition srcloc _) = srcloc
tokenSrcloc (TokenMult srcloc _) = srcloc
tokenSrcloc (TokenDiv srcloc _) = srcloc
tokenSrcloc (TokenAdd srcloc _) = srcloc
tokenSrcloc (TokenSub srcloc _) = srcloc
tokenSrcloc (TokenEq srcloc _) = srcloc
tokenSrcloc (TokenNeq srcloc _) = srcloc
tokenSrcloc (TokenLt srcloc _) = srcloc
tokenSrcloc (TokenGt srcloc _) = srcloc
tokenSrcloc (TokenLe srcloc _) = srcloc
tokenSrcloc (TokenGe srcloc _) = srcloc
tokenSrcloc (TokenCons srcloc _) = srcloc
tokenSrcloc (TokenSnoc srcloc _) = srcloc
tokenSrcloc (TokenAnd srcloc _) = srcloc
tokenSrcloc (TokenOr srcloc _) = srcloc
tokenSrcloc (TokenId srcloc _) = srcloc
tokenSrcloc (TokenQuotedId srcloc _) = srcloc
tokenSrcloc (TokenTypeId srcloc _) = srcloc
tokenSrcloc (TokenArrow srcloc) = srcloc
tokenSrcloc TokenEOF = Srcloc 0 0
