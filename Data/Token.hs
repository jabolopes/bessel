module Data.Token where

data Token
     -- punctuation
     = TokenArrow
     | TokenAt
     | TokenAtSpace
     | TokenBar
     | TokenColon
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
     | TokenDef
     | TokenMe
     | TokenSig
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