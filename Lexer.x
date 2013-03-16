{
module Lexer where

import Parser
}


%wrapper "basic"


$digit      = [0-9]
$letter     = [a-zA-Z]

@symbol = "|" | "\\" | "!" | "@" | "#" | "$"
	| "%" | "&"  | "/" | "{" | "(" | "["
	| ")" | "]"  | "=" | "}" | "'" | "?"
	| "«" | "»"  | "+" | "*" | "`" | "´"
	| "º" | "ª"  | "~" | "^" | "," | ";"

@whitespace = [\ \t\n\r]
@comment    = ("|-"([^\*]|[\r\n]|(\*+([^\|\/]|[\r\n])))*"-|")
	    | ("-- ".*)

@character = "'" ($letter | $digit) "'"
@integer = ("-")?$digit+
@real    = ("-")?$digit+("."$digit+)?([eE]("-"|"+")?$digit+)?
@identifier = $letter ($letter | $digit | @symbol)*


tokens :-
  -- ignore
  @whitespace         ;
  @comment            ;

  -- symbols
  ","                 { \_ -> TokenComma }

  -- grouping
  "("                 { \_ -> TokenLParen }
  ")"                 { \_ -> TokenRParen }
  "["                 { \_ -> TokenLConsParen}
  "]"                 { \_ -> TokenRConsParen }
  "{"                 { \_ -> TokenLEnvParen }
  "}"                 { \_ -> TokenREnvParen }

  -- keywords
  "as"                { \_ -> TokenAs }
  "def"               { \_ -> TokenDef }
  "me"                { \_ -> TokenMe }
  "module"            { \_ -> TokenModule }
  "nrdef"             { \_ -> TokenNrdef }
  "type"              { \_ -> TokenType }
  "use"               { \_ -> TokenUse }
  "where"             { \_ -> TokenWhere }

  -- literals
  @character          { \s -> TokenChar (head (tail s)) }
  @integer            { \s -> TokenInt (read s) }
  @real               { \s -> TokenDouble (read s) }
  \"[^\"]*\"          { \s -> TokenString (init (tail s)) }

  -- operators
  "~"                 { \_ -> TokenTilde }

  "."                 { \_ -> TokenDot }

  "o"                 { \s -> TokenComposition s }

  "*"                 { \s -> TokenMult s }
  "/"                 { \s -> TokenDiv s }

  "+"                 { \s -> TokenAdd s }
  "-"                 { \s -> TokenSub s }

  "=="                { \s -> TokenEq s }
  "/="                { \s -> TokenNeq s }
  "<"                 { \s -> TokenLt s }
  ">"                 { \s -> TokenGt s }
  "<="                { \s -> TokenLe s }
  ">="                { \s -> TokenGe s }

  "&&"                { \s -> TokenAnd s }
  "||"                { \s -> TokenOr s }

  "->"                { \s -> TokenRArrow s }
  "<-"                { \s -> TokenLArrow s }

  "|"                 { \_ -> TokenBar }

  "="                 { \_ -> TokenEquiv }

  "@ "                { \_ -> TokenAtSpace }
  "@"                 { \_ -> TokenAt }

-- identifier
  @identifier         { \s -> TokenId s }
  "`" @identifier "`" { \s -> TokenQuotedId (init (tail s)) }


{
lex :: String -> [Token]
lex = alexScanTokens
}
