{
module Lexer where

import Parser
}


%wrapper "basic"


$digit      = [0-9]
$letter     = [a-zA-Z]

@symbol = "|" | "\\" | "!" | "#" | "$" | "%"
        | "&"  | "/" | "=" | "'" | "?" | "«"
	| "»"  | "+" | "*" | "`" | "´" | "º"
	| "ª"  | "~" | "^" | "," | ";"

@id_char = $letter | $digit | @symbol

@whitespace = [\ \t\n\r]
@comment    = ("|-"([^\*]|[\r\n]|(\*+([^\|\/]|[\r\n])))*"-|")
	    | ("-- ".*)

@character  = "'" ($letter | $digit) "'"
@integer    = ("-")?$digit+
@real       = ("-")?$digit+("."$digit+)?([eE]("-"|"+")?$digit+)?
@string     = \"[^\"]*\"
@identifier = $letter (@id_char)*


tokens :-
  -- ignore
  @whitespace         ;
  @comment            ;

  -- punctuation
  "@ "                { \_ -> TokenAtSpace }
  "@"                 { \_ -> TokenAt }
  "|"                 { \_ -> TokenBar }
  ","                 { \_ -> TokenComma }
  "."                 { \_ -> TokenDot }
  "="                 { \_ -> TokenEquiv }

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
  @string             { \s -> TokenString (init (tail s)) }

  -- operators
  "o" (@id_char)*     { \s -> TokenComposition s }

  "*" (@id_char)*     { \s -> TokenMult s }
  "/" (@id_char)*     { \s -> TokenDiv s }
  "+" (@id_char)*     { \s -> TokenAdd s }
  "-" (@id_char)*     { \s -> TokenSub s }

  "==" (@id_char)*    { \s -> TokenEq s }
  "/=" (@id_char)*    { \s -> TokenNeq s }
  "<" (@id_char)*     { \s -> TokenLt s }
  ">" (@id_char)*     { \s -> TokenGt s }
  "<=" (@id_char)*    { \s -> TokenLe s }
  ">=" (@id_char)*    { \s -> TokenGe s }

  "->" (@id_char)*    { \s -> TokenRArrow s }
  "<-" (@id_char)*    { \s -> TokenLArrow s }

  "&&"                { \s -> TokenAnd s }
  "||"                { \s -> TokenOr s }

-- identifier
  @identifier         { \s -> TokenId s }
  "`" @identifier "`" { \s -> TokenQuotedId (init (tail s)) }


{
lex :: String -> [Token]
lex = alexScanTokens
}
