{
{-# LANGUAGE NamedFieldPuns #-}
module Lexer where

import Prelude hiding (lex)

import Data.Exception
import Data.LexState
import Data.Token
}


%wrapper "basic"


$digit        = [0-9]
$lower_letter = [a-z]
$upper_letter = [A-Z]
$letter       = [a-zA-Z]

@symbol = "\\" | "!" | "#" | "$" | "%"
        | "&"  | "/" | "'" | "?" | "«"
        | "»"  | "+" | "*" | "´" | "º"
        | "ª"  | "~" | "^" | ";" | "-"

@id_char = $letter | $digit | @symbol

@whitespace = [\ \t\n\r]
@comment    = ("|-"([^\*]|[\r\n]|(\*+([^\|\/]|[\r\n])))*"-|")
            | ("-- ".*)

@character  = "'" ($letter | $digit) "'"
@integer    = ("-")?$digit+
@real       = ("-")?$digit+("."$digit+)?([eE]("-"|"+")?$digit+)?
@string     = \"[^\"]*\"
@identifier = $lower_letter (@id_char)*
@type_id    = $upper_letter (@id_char)*

tokens :-
  -- ignore
  @whitespace         ;
  @comment            ;

  -- punctuation
  "->"                { \_ -> TokenArrow }
  "@ "                { \_ -> TokenAtSpace }
  "@"                 { \_ -> TokenAt }
  "|"                 { \_ -> TokenBar }
  ":"                 { \_ -> TokenColon }
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
  "sig"               { \_ -> TokenSig }
  "type"              { \_ -> TokenType }
  "use"               { \_ -> TokenUse }
  "where"             { \_ -> TokenWhere }

  -- literals
  @character          { \s -> TokenChar (head (tail s)) }
  @integer            { \s -> TokenInt (read s) }
  @real               { \s -> TokenDouble (read s) }
  @string             { \s -> TokenString (init (tail s)) }

  -- operators
  "o"                 { \s -> TokenComposition s }

  "*" (@id_char)*     { \s -> TokenMult s }
  "/" (@id_char)*     { \s -> TokenDiv s }
  "+" (@id_char)*     { \s -> TokenAdd s }
  "-" (@id_char)*     { \s -> TokenSub s }

  "==" (@id_char)*    { \s -> TokenEq s }
  "/=" (@id_char)*    { \s -> TokenNeq s }
  "<"  (@id_char)*    { \s -> TokenLt s }
  ">"  (@id_char)*    { \s -> TokenGt s }
  "<=" (@id_char)*    { \s -> TokenLe s }
  ">=" (@id_char)*    { \s -> TokenGe s }

  "+>" (@id_char)*    { \s -> TokenCons s }
  "<+" (@id_char)*    { \s -> TokenSnoc s }

  "&&"                { \s -> TokenAnd s }
  "||"                { \s -> TokenOr s }

-- identifier
  @identifier         { \s -> TokenId s }
  "`" @identifier "`" { \s -> TokenQuotedId (init (tail s)) }
  @type_id            { \s -> TokenTypeId s }


{
lex :: LexState -> (Token, LexState)
lex state@LexState { endLine = n, input } = lex' n input
  where lex' n input@(_, _, str) =
          case alexScan input 0 of
            AlexEOF -> (TokenEOF, state { beginLine = n, endLine = n, input = input })
            AlexError _ -> throwLexException str
            AlexSkip  input' len -> lex' (line n (take len str)) input'
            AlexToken input' len action ->
	      (action (take len str), state { beginLine = n,
	      	      	    	      	      endLine = line n (take len str),
					      input = input' })

        line n = (n +) . length . filter (== '\n')


lexTokens :: String -> String -> [Token]
lexTokens filename str = yield (lex (lexState filename str))
  where yield (TokenEOF, _) = []
        yield (tk, state) = tk:yield (lex state)
}
