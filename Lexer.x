{
module Lexer where

import Parser
}


%wrapper "basic"

-- These are some regular expression definitions
$digit      = [0-9]
$letter     = [a-zA-Z]

-- EDIT: comments

@whitespace = [\ \t\n\r]
@comment    = ("|-"([^\*]|[\r\n]|(\*+([^\|\/]|[\r\n])))*"-|")
	    | ("-- ".*)

@code = ($letter | $digit | "-")+

@code_char = "\\" @code "\\" | "\\\\" | "\\\"\\"

@visible_char = "~" | "`" | "!" | "@" | "#" | "$"
	      | "%" | "^" | "&" | "*" | "(" | ")"
	      | "-" | "_" | "=" | "+" | "|" | "{"
	      | "}" | "[" | "]" | ":" | ";" | "'"
	      | "´" | "<" | "," | ">" | "." | "?"
	      | "/"

@special_char = @visible_char | @code_char
@character = $letter | $digit | @special_char

-- Numbers
@integer = ("-")?$digit+
@real    = ("-")?$digit+("."$digit+)?([eE]("-"|"+")?$digit+)?

@ident_char = $letter
	    | "/"
	    | "$"
	    | "%"
	    | "#"
	    | "_"
	    | "?"
	    | "^"
	    | "¬"

@identifier = @ident_char (@ident_char | $digit | "'")*

@name = @identifier
      | "&&"
      | "||"
      | "|->"
      | "<-|"
      | "+"
      | "-"
      | "*"
      | "/"
      | "@"
      | "="
      | "|"
      | "|=>"
      | "<="
      | "true"
      | "false"


tokens :-
  -- ignore
  @whitespace		;
  @comment		;

  -- symbols
  ","			{ \_ -> TokenComma }

  -- grouping
  "("			{ \_ -> TokenLParen }
  ")"			{ \_ -> TokenRParen }
  "["			{ \_ -> TokenLConsParen}
  "]"			{ \_ -> TokenRConsParen }
  "{"			{ \_ -> TokenLEnvParen }
  "}"			{ \_ -> TokenREnvParen }

  -- keywords
  "as"			{ \_ -> TokenAs }
  "def"			{ \_ -> TokenDef }
  "me"			{ \_ -> TokenMe }
  "module"		{ \_ -> TokenModule }
  "nrdef"		{ \_ -> TokenNrdef }
  "type"		{ \_ -> TokenType }
  "use"			{ \_ -> TokenUse }
  "where"		{ \_ -> TokenWhere }

  -- literals
  "`" @character	{ \(_:s:_) -> TokenChar s }
  @integer	  	{ \s -> TokenInt (read s) }
  @real		  	{ \s -> TokenDouble (read s) }
  \"[^\"]*\"    	{ \s -> TokenString (init (tail s)) }

  -- operators
  "~"			{ \_ -> TokenTilde }

  "@ "			{ \_ -> TokenAtSpace }
  "@"			{ \_ -> TokenAt }

  "o"			{ \s -> TokenComposition s }

  "*"			{ \s -> TokenMult s }
  "/"			{ \s -> TokenDiv s }

  "+"			{ \s -> TokenAdd s }
  "-"			{ \s -> TokenSub s }

  "=="			{ \s -> TokenEq s }
  "/="			{ \s -> TokenNeq s }
  "<"			{ \s -> TokenLt s }
  ">"			{ \s -> TokenGt s }
  "<="			{ \s -> TokenLe s }
  ">="			{ \s -> TokenGe s }

  "&&"			{ \s -> TokenAnd s }
  "||"			{ \s -> TokenOr s }

  "->"			{ \s -> TokenRArrow s }
  "<-"			{ \s -> TokenLArrow s }

  "|"			{ \_ -> TokenBar }

  "="			{ \_ -> TokenEquiv }

  "."			{ \_ -> TokenDot }
    

-- identifier
  @name			{ \s -> TokenName s }


{
lex :: String -> [Token]
lex str =
    alexScanTokens str
}
