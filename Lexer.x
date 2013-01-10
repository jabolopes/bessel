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

@identifier = @ident_char (@ident_char | $digit)*

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
  "::"			{ \_ -> TokenColonColon }
  ","			{ \_ -> TokenComma }
  "=f=>"		{ \_ -> TokenFnMetaPred }
  "<-"			{ \_ -> TokenLArrow}

  -- grouping
  "("			{ \_ -> TokenLParen }
  ")"			{ \_ -> TokenRParen }
  "["			{ \_ -> TokenLConsParen}
  "]"			{ \_ -> TokenRConsParen }
  "{"			{ \_ -> TokenLEnvParen }
  "}"			{ \_ -> TokenREnvParen }
  "[|"			{ \_ -> TokenLPredParen }
  "|]"			{ \_ -> TokenRPredParen }
  "<"			{ \_ -> TokenLSeqParen }
  ">"			{ \_ -> TokenRSeqParen }

  -- keywords
  "asn"			{ \_ -> TokenAsn }
  "def"			{ \_ -> TokenDef }
  "exdef"		{ \_ -> TokenExdef }
  "export"		{ \_ -> TokenExport }
  "forall"		{ \_ -> TokenForall }
  "hide"		{ \_ -> TokenHide }
  "lambda"		{ \_ -> TokenLambda }
  "meta"		{ \_ -> TokenMeta }
  "me"			{ \_ -> TokenMe }
  "module"		{ \_ -> TokenModule }
  "nrdef"		{ \_ -> TokenNrdef }
  "pat"			{ \_ -> TokenPat }
  "rec"			{ \_ -> TokenRec }
  "sig"			{ \_ -> TokenSig }
  "type"		{ \_ -> TokenType }
  "use"			{ \_ -> TokenUse }
  "where"		{ \_ -> TokenWhere }

  -- literals
  "`" @character	{ \(_:s:_) -> TokenChar s }
  @integer	  	{ \s -> TokenInt (read s) }
  @real		  	{ \s -> TokenDouble (read s) }
  \"[^\"]*\"    	{ \s -> TokenString (init (tail s)) }

  -- operators
  "'"			{ \_ -> TokenPrime }

  "~"			{ \_ -> TokenTilde }

  ":"			{ \_ -> TokenColon }

  ". "			{ \_ -> TokenPeriodSpace }
  "."			{ \_ -> TokenPeriod }

  "o"			{ \s -> TokenComposition s }

  "o."			{ \s -> TokenPatComposition "o" }

  "*"			{ \s -> TokenMult s }
  "/"			{ \s -> TokenDiv s }

  "+"			{ \s -> TokenAdd s }
  "-"			{ \s -> TokenSub s }

  "="			{ \s -> TokenEq s }

  "=>"			{ \s -> TokenPredAppendR s }
  "<="			{ \s -> TokenPredAppendL s }

  "=>."			{ \_ -> TokenPatAppendR "=>" }
  "<=."			{ \_ -> TokenPatAppendL "<=" }

  "&&"			{ \s -> TokenAnd s }
  "||"			{ \s -> TokenOr s }

  "&&."			{ \s -> TokenPatAnd "&&" }
  "||."			{ \s -> TokenPatOr "||" }

  "->"			{ \_ -> TokenRArrow}
  ";"			{ \_ -> TokenSemicolon }

  "|"			{ \s -> TokenReverse s }

  ":="			{ \_ -> TokenEquiv }

  "@"			{ \_ -> TokenAt "." }
    

-- identifier
  @name			{ \s -> TokenName s }


{
lex :: String -> [Token]
lex str =
    alexScanTokens str
}
