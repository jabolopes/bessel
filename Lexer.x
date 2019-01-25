{
{-# LANGUAGE NamedFieldPuns #-}
module Lexer where

import Prelude hiding (lex)

import Data.Exception
import Data.LexState
import Data.Token
}


%wrapper "posn"


$digit        = [0-9]
$lower_letter = [a-z]
$upper_letter = [A-Z]
$letter       = [a-zA-Z]

@symbol = "\\" | "!" | "#" | "$" | "%"
        | "/"  | "'" | "?" | "«" | "»"
        | "+"  | "*" | "´" | "º" | "ª"
        | "~"  | "^" | ";" | "-" | ">"
        | "<"

@id_char = $letter | $digit | @symbol

@whitespace = [\ \t\n\r]
@comment    = ("|-"([^\*]|[\r\n]|(\*+([^\|\/]|[\r\n])))*"-|")
            | ("--".*)

@character  = "'" ($letter | $digit) "'"
@integer    = ("-")?$digit+
@real       = ("-")?$digit+("."$digit+)?([eE]("-"|"+")?$digit+)?
@string     = \"[^\"]*\"
@identifier = $lower_letter (@id_char)*
@type_id    = $upper_letter (@id_char)*
@operator   = (@symbol)+

tokens :-
  -- ignore
  @whitespace         ;
  @comment            ;

  -- punctuation
  " @"                { \p _ -> TokenAtSpace (srcloc p) }
  "@"                 { \p _ -> TokenAt (srcloc p) }
  ","                 { \p _ -> TokenComma (srcloc p) }
  "."                 { \p _ -> TokenDot (srcloc p) }
  "="                 { \p _ -> TokenEquiv (srcloc p) }
  "|"                 { \p _ -> TokenAlternative (srcloc p) }

  -- grouping
  "("                 { \p _ -> TokenLParen (srcloc p) }
  ")"                 { \p _ -> TokenRParen (srcloc p) }
  "["                 { \p _ -> TokenLConsParen (srcloc p) }
  "]"                 { \p _ -> TokenRConsParen (srcloc p) }

  -- keywords
  "as"                { \p _ -> TokenAs (srcloc p) }
  "let"               { \p _ -> TokenLet (srcloc p) }
  "me"                { \p _ -> TokenMe (srcloc p) }
  "in"                { \p _ -> TokenIn (srcloc p) }
  "type"              { \p _ -> TokenType (srcloc p) }
  "use"               { \p _ -> TokenUse (srcloc p) }
  "where"             { \p _ -> TokenWhere (srcloc p) }

  -- literals
  @character          { \p s -> TokenChar (srcloc p) (head (tail s)) }
  @integer            { \p s -> TokenInt (srcloc p) (read s) }
  @real               { \p s -> TokenDouble (srcloc p) (read s) }
  @string             { \p s -> TokenString (srcloc p) (init (tail s)) }

  -- types
  "->"                { \p _ -> TokenArrow (srcloc p) }
  ":"                 { \p _ -> TokenColon (srcloc p) }

  -- operators
  "o"                 { \p s -> TokenComposition (srcloc p) s }

  "*" (@id_char)*     { \p s -> TokenMult (srcloc p) s }
  "/" (@id_char)*     { \p s -> TokenDiv (srcloc p) s }
  "+" (@id_char)*     { \p s -> TokenAdd (srcloc p) s }
  "-" (@id_char)*     { \p s -> TokenSub (srcloc p) s }

  "==" (@id_char)*    { \p s -> TokenEq (srcloc p) s }
  "/=" (@id_char)*    { \p s -> TokenNeq (srcloc p) s }
  "<"  (@id_char)*    { \p s -> TokenLt (srcloc p) s }
  ">"  (@id_char)*    { \p s -> TokenGt (srcloc p) s }
  "<=" (@id_char)*    { \p s -> TokenLe (srcloc p) s }
  ">=" (@id_char)*    { \p s -> TokenGe (srcloc p) s }

  "+>" (@id_char)*    { \p s -> TokenCons (srcloc p) s }
  "<+" (@id_char)*    { \p s -> TokenSnoc (srcloc p) s }

  "&&"                { \p s -> TokenAnd (srcloc p) s }
  "||"                { \p s -> TokenOr (srcloc p) s }

-- identifier
  @identifier         { \p s -> TokenId (srcloc p) s }
  "`" @identifier "`" { \p s -> TokenQuotedId (srcloc p) (init (tail s)) }
  @type_id            { \p s -> TokenTypeId (srcloc p) s }
  @operator           { \p s -> operatorFixity (srcloc p) s }

{
srcloc :: AlexPosn -> Srcloc
srcloc (AlexPn _ line column) = Srcloc line column

toAlexPosn :: Srcloc -> AlexPosn
toAlexPosn (Srcloc line column) = AlexPn 0 line column

toLexState :: AlexInput -> (Srcloc, Char, [Word8], String)
toLexState (pos, previousChar, str1, str2) =
  (srcloc pos, previousChar, str1, str2)

toAlexInput :: (Srcloc, Char, [Word8], String) -> AlexInput
toAlexInput (srcloc, previousChar, str1, str2) =
  (toAlexPosn srcloc, previousChar, str1, str2)

lex :: LexState -> (Token, LexState)
lex state@LexState { lexInput } = lex' (toAlexInput lexInput)
  where
    lex' :: AlexInput -> (Token, LexState)
    lex' input@(pos@(AlexPn _ line column), _, _, str) =
       case alexScan input 0 of
         AlexEOF -> (TokenEOF, state { lexInput = toLexState input })
         AlexError _ -> throwLexerException line column str
         AlexSkip  input' len -> lex' input'
         AlexToken input' len action ->
           (action pos (take len str), state { lexInput = toLexState input' })

lexTokens :: String -> Int -> String -> [Token]
lexTokens filename line str = yield (lex (lexState filename line str))
  where
    yield (TokenEOF, _) = []
    yield (token, state) = token:yield (lex state)
}
