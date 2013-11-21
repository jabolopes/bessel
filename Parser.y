{
{-# LANGUAGE TupleSections #-}
module Parser where

import Prelude hiding (lex)

import Control.Monad.State

import GHC.Exts (sortWith)

import Config
import Data.Exception
import Data.Expr hiding (Pat)
import Data.Functor ((<$>))
import Data.LexState
import Data.Macro
import Data.Module
import Data.QualName (mkQualName)
import Data.Token
import Lexer
import Monad.ParserM
import qualified Monad.ParserM as ParserM
import Utils

ensureExpr :: Pat -> ParserM Macro
ensureExpr val =
  case toMacro val of
    Nothing -> failM "expecting expression but got pattern"
    Just expr -> return expr
}

%monad { ParserM }
%lexer { nextToken } { TokenEOF }

%name parseModule Module
%name parseDefnOrExpr DefnOrExpr

%tokentype { Token }
%error { parseError }

%token
  -- punctuation
  '@'     { TokenAt }
  '@ '    { TokenAtSpace }
  '|'     { TokenBar }
  '.'     { TokenDot }
  ','     { TokenComma }
  '='     { TokenEquiv }

  -- grouping
  '('     { TokenLParen }
  ')'     { TokenRParen }
  '['     { TokenLConsParen }
  ']'     { TokenRConsParen }
  '{'     { TokenLEnvParen }
  '}'     { TokenREnvParen }

  -- keywords
  as      { TokenAs }
  def     { TokenDef }
  me      { TokenMe }
  use     { TokenUse }
  where   { TokenWhere }

  -- literals
  character { TokenChar $$ }
  integer   { TokenInt $$ }
  double    { TokenDouble $$ }
  string    { TokenString $$ }

  -- operators
  'o'     { TokenComposition $$ }

  '*'     { TokenMult $$ }
  '/'     { TokenDiv $$ }
  '+'     { TokenAdd $$ }
  '-'     { TokenSub $$ }

  '=='    { TokenEq $$ }
  '/='    { TokenNeq $$ }
  '<'     { TokenLt $$ }
  '>'     { TokenGt $$ }
  '<='    { TokenLe $$ }
  '>='    { TokenGe $$ }

  '+>'    { TokenCons $$ }
  '<+'    { TokenSnoc $$ }

  '&&'    { TokenAnd $$ }
  '||'    { TokenOr $$ }

  -- identifier
  id       { TokenId $$ }
  quotedId { TokenQuotedId $$ }
  typeId   { TokenTypeId $$ }

  -- eof   { TokenEOF }


-- Precedence (lower)
%right '->'
%left lambda_prec                 -- lambda
%left where                       -- where
%left '&&' '||'                   -- logical
%left '+>' '<+'                   -- arrow
%left '==' '/=' '<' '>' '<=' '>=' -- comparison
%left '+' '-'                     -- additives
%left '*' '/'                     -- multiplicatives
%left 'o'                         -- composition
%left quotedId                    -- functions as operators
%nonassoc below_app_prec          -- below application (e.g., single id)
%left app_prec                    -- application
%nonassoc '(' '[' '[|' character integer double string id typeId
%nonassoc '@ '
-- /Precedence (greater)

%%

DefnOrExpr :: { Macro }
DefnOrExpr:
    Defn { $1 }
  | Expr { $1 }

Module :: { Macro }
Module:
    me LongTypeId UseList DefnList { ModuleM (flattenId $2) $3 $4 }
  | me LongTypeId DefnList         { ModuleM (flattenId $2) [] $3 }

UseList :: { [(String, String)] }
UseList:
    UseList use LongTypeId as LongTypeId { $1 ++ [(flattenId $3, flattenId $5)] }
  | UseList use LongTypeId               { $1 ++ [(flattenId $3, "")] }
  | use LongTypeId as LongTypeId         { [(flattenId $2, flattenId $4)] }
  | use LongTypeId                       { [(flattenId $2, "")] }

DefnList :: { [Macro] }
DefnList:
    DefnList Defn   { $1 ++ [$2] }
  | Defn            { [$1] }

Defn :: { Macro }
Defn:
    FnDefn { $1 }

FnDefn :: { Macro }
FnDefn:
    def Name Expr    { FnDeclM $2 $3 }

Expr :: { Macro }
Expr:
    SimpleExprPat %prec below_app_prec {% ensureExpr $1 }

  | Expr SimpleExprPat %prec app_prec {% AppM $1 <\$> ensureExpr $2 }

  | Expr 'o' Expr   { BinOpM $2 $1 $3 }

  | Expr '*' Expr   { BinOpM $2 $1 $3 }
  | Expr '/' Expr   { BinOpM $2 $1 $3 }

  | Expr '+' Expr   { BinOpM $2 $1 $3 }
  | Expr '-' Expr   { BinOpM $2 $1 $3 }

  | Expr '==' Expr  { BinOpM $2 $1 $3 }
  | Expr '/=' Expr  { BinOpM $2 $1 $3 }
  | Expr '<'  Expr  { BinOpM $2 $1 $3 }
  | Expr '>'  Expr  { BinOpM $2 $1 $3 }
  | Expr '<=' Expr  { BinOpM $2 $1 $3 }
  | Expr '>=' Expr  { BinOpM $2 $1 $3 }

  | Expr '+>' Expr  { BinOpM $2 $1 $3 }
  | Expr '<+' Expr  { BinOpM $2 $1 $3 }

  | Expr '&&' Expr  { AndM $1 $3 }
  | Expr '||' Expr  { OrM $1 $3 }

  | Expr quotedId Expr { BinOpM $2 $1 $3 }

  | Expr where '{' DefnList '}' { WhereM $1 $4 }

  | Lambda { CondM $1 }

Lambda :: { [([Pat], Macro)] }
Lambda:
    Lambda '|' PatList '=' Expr %prec lambda_prec { $1 ++ [($3, $5)] }
  | PatList '=' Expr            %prec lambda_prec { [($1, $3)] }

PatList :: { [Pat] }
PatList:
    PatList Pat         { $1 ++ [$2] }
  | Pat                 { [$1] }

-- patterns

Pat :: { Pat }
Pat:
    id '@ ' { bindPat $1 allPat }
  |    '@ ' { allPat }
  | PatRest { $1 }

PatNoSpace :: { Pat }
PatNoSpace:
    id '@'  { bindPat $1 allPat }
  |    '@'  { allPat }
  | PatRest { $1 }

PatRest :: { Pat }
PatRest:
  -- list patterns
    id '@' '(' ListPat ')' { bindPat $1 $4 }
  |        '(' ListPat ')' { $2 }

  -- expression patterns
  | id '@' SimpleExprPat   { bindPat $1 $3 }
  |    '@' SimpleExprPat   { $2 }

ListPat :: { Pat }
ListPat:
    Pat '+>' PatNoSpace    { listPat $1 $3 }

SimpleExprPat :: { Pat }
SimpleExprPat:
    QualName            { predicatePat (IdM $1) }
  | character           { predicatePat (CharM $1) }
  | integer             { predicatePat (IntM $1) }
  | double              { predicatePat (RealM $1) }
  | '[' ExprPatList ']' { tuplePat $2 }
  | '['             ']' { tuplePat [] }
  | string              { predicatePat (StringM $1) }
  | '(' Expr ')'        { predicatePat $2 }

ExprPatList :: { [Pat] }
ExprPatList:
    ExprPatList ',' PatNoSpace { $1 ++ [$3] }
  | ExprPatList ',' Expr       { $1 ++ [predicatePat $3] }
  | PatNoSpace                 { [$1] }
  | Expr                       { [predicatePat $1] }

-- identifiers

QualName:
    LongTypeId '.' Name { mkQualName ($1 ++ [$3]) }
  | Name                { mkQualName [$1] }

LongTypeId:
    LongTypeId '.' typeId { $1 ++ [$3] }
  | typeId                { [$1] }

Name:
    '(' Operator ')' { $2 }
  | id               { $1 }

Operator:
    'o'         { $1 }

  | '*'         { $1 }
  | '/'         { $1 }
  | '+'         { $1 }
  | '-'         { $1 }

  | '=='        { $1 }
  | '/='        { $1 }
  | '<'         { $1 }
  | '>'         { $1 }
  | '<='        { $1 }
  | '>='        { $1 }

  | '+>'        { $1 }
  | '<+'        { $1 }

  | '&&'        { $1 }
  | '||'        { $1 }

{
parseError :: Token -> ParserM a
parseError = failM . show

nextToken :: (Token -> ParserM a) -> ParserM a
nextToken cont =
  do (tk, state') <- lex . psLexerState <$> get
     modify $ \s -> s { psLexerState = state' }
     cont tk

runParser :: ParserM Macro -> String -> String -> Either String Macro
runParser m filename str =
  case runStateT m $ ParserM.initial $ lexState filename str of
    Right (mod, _) -> Right mod
    Left str -> Left str

parseFile :: String -> String -> Either String Macro
parseFile filename str = runParser parseModule filename str

parseRepl :: String -> String -> Either String Macro
parseRepl filename str =
  evalStateT parseDefnOrExpr $ ParserM.initial $ lexState filename str
}
