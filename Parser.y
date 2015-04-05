{
{-# LANGUAGE TupleSections #-}
module Parser where

import Prelude hiding (lex)

import Control.Monad.State

import GHC.Exts (sortWith)

import Config
import Data.Exception
import Data.Expr
import Data.Functor ((<$>))
import Data.LexState
import Data.Module
import Data.QualName (QualName)
import qualified Data.QualName as QualName (mkQualName, fromQualName)
import Data.Source
import Data.Token
import Lexer
import Monad.ParserM
import qualified Monad.ParserM as ParserM
import qualified Stage.IndentLexer as IndentLexer
import Utils
}

%monad { ParserM }
%lexer { nextToken } { TokenEOF }

%name parseModule Module
%name parseDefnOrExpr DefnOrExpr

%tokentype { Token }
%error { failM }

%token
  -- punctuation
  '@'     { TokenAt _ }
  ' @'    { TokenAtSpace _ }
  '.'     { TokenDot _ }
  ','     { TokenComma _ }
  '='     { TokenEquiv _ }

  -- grouping
  '('     { TokenLParen _ }
  ')'     { TokenRParen _ }
  '['     { TokenLConsParen _ }
  ']'     { TokenRConsParen _ }
  '{'     { TokenLEnvParen }
  '}'     { TokenREnvParen }

  -- keywords
  as      { TokenAs _ }
  let     { TokenLet _ }
  me      { TokenMe _ }
  in      { TokenIn _ }
  type    { TokenType _ }
  use     { TokenUse _ }
  where   { TokenWhere _ }

  -- literals
  character { TokenChar _ $$ }
  integer   { TokenInt _ $$ }
  double    { TokenDouble _ $$ }
  string    { TokenString _ $$ }

  -- operators
  'o'     { TokenComposition _ $$ }

  '*'     { TokenMult _ $$ }
  '/'     { TokenDiv _ $$ }
  '+'     { TokenAdd _ $$ }
  '-'     { TokenSub _ $$ }

  '+>'    { TokenCons _ $$ }
  '<+'    { TokenSnoc _ $$ }

  '=='    { TokenEq _ $$ }
  '/='    { TokenNeq _ $$ }
  '<'     { TokenLt _ $$ }
  '>'     { TokenGt _ $$ }
  '<='    { TokenLe _ $$ }
  '>='    { TokenGe _ $$ }

  '&&'    { TokenAnd _ $$ }
  '||'    { TokenOr _ $$ }

  -- identifier
  id       { TokenId _ $$ }
  quotedId { TokenQuotedId _ $$ }
  typeId   { TokenTypeId _ $$ }

  -- eof   { TokenEOF }


-- Precedence (lower)
%left     where                         -- where
%nonassoc let_prec                      -- let
%left     lambda_prec                   -- lambda
%left     '$'                           -- dollar application
%left     '||'                          -- or
%left     '&&'                          -- and
%left     '==' '/=' '<' '>' '<=' '>='   -- comparison
%left     '<+'                          -- snoc
%right    '+>'                          -- cons
%left     '+' '-'                       -- addition
%left     '*' '/'                       -- multiplication
%right    '**'                          -- exponentiation
%left     'o'                           -- composition
%left     quotedId                      -- functions as operators
%nonassoc below_app_prec                -- below application (e.g., single id)
%left     app_prec                      -- application
%nonassoc below_at_prec
%nonassoc '@' ' @'
%nonassoc '(' '[' character integer double string id typeId
%nonassoc above_id_prec
-- /Precedence (greater)

%%

DefnOrExpr :: { Source }
DefnOrExpr:
    Defn { $1 }
  | Expr {% ensureExpr $1 }

Module :: { Source }
Module:
    me TypeName UseList DefnList { ModuleS (QualName.fromQualName $2) $3 $4 }
  | me TypeName DefnList         { ModuleS (QualName.fromQualName $2) [] $3 }

UseList :: { [(String, String)] }
UseList:
    UseList use TypeName as TypeName { $1 ++ [(QualName.fromQualName $3, QualName.fromQualName $5)] }
  | UseList use TypeName             { $1 ++ [(QualName.fromQualName $3, "")] }
  | use TypeName as TypeName         { [(QualName.fromQualName $2, QualName.fromQualName $4)] }
  | use TypeName                     { [(QualName.fromQualName $2, "")] }

DefnList :: { [Source] }
DefnList:
    DefnList Defn { $1 ++ [$2] }
  | Defn          { [$1] }

Defn :: { Source }
Defn:
    FnDefn   { $1 }
  | TypeDefn { $1 }

FnDefn :: { Source }
FnDefn:
    let Pat CondExpr                        { FnDefS $2 $3 }
 |  let Pat CondExpr where '{' DefnList '}' { FnDefS $2 (WhereS $3 $6) }
 -- The following rule is an unfortunate artifact from the 'IndentLexer'.
 |  let Pat '{' CondExpr '}'                        { FnDefS $2 $4 }
 |  let Pat '{' CondExpr '}' where '{' DefnList '}' { FnDefS $2 (WhereS $4 $8) }

Expr :: { Source }
Expr:
    AppExpr         { listToApp $1 }

  | Expr 'o' Expr   { BinOpS $2 $1 $3 }

  | Expr '*' Expr   { BinOpS $2 $1 $3 }
  | Expr '/' Expr   { BinOpS $2 $1 $3 }

  | Expr '+' Expr   { BinOpS $2 $1 $3 }
  | Expr '-' Expr   { BinOpS $2 $1 $3 }

  | Expr '+>' Expr  { BinOpS $2 $1 $3 }
  | Expr '<+' Expr  { BinOpS $2 $1 $3 }

  | Expr '==' Expr  { BinOpS $2 $1 $3 }
  | Expr '/=' Expr  { BinOpS $2 $1 $3 }
  | Expr '<'  Expr  { BinOpS $2 $1 $3 }
  | Expr '>'  Expr  { BinOpS $2 $1 $3 }
  | Expr '<=' Expr  { BinOpS $2 $1 $3 }
  | Expr '>=' Expr  { BinOpS $2 $1 $3 }

  | Expr '&&' Expr  { AndS $1 $3 }
  | Expr '||' Expr  { OrS $1 $3 }

  | Expr quotedId Expr { BinOpS $2 $1 $3 }

  | Cond            { CondS $1 }

  | Let             { $1 }

CondExpr :: { Source }
CondExpr:
    Cond                               {% ensureExpr (CondS $1) }
  | '=' '{' Expr '}' %prec lambda_prec {% ensureExpr $3 }

Cond :: { [([Source], Source)] }
Cond:
    Cond AppExpr '=' '{' Expr '}' %prec lambda_prec { $1 ++ [($2, $5)] }
  |      AppExpr '=' '{' Expr '}' %prec lambda_prec { [($1, $4)] }

Let:
    let Pat CondExpr in Expr %prec let_prec {% ensureExpr $3 >>= \expr ->
                                                 return (LetS [FnDefS $2 expr] $5) }

TypeDefn :: { Source }
TypeDefn:
    type TypeName '=' '{' ConstructorList '}' { TypeDeclS $2 $5 }

ConstructorList :: { [(QualName, Source)] }
ConstructorList:
    ConstructorList TypeName Pat { $1 ++ [($2, $3)] }
  | TypeName Pat             { [($1, $2)] }

-- patterns

AppExpr :: { [Source] }
AppExpr:
    Pat         %prec below_app_prec { [$1] }
  | AppExpr Pat %prec app_prec       { $1 ++ [$2] }

Pat :: { Source }
Pat:
    QualName '@'  SimplePat %prec above_id_prec { bindPat (QualName.fromQualName $1) $3 }
  |          '@'  QualName                      { IdS $2 }
  |          ' @' QualName                      { IdS $2 }
  |          '@'                                { allPat }
  |          ' @'                               { allPat }
  |               SimplePat                     { case $1 of
                                                    IdS name -> bindPat (QualName.fromQualName name) allPat
                                                    _ -> $1 }

SimplePat :: { Source }
SimplePat:
    QualName %prec below_at_prec { IdS $1 }
  | character                    { CharS $1 }
  | integer                      { IntS $1 }
  | double                       { RealS $1 }
  | string                       { StringS $1 }
  | '[' ExprList ']'             { SeqS $2 }
  | '['          ']'             { SeqS [] }
  | TypeName                     { idS (QualName.fromQualName $1) }
  | '(' Expr ')'                 { $2 }
  | '{' Expr '}'                 { $2 }

ExprList :: { [Source] }
ExprList:
    ExprList ',' Expr { $1 ++ [$3] }
  | Expr              { [$1] }

-- identifiers

QualName :: { QualName }
QualName:
    LongTypeId '.' Name { QualName.mkQualName ($1 ++ [$3]) }
  | Name                { QualName.mkQualName [$1] }

TypeName :: { QualName }
TypeName:
    LongTypeId { QualName.mkQualName $1 }

LongTypeId :: { [String] }
LongTypeId:
    LongTypeId '.' typeId { $1 ++ [$3] }
  | typeId                { [$1] }

Name :: { String }
Name:
    '(' Operator ')' { $2 }
  | id               { $1 }

Operator:
    'o'         { $1 }

  | '*'         { $1 }
  | '/'         { $1 }
  | '+'         { $1 }
  | '-'         { $1 }

  | '+>'        { $1 }
  | '<+'        { $1 }

  | '=='        { $1 }
  | '/='        { $1 }
  | '<'         { $1 }
  | '>'         { $1 }
  | '<='        { $1 }
  | '>='        { $1 }

  | '&&'        { $1 }
  | '||'        { $1 }

{
nextToken :: (Token -> ParserM a) -> ParserM a
nextToken cont =
  do tokens <- psTokens <$> get
     case tokens of
       [] -> cont TokenEOF
       (token:tokens') ->
         do modify $ \s -> s { psTokens = tokens' }
            cont token

runParser :: ParserM Source -> String -> String -> Either String Source
runParser m filename str =
  case runStateT m $ ParserM.initial filename (IndentLexer.indentLex filename str) of
    Right (mod, _) -> Right mod
    Left str -> Left str

parseFile :: String -> String -> Either String Source
parseFile filename str = runParser parseModule filename str

parseRepl :: String -> String -> Either String Source
parseRepl filename str =
  evalStateT parseDefnOrExpr $ ParserM.initial filename $
    IndentLexer.indentLex filename str
}
