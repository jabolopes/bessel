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
import Data.Name (Name)
import qualified Data.Name as Name
import Data.Source
import Data.Token
import Lexer
import Monad.ParserM
import qualified Monad.ParserM as ParserM
import qualified Stage.IndentLexer as IndentLexer
import Typechecker.Type (Type(..))
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

  -- types
  '->'     { TokenArrow _ }
  ':'     { TokenColon _ }

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
%right    '->'
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
    me TypeName UseList DefnList { ModuleS $2 $3 $4 }
  | me TypeName DefnList         { ModuleS $2 [] $3 }

UseList :: { [(Name, Name)] }
UseList:
    UseList use TypeName as TypeName { $1 ++ [($3, $5)] }
  | UseList use TypeName             { $1 ++ [($3, Name.empty)] }
  | use TypeName as TypeName         { [($2, $4)] }
  | use TypeName                     { [($2, Name.empty)] }

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
    let Pat CondExpr                                 { FnDefS $2 Nothing $3 [] }
  | let Pat ':' Type CondExpr                        { FnDefS $2 (Just $4) $5 [] }
  | let Pat CondExpr where '{' DefnList '}'          { FnDefS $2 Nothing $3 $6 }
  | let Pat ':' Type CondExpr where '{' DefnList '}' { FnDefS $2 (Just $4) $5 $8 }
  -- The following rules are an unfortunate artifact from 'IndentLexer'.
  | let Pat '{' CondExpr '}'                         { FnDefS $2 Nothing $4 [] }
  | let Pat ':' Type '{' CondExpr '}'                { FnDefS $2 (Just $4) $6 [] }
  | let Pat '{' CondExpr '}' where '{' DefnList '}'  { FnDefS $2 Nothing $4 $8 }
  | let Pat ':' Type'{' CondExpr '}' where '{' DefnList '}' { FnDefS $2 (Just $4) $6 $10 }

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
                                                 return (LetS [FnDefS $2 Nothing expr []] $5) }

TypeDefn :: { Source }
TypeDefn:
    type TypeName '=' '{' ConstructorList '}' { TypeDeclS $2 $5 }

ConstructorList :: { [(Name, Source)] }
ConstructorList:
    ConstructorList TypeName Pat { $1 ++ [($2, $3)] }
  | TypeName Pat                 { [($1, $2)] }

-- patterns

AppExpr :: { [Source] }
AppExpr:
    Pat         %prec below_app_prec { [$1] }
  | AppExpr Pat %prec app_prec       { $1 ++ [$2] }

Pat :: { Source }
Pat:
    QualName '@'  SimplePat %prec above_id_prec { bindPat $1 $3 }
  |          '@'  QualName                      { IdS $2 }
  |          ' @' QualName                      { IdS $2 }
  |          '@'                                { allPat }
  |          ' @'                               { allPat }
  |               SimplePat                     { case $1 of
                                                    IdS name -> bindPat name allPat
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
  | TypeName                     { IdS $1 }
  | '(' Expr ')'                 { $2 }
  | '{' Expr '}'                 { $2 }

ExprList :: { [Source] }
ExprList:
    ExprList ',' Expr { $1 ++ [$3] }
  | Expr              { [$1] }

-- identifiers

QualName :: { Name }
QualName:
    LongTypeId '.' Name { Name.untyped (Utils.flattenId $1 ++ "." ++ $3) }
  | Name                { Name.untyped $1 }

TypeName :: { Name }
TypeName:
    LongTypeId { Name.untyped (Utils.flattenId $1) }

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

Type :: { Type }
Type:
    TypeName                { PrimitiveT (Name.nameStr $1) }
  | Type '->' Type          { Arrow $1 $3 }

{
nextToken :: (Token -> ParserM a) -> ParserM a
nextToken cont =
  do tokens <- psTokens <$> get
     case tokens of
       [] -> cont TokenEOF
       (token:tokens') ->
         do modify $ \s -> s { psTokens = tokens' }
            cont token

runParser :: ParserM Source -> Name -> String -> Either String Source
runParser m modName str =
  case runStateT m $ ParserM.initial modName (IndentLexer.indentLex modName str) of
    Right (mod, _) -> Right mod
    Left str -> Left str

parseFile :: Name -> String -> Either String Source
parseFile modName str = runParser parseModule modName str

parseRepl :: Name -> String -> Either String Source
parseRepl modName str =
  evalStateT parseDefnOrExpr $ ParserM.initial modName $
    IndentLexer.indentLex modName str
}
