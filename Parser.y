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
import Utils

ensureExpr :: Source -> ParserM Source
ensureExpr val =
  case toSource val of
    Nothing -> failM "expecting expression instead of pattern"
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
  '@id'   { TokenAtId $$ }
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
  type    { TokenType }
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
%left '<+'                        -- snoc
%right '+>'                       -- cons
%left '==' '/=' '<' '>' '<=' '>=' -- comparison
%left '+' '-'                     -- additives
%left '*' '/'                     -- multiplicatives
%left 'o'                         -- composition
%left quotedId                    -- functions as operators
%nonassoc below_app_prec          -- below application (e.g., single id)
%left app_prec                    -- application
%nonassoc '(' '[' '[|' character integer double string id typeId
%nonassoc '@'
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
    def Name Expr {% return . FnDeclS $2 =<< ensureExpr $3 }

Expr :: { Source }
Expr:
    AppExpr         { listToApp $1 }

  | Expr 'o' Expr   { BinOpS $2 $1 $3 }

  | Expr '*' Expr   { BinOpS $2 $1 $3 }
  | Expr '/' Expr   { BinOpS $2 $1 $3 }

  | Expr '+' Expr   { BinOpS $2 $1 $3 }
  | Expr '-' Expr   { BinOpS $2 $1 $3 }

  | Expr '==' Expr  { BinOpS $2 $1 $3 }
  | Expr '/=' Expr  { BinOpS $2 $1 $3 }
  | Expr '<'  Expr  { BinOpS $2 $1 $3 }
  | Expr '>'  Expr  { BinOpS $2 $1 $3 }
  | Expr '<=' Expr  { BinOpS $2 $1 $3 }
  | Expr '>=' Expr  { BinOpS $2 $1 $3 }

  | Expr '+>' Expr  { BinOpS $2 $1 $3 }
  | Expr '<+' Expr  { BinOpS $2 $1 $3 }

  | Expr '&&' Expr  { AndS $1 $3 }
  | Expr '||' Expr  { OrS $1 $3 }

  | Expr quotedId Expr { BinOpS $2 $1 $3 }

  | Expr where '{' DefnList '}' { WhereS $1 $4 }

  | Cond            { CondS $1 }

Cond :: { [([Source], Source)] }
Cond:
    Cond '|' AppExpr '=' Expr %prec lambda_prec { $1 ++ [($3, $5)] }
  | AppExpr '=' Expr          %prec lambda_prec { [($1, $3)] }

TypeDefn :: { Source }
TypeDefn:
    type TypeName ConstructorList { TypeDeclS $2 $3 }

ConstructorList :: { [(QualName, Source)] }
ConstructorList:
    ConstructorList '|' TypeName Pat { $1 ++ [($3, $4)] }
  | TypeName Pat                     { [($1, $2)] }

-- patterns

AppExpr :: { [Source] }
AppExpr:
    Pat         %prec below_app_prec { [$1] }
  | AppExpr Pat %prec app_prec       { $1 ++ [$2] }

Pat :: { Source }
Pat:
    id '@id'         { bindPat $1 (IdS (QualName.mkQualName [$2])) }
  | id '@' SimplePat { bindPat $1 $3 }
  | id '@'           { bindPat $1 allPat }
  |    '@'           { allPat }
  |        SimplePat { $1 }

SimplePat :: { Source }
SimplePat:
    QualName            { IdS $1 }
  | character           { CharS $1 }
  | integer             { IntS $1 }
  | double              { RealS $1 }
  | '[' ExprList ']'    { SeqS $2 }
  | '['          ']'    { SeqS [] }
  | string              { StringS $1 }
  | TypeName            { idS (QualName.fromQualName $1) }
  | '(' Expr ')'        { $2 }

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

runParser :: ParserM Source -> String -> String -> Either String Source
runParser m filename str =
  case runStateT m $ ParserM.initial $ lexState filename str of
    Right (mod, _) -> Right mod
    Left str -> Left str

parseFile :: String -> String -> Either String Source
parseFile filename str = runParser parseModule filename str

parseRepl :: String -> String -> Either String Source
parseRepl filename str =
  evalStateT parseDefnOrExpr $ ParserM.initial $ lexState filename str
}
