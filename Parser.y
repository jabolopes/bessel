{
{-# LANGUAGE TupleSections #-}
module Parser where

import Prelude hiding (lex)

import Control.Monad.State
import Data.List (intercalate, nub, partition)

import GHC.Exts (sortWith)

import Config
import Data.Exception
import Data.Functor ((<$>))
import Data.LexState
import Data.Module
import Data.Expr
import Data.QualName (mkQualName)
import Data.Token
import Lexer
import Monad.ParserM
import qualified Monad.ParserM as ParserM
import Utils


checkUniqueImports :: [String] -> [(String, String)] -> ParserM ()
checkUniqueImports unprefixed prefixed
  | length (nub $ unprefixed) /= length unprefixed =
      failM "duplicated 'use' forms"
  | length (nub $ map fst prefixed) /= length (map fst prefixed) =
      failM "duplicated 'use' forms"
  | otherwise =
      return ()


checkUniqueQualifiers :: [(String, String)] -> ParserM ()
checkUniqueQualifiers prefixed
  | length (nub $ map snd prefixed) /= length (map snd prefixed) =
      failM "duplicated qualifiers in 'use' forms"
  | otherwise =
      return ()


checkUseList :: [(String, String)] -> ParserM ()
checkUseList uses =
  do let (unprefixed, prefixed) = partition (null . snd) uses
     checkUniqueImports (map fst unprefixed) prefixed
     checkUniqueQualifiers prefixed
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
%nonassoc guard_prec     -- guard
%left where              -- where
%left '&&' '||'          -- logical
%left '+>' '<+'          -- arrow
%left '==' '/=' '<' '>' '<=' '>='  -- comparison
%left '+' '-'            -- additives
%left '*' '/'            -- multiplicatives
%left 'o'                -- composition
%left quotedId           -- functions as operators
%nonassoc below_app_prec -- below application (e.g., single id)
%left app_prec           -- application
%nonassoc '(' '[' '[|' character integer double string id typeId
-- /Precedence (greater)

%%

DefnOrExpr:
    Defn { $1 }
  | Expr { $1 }


Module:
    me LongTypeId CheckUseList DefnList { mkParsedModule (flattenId $2) $3 $4 }
  | me LongTypeId DefnList              { mkParsedModule (flattenId $2) [] $3 }

CheckUseList:
    UseList {% checkUseList $1 >>
               mapM_ (addDependencyM . fst) $1 >>
               return $1 }

UseList:
    UseList use LongTypeId as LongTypeId { $1 ++ [(flattenId $3, flattenId $5)] }
  | UseList use LongTypeId               { $1 ++ [(flattenId $3, "")] }
  | use LongTypeId as LongTypeId         { [(flattenId $2, flattenId $4)] }
  | use LongTypeId                       { [(flattenId $2, "")] }

DefnList:
    DefnList Defn   { $1 ++ [$2] }
  | Defn            { [$1] }

Defn:
    FnDefn   { $1 }

FnDefn:
    def Name DefnMatches { FnDecl Def $2 (CondMacro $3 $2) }
  | def Name '=' Expr    { FnDecl Def $2 $4 }

DefnMatches:
    DefnMatches '|' PatList '=' Expr  { $1 ++ [($3, $5)] }
  | PatList '=' Expr                  { [($1, $3)] }

Expr:
    SimpleExpr %prec below_app_prec { $1 }

  | Expr SimpleExpr %prec app_prec  { AppE $1 $2 }

  | Expr 'o' Expr   { binOpE $2 $1 $3 }

  | Expr '*' Expr   { binOpE $2 $1 $3 }
  | Expr '/' Expr   { binOpE $2 $1 $3 }

  | Expr '+' Expr   { binOpE $2 $1 $3 }
  | Expr '-' Expr   { binOpE $2 $1 $3 }

  | Expr '==' Expr  { binOpE $2 $1 $3 }
  | Expr '/=' Expr  { binOpE $2 $1 $3 }
  | Expr '<'  Expr  { binOpE $2 $1 $3 }
  | Expr '>'  Expr  { binOpE $2 $1 $3 }
  | Expr '<=' Expr  { binOpE $2 $1 $3 }
  | Expr '>=' Expr  { binOpE $2 $1 $3 }

  | Expr '+>' Expr  { binOpE $2 $1 $3 }
  | Expr '<+' Expr  { binOpE $2 $1 $3 }

  | Expr '&&' Expr  { andE $1 $3 }
  | Expr '||' Expr  { orE $1 $3 }

  | Expr quotedId Expr { binOpE $2 $1 $3 }

  | Expr where '{' DefnList '}' { WhereE $1 $4 }

  | Lambda          { CondMacro $1 "lambda" }
  | Merge           { $1 }

Lambda:
    Lambda '|' PatList SimpleExpr { $1 ++ [($3, $4)] }
  | PatList SimpleExpr            { [($1, $2)] }

PatList:
    PatList Pat         { $1 ++ [$2] }
  | Pat                 { [$1] }

Merge:
    '{' MergeObservations '}' { MergeE (sortWith fst $2) }

MergeObservations:
    MergeObservations '|' QualName '=' Expr { $1 ++ [($3, $5)] }
  | QualName '=' Expr                       { [($1, $3)] }

SimpleExpr:
    QualName     { IdE $1 }
  | Constant     { $1 }
  | '(' Expr ')' { $2 }

Constant:
    character        { CharE $1 }
  | integer          { if $1 >= 0 then IntE $1 else appE "negInt" (IntE (- $1)) }
  | double           { if $1 >= 0 then RealE $1 else appE "negReal" (RealE (- $1)) }
  | '[' ExprList ']' { SeqE $2 }
  | '['          ']' { idE "null#" }
  | string           { stringE $1 }


-- patterns

Pat:
    id '@ ' { namePat $1 mkAllPat }
  |    '@ ' { mkAllPat }
  | PatRest { $1 }

PatNoSpace:
    id '@'  { namePat $1 mkAllPat }
  |    '@'  { mkAllPat }
  | PatRest { $1 }

PatRest:
-- predicate patterns
    id '@' '(' Expr ')'    { namePat $1 (mkPredPat $4) }
  |    '@' '(' Expr ')'    { mkPredPat $3 }
  | id '@' QualName        { namePat $1 (mkPredPat (IdE $3)) }
  |    '@' QualName        { mkPredPat (IdE $2) }

-- list patterns
  | id '@' ListPat         { namePat $1 $3 }
  |        ListPat         { $1 }

-- combined patterns
  | id '@' '(' CombPat ')' { namePat $1 $4 }
  |        '(' CombPat ')' { $2 }

CombPat:
    Pat '+>' PatNoSpace { mkCombPat (idE "isList") [idE "hd", idE "tl"] [$1, $3] }
--  | Pat '&&' PatNoSpace { mkCombPat (idE "pand") [idE "id", idE "id"] [$1, $3] }
--  | Pat '||' PatNoSpace { mkCombPat (idE "por") [idE "id", idE "id"] [$1, $3] }

ListPat:
    '[' ExprPatList ']' { mkListPat $2 }
  | '@' '[' ']'         { mkListPat [] }

ExprPatList:
    ExprList ',' PatNoSpace ',' PatList2 { map mkPredPat $1 ++ [$3] ++ $5 }
  | ExprList ',' PatNoSpace              { map mkPredPat $1 ++ [$3] }
  | PatNoSpace ',' PatList2              { $1:$3 }
  | PatNoSpace                           { [$1] }

PatList2:
    Expr ',' PatList2        { mkPredPat $1:$3 }
  | PatNoSpace  ',' PatList2 { $1:$3 }
  | Expr                     { [mkPredPat $1] }
  | PatNoSpace               { [$1] }

ExprList:
    ExprList ',' Expr   { $1 ++ [$3] }
  | Expr                { [$1] }

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
    do (tk, state') <- lex . lexerState <$> get
       modify $ \s -> s { lexerState = state' }
       cont tk


runParser :: ParserM Module -> [String] -> String -> String -> Either String Module
runParser m deps filename str =
    case runStateT m $ ParserM.initial $ lexState filename str of
        Right (mod, s) -> Right mod { modDeps = nub $ deps ++ dependencies s }
        Left str -> Left str


parseFile :: String -> String -> Either String Module
parseFile filename str =
    let deps = ["Core", preludeName] in
    addImplicitUnprefixedUses deps <$> runParser parseModule deps filename str


parsePrelude :: String -> String -> Either String Module
parsePrelude filename str =
    runParser parseModule [] filename str


parseRepl :: String -> String -> Either String Expr
parseRepl filename str =
    evalStateT parseDefnOrExpr $ ParserM.initial $ lexState filename str
}
