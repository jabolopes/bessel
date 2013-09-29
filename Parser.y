{
{-# LANGUAGE TupleSections #-}
module Parser where

import Prelude hiding (lex)

import Control.Monad.State
import Data.List (intercalate, nub, partition, sort)

import GHC.Exts (sortWith)

import Config
import Data.Exception
import Data.Functor ((<$>))
import Data.LexState
import Data.SrcFile
import Data.Expr
import Data.QualName (mkQualName)
import Data.Token
import Data.Type
import Lexer
import Monad.ParserM
import qualified Monad.ParserM as ParserM
import Utils


checkUniqueImports :: [String] -> [(String, String)] -> ParserM ()
checkUniqueImports unprefixed prefixed
  | length (nub $ sort unprefixed) /= length unprefixed =
      failM "duplicated 'use' forms"
  | length (nub $ sort $ map fst prefixed) /= length (map fst prefixed) =
      failM "duplicated 'use' forms"
  | otherwise =
      return ()


checkUniqueQualifiers :: [(String, String)] -> ParserM ()
checkUniqueQualifiers prefixed
  | length (nub $ sort $ map snd prefixed) /= length (map snd prefixed) =
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

%name parseSrcFile SrcFile
%name parseDefnOrExpr DefnOrExpr

%tokentype { Token }
%error { parseError }

%token
  -- punctuation
  '->'    { TokenArrow }
  '@'     { TokenAt }
  '@ '    { TokenAtSpace }
  '|'     { TokenBar }
  '.'     { TokenDot }
  ':'     { TokenColon }
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
  '&'     { TokenAmpersand $$ }

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
%right '&'               -- and type operator (e.g., Int & Int)
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


SrcFile:
    me LongTypeId CheckUseList DefnList { mkParsedSrcFile (flattenId $2) $3 $4 }
  | me LongTypeId DefnList              { mkParsedSrcFile (flattenId $2) [] $3 }

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
  | TypeDefn { $1 }

FnDefn:
    def Name ':' Type DefnMatches { FnDecl Def $2 (CastE $4 (CondMacro $5 $2)) }
  | def Name ':' Type '=' Expr    { FnDecl Def $2 (CastE $4 $6) }

TypeDefn:
    type Cotype { CotypeDecl $2 }
  | type OrType { CotypeDecl $2 }

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
-- type patterns
    id '@' PatType         { namePat $1 (mkTypePat $3) }
  |    '@' PatType         { mkTypePat $2 }

-- predicate patterns
  | id '@' '(' Expr ')'    { namePat $1 (mkPredPat $4) }
  |    '@' '(' Expr ')'    { mkPredPat $3 }
  | id '@' QualName        { namePat $1 (mkPredPat (IdE $3)) }
  |    '@' QualName        { mkPredPat (IdE $2) }

-- list patterns
  | id '@' ListPat         { namePat $1 $3 }
  |        ListPat         { $1 }

-- combined patterns
  | id '@' '(' CombPat ')' { namePat $1 $4 }
  |        '(' CombPat ')' { $2 }

PatType:
    '(' MonotypeList ')' { TupT $2 }
  | '[' Monotype ']'     { SeqT $2 }
  | TypeId               { $1 }
  | '(' Monotype ')'     { $2 }

CombPat:
    Pat '+>' PatNoSpace { mkCombPat (idE "pal") TupT [idE "hd", idE "tl"] [$1, $3] }
  | Pat '<+' PatNoSpace { mkCombPat (idE "par") TupT [idE "tlr", idE "hdr"] [$1, $3] }
--  | Pat '&&' PatNoSpace { mkCombPat (idE "pand") DynT [idE "id", idE "id"] [$1, $3] }
--  | Pat '||' PatNoSpace { mkCombPat (idE "por") DynT [idE "id", idE "id"] [$1, $3] }
  | Pat '&'  PatNoSpace { mkAndPat [$1, $3] }

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


-- types

Monotype:
    Monotype '&' Monotype  { AndT $1 $3 }
  | Monotype '->' Monotype { ArrowT $1 $3 }
  | '(' MonotypeList ')'   { TupT $2 }
  | '[' Monotype ']'       { SeqT $2 }
  | TypeId                { $1 }
  | '(' Monotype ')'       { $2 }

MonotypeList:
    MonotypeList ',' Monotype { $1 ++ [$3] }
  | Monotype ',' Monotype     { [$1, $3] }

TypeId:
    typeId { case $1 of
               "Bool" -> BoolT
               "Int" -> IntT
               "Real" -> DoubleT
               "Char" -> CharT
               "Dyn" -> DynT }

Type:
    TypeAux { rebuildForallT $1 }

TypeAux:
    TypeAux '->' TypeAux { ArrowT $1 $3 }
  | TypeAux '&' TypeAux  { AndT $1 $3 }
  | '(' TypeList ')'     { TupT $2 }
  | '[' TypeAux ']'      { SeqT $2 }
  | Cotype               { $1 }
  | OrType               { $1 }
  | TypeId               { $1 }
  | id                   { VarT $1 }
  | '(' TypeAux ')'      { $2 }

TypeList:
    TypeList ',' TypeAux { $1 ++ [$3] }
  | TypeAux ',' TypeAux  { [$1, $3] }

Cotype:
    '{' CotypeObservations '}' { CoT (sortWith fst $2) }

CotypeObservations:
    CotypeObservations '|' QualName ':' Type { $1 ++ [($3, $5)] }
  | QualName ':' Type                        { [($1, $3)] }

OrType:
    '{' OrTypeCons '}' { OrT $2 }

OrTypeCons:
    OrTypeCons '|' TypeAux { $1 ++ [$3] }
  | TypeAux '|' TypeAux    { [$1, $3] }


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


runParser :: ParserM SrcFile -> [String] -> String -> String -> Either String SrcFile
runParser m deps filename str =
    case runStateT m $ ParserM.initial $ lexState filename str of
        Right (srcfile, s) -> Right srcfile { deps = nub $ sort $ deps ++ dependencies s }
        Left str -> Left str


parseFile :: String -> String -> Either String SrcFile
parseFile filename str =
    let deps = ["Core", preludeName] in
    addImplicitUnprefixedUses deps <$> runParser parseSrcFile deps filename str


parsePrelude :: String -> String -> Either String SrcFile
parsePrelude filename str =
    runParser parseSrcFile [] filename str


parseRepl :: String -> String -> Either String Expr
parseRepl filename str =
    evalStateT parseDefnOrExpr $ ParserM.initial $ lexState filename str
}
