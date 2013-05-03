{
module Parser where

import Prelude hiding (lex)

import Data.List (intercalate)

import Config
import Data.Exception
import Data.SrcFile
import Data.Stx
import Data.Token
import Data.Type
import Lexer
import Macros
import Utils

}

%monad { P } { thenP } { returnP }
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
        module  { TokenModule }
        sig     { TokenSig }
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

-- This is the type of the data produced by a successful reduction of the 'start'
-- symbol:
-- %type < Ast.Stx > start

%%

SrcFile:
    me LongTypeId Namespace { mkParsedSrcFile (flattenId $2) $3 }

Namespace:
    UseList DefnList { Namespace $1 $2 }
  | UseList          { Namespace $1 [] }
  | DefnList         { Namespace [] $1 }

UseList:
    UseList use LongTypeId as LongTypeId { $1 ++ [(flattenId $3, flattenId $5)] }
  | UseList use LongTypeId               { $1 ++ [(flattenId $3, "")] }
  | use LongTypeId as LongTypeId         { [(flattenId $2, flattenId $4)] }
  | use LongTypeId                       { [(flattenId $2, "")] }

DefnList:
    DefnList Module { $1 ++ [$2] }
  | DefnList Defn   { $1 ++ [$2] }
  | Module          { [$1] }
  | Defn            { [$1] }

Module:
    module            where '{' Namespace '}' { ModuleStx [] $4 }
  | module LongTypeId where '{' Namespace '}' { ModuleStx $2 $5 }

DefnOrExpr:
    Defn { $1 }
  | Expr { $1 }

Defn:
    -- edit: ensure the name in the type ann is the same
    TypeAnn FnDefn  { let
                          (_, ann) = $1
                          (kw, name, body) = $2
                      in
                        DefnStx (Just ann) kw name body }

  | FnDefn          { let (kw, name, body) = $1 in
                      DefnStx Nothing kw name body }

  | type   typeId '=' PatNoSpace          { typeMacro $2 $4 }

FnDefn:
    def Name TypePatList DefnMatches { (Def, $2, LambdaMacro $3 (CondMacro $4 $2)) }
  | def Name TypePatList '=' Expr    { (Def, $2, LambdaMacro $3 $5) }
  | def Name DefnMatches             { (Def, $2, CondMacro $3 $2) }
  | def Name '=' Expr                { (Def, $2, $4) }

TypeAnn:
    sig Name ':' Type { ($2, $4) }

DefnMatches:
    DefnMatches '|' PredPatList '=' Expr  { $1 ++ [($3, $5)] }
  | PredPatList '=' Expr                  { [($1, $3)] }

Expr:
    SimpleExpr %prec below_app_prec { $1 }

  | Expr SimpleExpr %prec app_prec  { AppStx $1 $2 }

  | Expr 'o' Expr   { binOpStx $2 $1 $3 }

  | Expr '*' Expr   { binOpStx $2 $1 $3 }
  | Expr '/' Expr   { binOpStx $2 $1 $3 }

  | Expr '+' Expr   { binOpStx $2 $1 $3 }
  | Expr '-' Expr   { binOpStx $2 $1 $3 }

  | Expr '==' Expr  { binOpStx $2 $1 $3 }
  | Expr '/=' Expr  { binOpStx $2 $1 $3 }
  | Expr '<'  Expr  { binOpStx $2 $1 $3 }
  | Expr '>'  Expr  { binOpStx $2 $1 $3 }
  | Expr '<=' Expr  { binOpStx $2 $1 $3 }
  | Expr '>=' Expr  { binOpStx $2 $1 $3 }

  | Expr '+>' Expr  { binOpStx $2 $1 $3 }
  | Expr '<+' Expr  { binOpStx $2 $1 $3 }

  | Expr '&&' Expr  { andStx $1 $3 }
  | Expr '||' Expr  { orStx $1 $3 }

  | Expr quotedId Expr { binOpStx $2 $1 $3 }

  | Lambda          { $1 }

  | Expr where '{' DefnList '}' { WhereStx $1 $4 }

Lambda:
    TypePatList LambdaMatches { LambdaMacro $1 (CondMacro $2 "lambda") }
  | TypePatList SimpleExpr    { LambdaMacro $1 $2 }
  | LambdaMatches             { CondMacro $1 "lambda" }

LambdaMatches:
    LambdaMatches '|' PredPatList SimpleExpr { $1 ++ [($3, $4)] }
  | PredPatList SimpleExpr                   { [($1, $2)] }

TypePatList:
    TypePatList TypePat { $1 ++ [$2] }
  | TypePat             { [$1] }

PredPatList:
    PredPatList Pat { $1 ++ [$2] }
  | Pat             { [$1] }

SimpleExpr:
    LongName                 { IdStx $1 }
  | Constant                 { $1 }
  | Seq                      { $1 }
  | '(' Expr ')'             { $2 }

Constant:
    character   { CharStx $1 }
  | integer     { if $1 >= 0 then IntStx $1 else appStx "negInt" (IntStx (- $1)) }
  | double      { if $1 >= 0 then DoubleStx $1 else appStx "negReal" (DoubleStx (- $1)) }

Seq:
    '[' ExprList ']'   { SeqStx $2 }
  | '['          ']'   { SeqStx [] }
  | string             { stringStx $1 }


-- patterns

TypePat:
    id '@' typeId  { namePat $1 (mkPredPat (IdStx $3)) }

Pat:
    '@ '         { mkPredPat constTrueStx }
  | id '@ '      { namePat $1 (mkPredPat constTrueStx) }
  | PatRest      { $1 }

PatNoSpace:
    '@'         { mkPredPat constTrueStx }
  | id '@'      { namePat $1 (mkPredPat constTrueStx) }
  | PatRest     { $1 }

PatRest:
    ListPat              { $1 }
  | '(' OpPat ')'        { $2 }
  | '@' LongName         { mkPredPat (IdStx $2) }
  | '@' '(' Expr ')'     { mkPredPat $3 }
  | id '@' LongName      { namePat $1 (mkPredPat (IdStx $3)) }
  | id '@' ListPat       { namePat $1 $3 }
  | id '@' '(' OpPat ')' { namePat $1 $4 }
  | id '@' '(' Expr ')'  { namePat $1 (mkPredPat $4) }

OpPat:
    Pat '+>' PatNoSpace { mkPat (IdStx "pal") [IdStx "hd", IdStx "tl"] [$1, $3] }
  | Pat '<+' PatNoSpace { mkPat (IdStx "par") [IdStx "tlr", IdStx "hdr"] [$1, $3] }
  | Pat '&&' PatNoSpace { mkPat (IdStx "pand") [IdStx "id", IdStx "id"] [$1, $3] }
  | Pat '||' PatNoSpace { mkPat (IdStx "por") [IdStx "id", IdStx "id"] [$1, $3] }

ListPat:
    '[' PatList ']' { mkListPat $2 }
  | '@' '[' ']'     { Pat (applyStx "plist" []) [] }

PatList:
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

Type:
    TypeAux { rebuildForallT $1 }

TypeAux:
    TypeAux '->' TypeAux { ArrowT $1 $3 }
  | '(' TypeList ')'     { TupT $2 }
  | '[' TypeAux ']'      { SeqT $2 }
  | typeId               { case $1 of
                             "Bool" -> BoolT
                             "Int" -> IntT
                             "Real" -> DoubleT
                             "Char" -> CharT
                             "Dyn" -> DynT }
  | id                   { TvarT $1 }

TypeList:
    TypeList ',' TypeAux { $1 ++ [$3] }
  | TypeAux              { [$1] }


-- identifiers

LongName:     
    LongTypeId '.' Name { intercalate "." ($1 ++ [$3]) }
  | Name                { $1 }

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

parseError :: Token -> P a
parseError tk = failP (show tk)


type ParserM a = Either String a
type P a = AlexInput -> ParserM a


thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s ->
   case m s of 
       Right a -> k a s
       Left e -> Left e


returnP :: a -> P a
returnP a = \s -> Right a


failP :: String -> P a
failP err = \s -> Left err


catchP :: P a -> (String -> P a) -> P a
catchP m k = \s ->
   case m s of
      Right a -> Right a
      Left e -> k e s


parsePrelude :: String -> SrcFile
parsePrelude s =
  case parseSrcFile (lexState s) of
    Right srcfile -> srcfile
    Left str -> throwParseException str


parseRepl :: String -> Stx String
parseRepl s =
  case parseDefnOrExpr (lexState s) of
    Right stx -> stx
    Left str -> throwParseException str


parseFile :: String -> SrcFile
parseFile s =
  let uses = [("Core", ""), (preludeName, "")] in
  case parseSrcFile (lexState s) of
     Right srcfile -> addImplicitDeps uses srcfile
     Left str -> throwParseException str


nextToken :: (Token -> P a) -> P a
nextToken cont s =
  case lex s of
    (tk, s') -> cont tk s'
}
