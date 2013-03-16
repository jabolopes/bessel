{
module Parser where

import Config
import Data.Exception
import Data.Stx
import Data.SrcFile
import Macros
import Utils

}

%name parseSrcFile SrcFile
%name parseRepl DefnOrExpr

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
        module  { TokenModule }
        nrdef   { TokenNrdef }
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

        '->'    { TokenRArrow $$ }
        '<-'    { TokenLArrow $$ }

        '&&'    { TokenAnd $$ }
        '||'    { TokenOr $$ }

        -- identifier
        id       { TokenId $$ }
        quotedId { TokenQuotedId $$ }


-- Precedence (lower)
%nonassoc guard_prec     -- guard
%left where              -- where
%left '&&' '||'          -- logical
%left '<-' '->'          -- arrow
%left '==' '/=' '<' '>' '<=' '>='  -- comparison
%left '+' '-'            -- additives
%left '*' '/'            -- multiplicatives
%left 'o'                -- composition
%left quotedId           -- functions as operators
%nonassoc below_app_prec -- below application (e.g., single id)
%left app_prec           -- application
%nonassoc '(' '[' '[|' character integer double string id
-- /Precedence (greater)

-- This is the type of the data produced by a successful reduction of the 'start'
-- symbol:
-- %type < Ast.Stx > start

%%

SrcFile:
    me LongId Namespace { mkParsedSrcFile (flattenId $2) $3 }

Namespace:
    UseList DefnList { Namespace $1 $2 }
  | UseList          { Namespace $1 [] }
  | DefnList         { Namespace [] $1 }

UseList:
    UseList use LongId as LongId { $1 ++ [(flattenId $3, flattenId $5)] }
  | UseList use LongId           { $1 ++ [(flattenId $3, "")] }
  | use LongId as LongId         { [(flattenId $2, flattenId $4)] }
  | use LongId                   { [(flattenId $2, "")] }

DefnList:
    DefnList Module          { $1 ++ [$2] }
  | DefnList Defn            { $1 ++ [$2] }
  | Module                   { [$1] }
  | Defn                     { [$1] }

Module:
    module        where '{' Namespace '}' { ModuleStx [] $4 }
  | module LongId where '{' Namespace '}' { ModuleStx $2 $5 }

DefnOrExpr:
    Defn        { $1 }
  | Expr        { $1 }

Defn:
    DefnKw Name DefnPatList    { defMacro $1 $2 $3 }
  | DefnKw Name '=' Expr       { DefnStx $1 $2 $4}
  | type   Name '=' Expr       { typeExprMacro $2 $4 }
  | type   Name '=' PatNoSpace { typePatMacro $2 $4 }

DefnPatList:
    DefnPatList '|' LambdaPatList '=' Expr  { $1 ++ [($3, $5)] }
  | LambdaPatList '=' Expr	      	    { [($1, $3)] }

DefnKw:
    def      { Def }
  | nrdef    { NrDef }

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

  | Expr '->' Expr  { binOpStx $2 $1 $3 }
  | Expr '<-' Expr  { binOpStx $2 $1 $3 }

  | Expr '&&' Expr  { andStx $1 $3 }
  | Expr '||' Expr  { orStx $1 $3 }

  | Expr quotedId Expr { binOpStx $2 $1 $3 }

  | Lambda          { lambdaMacro $1 }

  | Expr where '{' DefnList '}' { WhereStx $1 $4 }

Lambda:
    Lambda '|' LambdaPatList SimpleExpr { $1 ++ [($3, $4)] }
  | LambdaPatList SimpleExpr            { [($1, $2)] }

LambdaPatList:
    LambdaPatList Pat { $1 ++ [$2] }
  | Pat               { [$1] }

SimpleExpr:
    LongName	             { IdStx $1 }
  | Constant	             { $1 }
  | Seq			     { $1 }
  | '(' Expr ')'             { $2 }

Constant:
    character   { CharStx $1 }
  | integer     { if $1 >= 0 then IntStx $1 else appStx "negInt" (IntStx (- $1)) }
  | double      { if $1 >= 0 then DoubleStx $1 else appStx "negReal" (DoubleStx (- $1)) }

Seq:
    '[' ExprList ']'   { SeqStx $2 }
  | '['          ']'   { SeqStx [] }
  | string             { stringStx $1 }

Pat:
    '@ '         { mkPat constTrueStx [] [] }
  | id '@ '      { namePat $1 (mkPat constTrueStx [] []) }
  | '[' ']' '@ ' { Pat (applyStx "plist" []) [] }
  | PatRest      { $1 }

PatNoSpace:
    '@'         { mkPat constTrueStx [] [] }
  | id '@'      { namePat $1 (mkPat constTrueStx [] []) }
  | '[' ']' '@' { Pat (applyStx "plist" []) [] }
  | PatRest     { $1 }

PatRest:
    '(' OpPat ')'       { $2 }
  | ListPat             { $1 }
  | '@' LongName        { mkPat (IdStx $2) [] [] }
  | '@' ListPat         { $2 }
  | '@' '(' Expr ')'    { mkPat $3 [] [] }
  | id '@' LongName     { namePat $1 (mkPat (IdStx $3) [] []) }
  | id '@' ListPat      { namePat $1 $3 }
  | id '@' '(' Expr ')' { namePat $1 (mkPat $4 [] []) }

OpPat:
    Pat '->' PatNoSpace { mkPat (IdStx "pal") [IdStx "hd", IdStx "tl"] [$1, $3] }
  | Pat '<-' PatNoSpace { mkPat (IdStx "par") [IdStx "tlr", IdStx "hdr"] [$1, $3] }
  | Pat '&&' PatNoSpace { mkPat (IdStx "pand") [IdStx "id", IdStx "id"] [$1, $3] }
  | Pat '||' PatNoSpace { mkPat (IdStx "por") [IdStx "id", IdStx "id"] [$1, $3] }

ListPat:
    '[' PatList ']' { mkPat (IdStx "plist") [ appStx "s" (IntStx i) | i <- [1..length $2] ] $2 }

PatList:
    ExprList ',' PatNoSpace ',' PatList2 { map (\expr -> mkPat expr [] []) $1 ++ [$3] ++ $5 }
  | ExprList ',' PatNoSpace              { map (\expr -> mkPat expr [] []) $1 ++ [$3] }
  | PatNoSpace ',' PatList2              { $1 : $3 }
  | PatNoSpace                           { [$1] }

PatList2:
    Expr ',' PatList2        { mkPat $1 [] [] : $3 }
  | PatNoSpace  ',' PatList2 { $1 : $3 }
  | Expr                     { [mkPat $1 [] []] }
  | PatNoSpace               { [$1] }

ExprList:
    ExprList ',' Expr   { $1 ++ [$3] }
  | Expr                { [$1] }


-- identifiers

LongName:     
    LongName '.' Name { $1 ++ "." ++ $3 }
  | Name              { $1 }

LongId:
    LongId '.' id { $1 ++ [$3] }
  | id            { [$1] }

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

  | '->'        { $1 }
  | '<-'        { $1 }

  | '&&'        { $1 }
  | '||'        { $1 }

{

parseError :: [Token] -> a
parseError tks = throwParseException $ show tks

data Token
     -- punctuation
     = TokenAt
     | TokenAtSpace
     | TokenBar
     | TokenComma
     | TokenDot
     | TokenEquiv

     -- grouping
     | TokenLParen
     | TokenRParen
     | TokenLConsParen
     | TokenRConsParen
     | TokenLEnvParen
     | TokenREnvParen

     -- keywords
     | TokenAs
     | TokenDef
     | TokenMe
     | TokenModule
     | TokenNrdef
     | TokenType
     | TokenUse
     | TokenWhere

     -- literals
     | TokenChar   Char
     | TokenInt    Int
     | TokenDouble Double
     | TokenString String

     -- operators
     | TokenComposition String

     | TokenMult String
     | TokenDiv String
     | TokenAdd String
     | TokenSub String

     | TokenEq String
     | TokenNeq String
     | TokenLt String
     | TokenGt String
     | TokenLe String
     | TokenGe String

     | TokenRArrow String
     | TokenLArrow String

     | TokenAnd String
     | TokenOr String

     -- identifier
     | TokenId String
     | TokenQuotedId String
       deriving (Show)


parsePrelude = parseSrcFile

parseFile tks =
  let uses = [("Core", ""), (preludeName, "")] in
  addImplicitDeps uses (parseSrcFile tks)
}
