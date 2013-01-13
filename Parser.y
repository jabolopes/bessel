{
{-# LANGUAGE DeriveDataTypeable #-}
module Parser where

import Data.Exception
import Data.Stx
import Data.Pat
import Data.SrcFile
import Macros

}

%name parseSrcFile SrcFile
%name parseRepl DefnOrExpr

%tokentype { Token }
%error { parseError }

%token
        -- symbols
        ','     { TokenComma }
        '<-'    { TokenLArrow }

        -- grouping
        '('     { TokenLParen }
        ')'     { TokenRParen }
        '['     { TokenLConsParen }
        ']'     { TokenRConsParen }
        '{'     { TokenLEnvParen }
        '}'     { TokenREnvParen }
        '[|'    { TokenLPredParen }
        '|]'    { TokenRPredParen }
        '<'     { TokenLSeqParen }
        '>'     { TokenRSeqParen }

        -- keywords
        def     { TokenDef }
        exdef   { TokenExdef }
        lambda  { TokenLambda }
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
        '\''    { TokenPrime }

        '~'     { TokenTilde }

        ':'     { TokenColon }

        '.'     { TokenDot }

        'o'     { TokenComposition $$ }

        '*'     { TokenMult $$ }
        '/'     { TokenDiv $$ }

        '+'     { TokenAdd $$ }
        '-'     { TokenSub $$ }

        '='     { TokenEq $$ }

        '=>'   { TokenPredAppendR $$ }
        '<='   { TokenPredAppendL $$ }

        '&&'    { TokenAnd $$ }
        '||'    { TokenOr $$ }

        '->'    { TokenRArrow }
        ';'     { TokenSemicolon }

        '|'     { TokenReverse $$ }

        ':='    { TokenEquiv }

        '@'     { TokenAt }
        '@ '    { TokenAtSpace }

        -- identifier
        name      { TokenName $$ }


-- Precedence (lower)
                        -- definition
%left lambda_prec       -- lambda
%left where             -- where
%left '|'               -- reverse composition
%right '->' ';'         -- condition
%left name              -- infix name op
%left '&&' '||'         -- logical
%left '=>' '<='         -- predicate appends
%left '->' '<-'         -- arrows
%left '='               -- equality
%left '+' '-'           -- additives
%left '*' '/'           -- multiplicatives
%left 'o'               -- composition
-- %left '@' '@ '          -- pattern
%left ':'               -- application
%nonassoc '~'           -- constant
%nonassoc '\''          -- lift
-- /Precedence (greater)

-- This is the type of the data produced by a successful reduction of the 'start'
-- symbol:
-- %type < Ast.Stx > start

%%

SrcFile:
    me NameDotList Namespace { SrcFile $2 [] Nothing (Left $3) }

NameDotList:
    NameDotList '.' name { $1 ++ "." ++ $3 }
  | name                 { $1 }

Namespace:
    UseList DefnList { Namespace $1 $2 }
  | UseList          { Namespace $1 [] }
  | DefnList         { Namespace [] $1 }

UseList:
    UseList use NameDotList { $1 ++ [$3] }
  | use NameDotList         { [$2] }

DefnList:
    DefnList Module          { $1 ++ [$2] }
  | DefnList Defn            { $1 ++ [$2] }
  | Module                   { [$1] }
  | Defn                     { [$1] }

Module:
    module where '{' Namespace '}'      { ModuleStx "" $4 }
  | module name where '{' Namespace '}' { ModuleStx $2 $5 }

DefnOrExpr:
    Defn        { $1 }
  | Expr        { $1 }

Defn:
    DefnKw OpOrName DefnPatList '<-' Expr ':=' Expr    { defExprMacro $1 $2 $3 $5 $7 }
  | DefnKw OpOrName DefnPatList '<-' Pat  ':=' Expr    { defPatMacro $1 $2 $3 $5 $7 }
  | DefnKw OpOrName DefnPatList           ':=' Expr    { defMacro $1 $2 $3 $5 }
  | DefnKw OpOrName             '<-' Expr ':=' Expr    { defExprMacro $1 $2 [] $4 $6 }
  | DefnKw OpOrName             '<-' Pat  ':=' Expr    { defPatMacro $1 $2 [] $4 $6 }
  | DefnKw OpOrName                       ':=' Expr    { DefnStx $1 $2 $4 }

  | exdef  Pat      DefnPatList '<-' Expr ':=' Expr    { case exdefExprMacro $2 $3 $5 $7 of
                                                           [] -> error "exdef: empty pattern"
                                                           stxs -> SeqStx stxs }
  | exdef  Pat      DefnPatList '<-' Pat  ':=' Expr    { case exdefPatMacro $2 $3 $5 $7 of
                                                           [] -> error "exdef: empty pattern"
                                                           stxs -> SeqStx stxs }
  | exdef  Pat      DefnPatList           ':=' Expr    { case exdefMacro $2 $3 $5 of
                                                           [] -> error "exdef: empty pattern"
                                                           stxs -> SeqStx stxs }
  | exdef  Pat                  '<-' Expr ':=' Expr    { case exdefExprMacro $2 [] $4 $6 of
                                                           [] -> error "exdef: empty pattern"
                                                           stxs -> SeqStx stxs }
  | exdef  Pat                  '<-' Pat  ':=' Expr    { case exdefPatMacro $2 [] $4 $6 of
                                                           [] -> error "exdef: empty pattern"
                                                           stxs -> SeqStx stxs }
  | exdef  Pat                            ':=' Expr    { case exdefMacro $2 [] $4 of
                                                           [] -> error "exdef: empty pattern"
                                                           stxs -> SeqStx stxs }

  | type  OpOrName                        ':=' Expr        { typeExprMacro $2 $4 }
  | type  OpOrName                        ':=' PatNoSpace  { typePatMacro $2 $4 }
--| asn   Expr         ':=' Expr        { AsnStx  $2 $4 }
--| sig   OpOrName     	    		  '::' MetaPred    { SigStx  $2 $4 }

DefnKw:
    def      { Def }
  | nrdef    { NrDef }

DefnPatList:
    DefnPatList Pat { $1 ++ [$2] }
  | Pat             { [$1] }

Expr:
    Atom                          { $1 }

  | OpOrName                      { IdStx $1 }

  | Seq                           { $1 }
  | Cond                          { $1 }

  | Expr '\''                     { AppStx (IdStx "lift") $1 }
  | '~' Expr                      { AppStx (IdStx "K") $2 }
  | '~' PrimeList Expr            { AppStx (primesMacro $2 "K") $3 }
  | Expr ':' PrimeList Expr       { AppStx (primesMacro $3 "apply") (SeqStx [$1, $4]) }
  | Expr ':' Expr                 { AppStx $1 $3 }
  | Expr 'o' Expr                 { AppStx (IdStx $2) (SeqStx [$1, $3]) }
  | Expr '*' Expr                 { AppStx (IdStx $2) (SeqStx [$1, $3]) }
  | Expr '/' Expr                 { AppStx (IdStx $2) (SeqStx [$1, $3]) }
  | Expr '+' Expr                 { AppStx (IdStx $2) (SeqStx [$1, $3]) }
  | Expr '-' Expr                 { AppStx (IdStx $2) (SeqStx [$1, $3]) }
  | Expr '=' Expr                 { AppStx (IdStx $2) (SeqStx [$1, $3]) }
  | Expr '=>' Expr                { AppStx (IdStx $2) (SeqStx [$1, $3]) }
  | Expr '<=' Expr                { AppStx (IdStx $2) (SeqStx [$1, $3]) }
  | Expr '&&' Expr                { AppStx (IdStx $2) (SeqStx [$1, $3]) }
  | Expr '||' Expr                { AppStx (IdStx $2) (SeqStx [$1, $3]) }
  | Expr '|' Expr                 { AppStx (IdStx $2) (SeqStx [$1, $3]) }
  | Expr name Expr                { AppStx (IdStx $2) (SeqStx [$1, $3]) }

  | lambda Pat Expr %prec lambda_prec   { lambdaMacro $2 $3 }

  | Expr where '{' DefnList '}'   { WhereStx $1 $4 }

  | ParenExpr { $1 }

ParenExpr:
    '['                    ']'    { AppStx (IdStx "cons") (SeqStx []) }
  | '[' PrimeList          ']'    { AppStx (primesMacro $2 "cons") (SeqStx []) }
  | '['           ExprList ']'    { AppStx (IdStx "cons") (SeqStx $2) }
  | '[' PrimeList ExprList ']'    { AppStx (primesMacro $2 "cons") (SeqStx $3) }
  | '[|'                    '|]'  { AppStx (IdStx "pcons") (SeqStx []) }
  | '[|' PrimeList          '|]'  { AppStx (primesMacro $2 "pcons") (SeqStx []) }
  | '[|'           ExprList '|]'  { AppStx (IdStx "pcons") (SeqStx $2) }
  | '[|' PrimeList ExprList '|]'  { AppStx (primesMacro $2 "pcons") (SeqStx $3) }
  | '(' Expr ')'                  { $2 }

Atom:
    character   { CharStx $1 }
  | integer     { IntStx $1 }
  | double      { DoubleStx $1 }

OpOrName:
    'o'         { $1 }

  | '*'         { $1 }
  | '/'         { $1 }

  | '+'         { $1 }
  | '-'         { $1 }

  | '='         { $1 }

  | '=>'        { $1 }
  | '<='        { $1 }

  | '&&'        { $1 }
  | '||'        { $1 }

  | '|'         { $1 }
     
  | name        { $1 }

  | name '.' OpOrName { $1 ++ "." ++ $3 }

Seq:
    '<' ExprList '>'    { SeqStx $2 }
  | '<'          '>'    { SeqStx [] }
  | string              { stringStx $1 }

Cond:
    Expr '->' PrimeList Expr ';' Expr   { AppStx (primesMacro $3 "cond") (SeqStx [$1, $4, $6]) }
  | Expr '->' PrimeList Expr            { AppStx (primesMacro $3 "cond") (SeqStx [$1, $4]) }
  | Expr '->'           Expr ';' Expr   { applyStx "cond" [$1, $3, $5] }
  | Expr '->'           Expr            { applyStx "cond" [$1, $3] }
  | Pat  '->'           Expr ';' Expr   { ifelseMacro $1 $3 $5 }
  | Pat  '->'           Expr            { ifthenMacro $1 $3 }

Pat:
    PatRest   { $1 }
  | name '@ ' { namePat $1 (mkPat (IdStx "tt") [] []) }

PatNoSpace:
    PatRest   { $1 }
  | name '@'  { namePat $1 (mkPat (IdStx "tt") [] []) }

PatRest:
    OpPat                 { $1 }
  | ConsPat               { $1 }
  | name '@' OpOrName     { namePat $1 (mkPat (IdStx $3) [] []) }
  | name '@' ConsPat      { namePat $1 $3 }
  | name '@' ParenExpr    { namePat $1 (mkPat $3 [] []) }

OpPat:
    Pat 'o' Pat        { let Pat pred _ = $3 in
                         mkPat (IdStx $2) [pred, IdStx "id"] [$1, $3] }
  | Pat '=>' Pat       { mkPat (IdStx $2) [IdStx "hd", IdStx "tl"] [$1, $3] }
  | Pat '<=' Pat       { mkPat (IdStx $2) [IdStx "tlr", IdStx "hdr"] [$1, $3] }
  | Pat '&&' Pat       { mkPat (IdStx $2) [IdStx "id", IdStx "id"] [$1, $3] }
  | Pat '||' Pat       { mkPat (IdStx $2) [IdStx "id", IdStx "id"] [$1, $3] }

ConsPat:
    '[|' PatList '|]'   { mkPat (IdStx "pcons") [ appStx "s" (IntStx i) | i <- [1..length $2] ] $2 }

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

PrimeList :: { Int }
PrimeList:
    PrimeList '\''      { $1 + 1 }
  | '\''                { 1 }


{

parseError :: [Token] -> a
parseError tks = throwParseException $ show tks

data Token
     -- symbols
     = TokenComma
     | TokenLArrow

     -- grouping
     | TokenLParen
     | TokenRParen
     | TokenLConsParen
     | TokenRConsParen
     | TokenLEnvParen
     | TokenREnvParen
     | TokenLPredParen
     | TokenRPredParen
     | TokenLSeqParen
     | TokenRSeqParen

     -- keywords
     | TokenDef
     | TokenExdef
     | TokenLambda
     | TokenMe
     | TokenModule
     | TokenNrdef
     | TokenType
     | TokenUse
     | TokenWhere

     -- literals
     | TokenChar Char
     | TokenInt  Int
     | TokenDouble Double
     | TokenString String

     -- operators
     | TokenPrime

     | TokenTilde

     | TokenColon

     | TokenDot

     | TokenComposition String

     | TokenPatComposition String

     | TokenMult String
     | TokenDiv String

     | TokenAdd String
     | TokenSub String

     | TokenEq String

     | TokenPredAppendR String
     | TokenPredAppendL String

     | TokenPatAppendR String
     | TokenPatAppendL String

     | TokenAnd String
     | TokenOr String

     | TokenPatAnd String
     | TokenPatOr String

     | TokenRArrow
     | TokenSemicolon

     | TokenReverse String

     | TokenEquiv

     | TokenAt
     | TokenAtSpace

     -- identifier
     | TokenName String
       deriving Show


parsePrelude = parseSrcFile

parseFile tks =
  let SrcFile name deps Nothing (Left (Namespace uses stxs)) = parseSrcFile tks in
  SrcFile name deps Nothing (Left (Namespace ("Core":"Prelude":uses) stxs))
}
