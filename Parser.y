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
        '::'    { TokenColonColon }
        ','     { TokenComma }
        '=f=>'  { TokenFnMetaPred }
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
        asn     { TokenAsn }
        def     { TokenDef }
        exdef   { TokenExdef }
        export  { TokenExport }
        forall  { TokenForall }
        hide    { TokenHide }
        lambda  { TokenLambda }
        meta    { TokenMeta }
        me      { TokenMe }
        module  { TokenModule }
        nrdef   { TokenNrdef }
        pat     { TokenPat }
        rec     { TokenRec }
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
        '\''    { TokenPrime }

        '~'     { TokenTilde }

        ':'     { TokenColon }

        '.'     { TokenPeriod }
        '. '    { TokenPeriodSpace }

        'o'     { TokenComposition $$ }

        'o.'    { TokenPatComposition $$ }

        '*'     { TokenMult $$ }
        '/'     { TokenDiv $$ }

        '+'     { TokenAdd $$ }
        '-'     { TokenSub $$ }

        '='     { TokenEq $$ }

        '=>'   { TokenPredAppendR $$ }
        '<='   { TokenPredAppendL $$ }

        '=>.'   { TokenPatAppendR $$ }
        '<=.'   { TokenPatAppendL $$ }

        '&&'    { TokenAnd $$ }
        '||'    { TokenOr $$ }

        '&&.'   { TokenPatAnd $$ }
        '||.'   { TokenPatOr $$ }

        '->'    { TokenRArrow }
        ';'     { TokenSemicolon }

        '|'     { TokenReverse $$ }

        ':='    { TokenEquiv }

        '@'     { TokenAt $$ }

        -- identifier
        name      { TokenName $$ }


-- Precedence (lower)
                        -- definition
%left lambda_prec       -- lambda
%left where             -- where
%left '|'               -- reverse composition
%right '->' ';'         -- condition
%left name              -- infix name op
%left '&&.' '||.'       -- pattern logical
%left '&&' '||'         -- logical
%left '=>.' '<=.'       -- pattern appends
%left '=>' '<='         -- predicate appends
%left '->' '<-'         -- arrows
%left '='               -- equality
%left '+' '-'           -- additives
%left '*' '/'           -- multiplicatives
%left 'o.'              -- pattern composition
%left 'o'               -- composition
%left '.' '. '          -- pattern
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

  | exdef  DefnPat  DefnPatList '<-' Expr ':=' Expr    { case exdefExprMacro $2 $3 $5 $7 of
                                                           [] -> error "exdef: empty pattern"
                                                           stxs -> SeqStx stxs }
  | exdef  DefnPat  DefnPatList '<-' Pat  ':=' Expr    { case exdefPatMacro $2 $3 $5 $7 of
                                                           [] -> error "exdef: empty pattern"
                                                           stxs -> SeqStx stxs }
  | exdef  DefnPat  DefnPatList           ':=' Expr    { case exdefMacro $2 $3 $5 of
                                                           [] -> error "exdef: empty pattern"
                                                           stxs -> SeqStx stxs }
  | exdef  DefnPat              '<-' Expr ':=' Expr    { case exdefExprMacro $2 [] $4 $6 of
                                                           [] -> error "exdef: empty pattern"
                                                           stxs -> SeqStx stxs }
  | exdef  DefnPat              '<-' Pat  ':=' Expr    { case exdefPatMacro $2 [] $4 $6 of
                                                           [] -> error "exdef: empty pattern"
                                                           stxs -> SeqStx stxs }
  | exdef  DefnPat                        ':=' Expr    { case exdefMacro $2 [] $4 of
                                                           [] -> error "exdef: empty pattern"
                                                           stxs -> SeqStx stxs }

  | type  OpOrName                        ':=' Expr        { typeExprMacro $2 $4 }
  | type  OpOrName                        ':=' Pat         { typePatMacro $2 $4 }
--| asn   Expr         ':=' Expr        { AsnStx  $2 $4 }
--| sig   OpOrName     	    		  '::' MetaPred    { SigStx  $2 $4 }

DefnKw:
    def      { Def }
  | nrdef    { NrDef }

DefnPat:
    name '.' DefnPat    { namePat $1 $3 }
  | '(' OpPat ')'       { $2 }
  | NamePat             { $1 }
  | ConsPat             { $1 }

ArgExpr:
    DefnPatList                { ($1, Nothing) }
  | DefnPatList '<-' Expr      { ($1, Just (Right $3)) }
  | DefnPatList '<-' Pat       { ($1, Just (Left $3)) }
  | '<-' Expr                  { ([], Just (Right $2)) }
  | '<-' Pat                   { ([], Just (Left $2)) }

DefnPatList:
    DefnPatList DefnPat    { $1 ++ [$2] }
  | DefnPat                { [$1] }

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

  | '['                    ']'    { AppStx (IdStx "cons") (SeqStx []) }
  | '[' PrimeList          ']'    { AppStx (primesMacro $2 "cons") (SeqStx []) }
  | '['           ExprList ']'    { AppStx (IdStx "cons") (SeqStx $2) }
  | '[' PrimeList ExprList ']'    { AppStx (primesMacro $2 "cons") (SeqStx $3) }
  | '[|'                    '|]'  { AppStx (IdStx "pcons") (SeqStx []) }
  | '[|' PrimeList          '|]'  { AppStx (primesMacro $2 "pcons") (SeqStx []) }
  | '[|'           ExprList '|]'  { AppStx (IdStx "pcons") (SeqStx $2) }
  | '[|' PrimeList ExprList '|]'  { AppStx (primesMacro $2 "pcons") (SeqStx $3) }

  | lambda '(' Pat ')' Expr %prec lambda_prec   { lambdaMacro $3 $5 }

  | Expr where '{' DefnList '}'   { WhereStx $1 $4 }

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

  | name '@' OpOrName { $1 ++ $2 ++ $3 }

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
    name '.'            { namePat $1 (mkPat (IdStx "tt") [] []) }
  | name '.' Pat        { namePat $1 $3 }
  | OpPat               { $1 }
  | NamePat             { $1 }
  | ConsPat             { $1 }

OpPat:
    Pat 'o.' Pat        { let Pat pred _ = $3 in
                          mkPat (IdStx $2) [pred, IdStx "id"] [$1, $3] }
  | Pat '=>.' Pat       { mkPat (IdStx $2) [IdStx "hd", IdStx "tl"] [$1, $3] }
  | Pat '<=.' Pat       { mkPat (IdStx $2) [IdStx "tlr", IdStx "hdr"] [$1, $3] }
  | Pat '&&.' Pat       { mkPat (IdStx $2) [IdStx "id", IdStx "id"] [$1, $3] }
  | Pat '||.' Pat       { mkPat (IdStx $2) [IdStx "id", IdStx "id"] [$1, $3] }

NamePat:
    name '. '           { namePat $1 (mkPat (IdStx "tt") [] []) }
  | name '.' Expr       { namePat $1 (mkPat $3 [] []) }

ConsPat:
    '[|' PatList '|]'   { mkPat (IdStx "pcons") [ appStx "s" (IntStx i) | i <- [1..length $2] ] $2 }

PatList:
    ExprList ',' Pat ',' PatList2       { map (\expr -> mkPat expr [] []) $1 ++ [$3] ++ $5 }
  | ExprList ',' Pat                    { map (\expr -> mkPat expr [] []) $1 ++ [$3] }
  | Pat ',' PatList2                    { $1 : $3 }
  | Pat                                 { [$1] }

PatList2:
    Expr ',' PatList2   { mkPat $1 [] [] : $3 }
  | Pat  ',' PatList2   { $1 : $3 }
  | Expr                { [mkPat $1 [] []] }
  | Pat                 { [$1] }

NameList:
    NameList ',' name   { $1 ++ [$3] }
  | name                { [$1] }

MetaPred:
--  Expr                                { }
    name                                { IdStx $1 }
--| meta '(' Expr ')'                   { }
--| MetaPred '=f=>' MetaPred            { FnMetaPred $1 $3 }
  | '[|' MetaPredList '|]'              { SeqStx $2 }
  | '[|' '|]'                           { SeqStx [] }
  | MetaPred '<=' MetaPred              { }
  | MetaPred '=>' MetaPred              { }
--| seqof ':' MetaPred                  { SeqMetaPred $3 }
  | MetaPred '&&' MetaPred              { }
  | MetaPred '||' MetaPred              { }
--| '¬' ':' MetaPred                    { }
  | '(' MetaPred ')'                    { $2 }
--| forall '(' NameList ')' MetaPred    { }

MetaPredList:
    MetaPredList ',' MetaPred   { $1 ++ [$3] }
  | MetaPred                    { [$1] }

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
     = TokenColonColon
     | TokenComma
     | TokenFnMetaPred
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
     | TokenAsn
     | TokenDef
     | TokenExdef
     | TokenExport
     | TokenForall
     | TokenHide
     | TokenLambda
     | TokenMeta
     | TokenMe
     | TokenModule
     | TokenNrdef
     | TokenPat
     | TokenRec
     | TokenSig
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

     | TokenPeriod
     | TokenPeriodSpace

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

     | TokenAt String

     -- identifier
     | TokenName String
       deriving Show


parsePrelude = parseSrcFile

parseFile tks =
  let SrcFile name deps Nothing (Left (Namespace uses stxs)) = parseSrcFile tks in
  SrcFile name deps Nothing (Left (Namespace ("Prelude":uses) stxs))
}
