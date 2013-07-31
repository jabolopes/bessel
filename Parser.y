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


checkSigDefName :: String -> String -> ParserM ()
checkSigDefName sigName defName
  | sigName == defName =
      return ()
  | otherwise =
      failM $ "Function names are not equal in" ++
              "\n  sig " ++ sigName ++ " ..." ++
              "\nand" ++
              "\n  def " ++ defName ++ " ..."
}

%monad { ParserM }
%lexer { nextToken } { TokenEOF }

%name parseSrcFile SrcFile
%name parseDefnOrExpr DefnOrExpr

%tokentype { Token }
%error { parseError }

%token
  -- punctuation
  '&'     { TokenAmpersand }
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
    TypeAnn FnDefn {% let
                        (name, ann) = $1
                        (kw, name', body) = $2
                      in
                       checkSigDefName name name' >>
                       (return $ FnDecl (Just ann) kw name body) }
  | FnDefn         { let (kw, name, body) = $1 in
                     FnDecl Nothing kw name body }
  | TypeDefn       { $1 }

TypeAnn:
    sig Name ':' Type { ($2, $4) }

FnDefn:
    def Name TypePatList DefnMatches { (Def, $2, LambdaMacro $3 (CondMacro $4 $2)) }
  | def Name TypePatList '=' Expr    { (Def, $2, LambdaMacro $3 $5) }
  | def Name DefnMatches             { (Def, $2, CondMacro $3 $2) }
  | def Name '=' Expr                { (Def, $2, $4) }

TypeDefn:
    type Cotype { CotypeDecl $2 }

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

  | Lambda          { $1 }
  | Merge           { $1 }

Lambda:
    TypePatList LambdaMatches { LambdaMacro $1 (CondMacro $2 "lambda") }
  | TypePatList SimpleExpr    { LambdaMacro $1 $2 }
  | LambdaMatches             { CondMacro $1 "lambda" }

LambdaMatches:
    LambdaMatches '|' PatList SimpleExpr { $1 ++ [($3, $4)] }
  | PatList SimpleExpr                   { [($1, $2)] }

TypePatList:
    TypePatList TypePat { $1 ++ [$2] }
  | TypePat             { [$1] }

PatList:
    PatList Pat         { $1 ++ [$2] }
  | Pat                 { [$1] }

Merge:
    '{' MergeObservations '}' { MergeE (sortWith fst $2) }

MergeObservations:
    MergeObservations '&' QualName '=' Expr { $1 ++ [($3, $5)] }
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
  | '['          ']' { SeqE [] }
  | string           { stringE $1 }


-- patterns

TypePat:
    id '@' typeId  { namePat $1 (mkPredPat (idE $3)) }

Pat:
    '@ '         { mkPredPat constTrueE }
  | id '@ '      { namePat $1 (mkPredPat constTrueE) }
  | PatRest      { $1 }

PatNoSpace:
    '@'         { mkPredPat constTrueE }
  | id '@'      { namePat $1 (mkPredPat constTrueE) }
  | PatRest     { $1 }

PatRest:
-- expression patterns
    '@' '(' Expr ')'     { mkPredPat $3 }
  | id '@' '(' Expr ')'  { namePat $1 (mkPredPat $4) }

-- predicate patterns
  | '@' QualName         { mkPredPat (IdE $2) }
  | id '@' QualName      { namePat $1 (mkPredPat (IdE $3)) }

-- list patterns
  | ListPat              { $1 }
  | id '@' ListPat       { namePat $1 $3 }

-- combined patterns
  | '(' CombPat ')'        { $2 }
  | id '@' '(' CombPat ')' { namePat $1 $4 }

CombPat:
    Pat '+>' PatNoSpace { mkPat (idE "pal") [idE "hd", idE "tl"] [$1, $3] }
  | Pat '<+' PatNoSpace { mkPat (idE "par") [idE "tlr", idE "hdr"] [$1, $3] }
  | Pat '&&' PatNoSpace { mkPat (idE "pand") [idE "id", idE "id"] [$1, $3] }
  | Pat '||' PatNoSpace { mkPat (idE "por") [idE "id", idE "id"] [$1, $3] }

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

Type:
    TypeAux { rebuildForallT $1 }

TypeAux:
    TypeAux '->' TypeAux { ArrowT $1 $3 }
  | '(' TypeList ')'     { TupT $2 }
  | '[' TypeAux ']'      { SeqT $2 }
  | Cotype               { $1 }
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

Cotype:
    '{' CotypeObservations '}' { CoT (sortWith fst $2) }

CotypeObservations:
    CotypeObservations '|' QualName ':' Type { $1 ++ [($3, $5)] }
  | QualName ':' Type                        { [($1, $3)] }


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
