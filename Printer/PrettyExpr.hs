{-# LANGUAGE NamedFieldPuns #-}
module Printer.PrettyExpr where

import Control.Monad.State
import Data.Functor ((<$>))
import Data.List (intercalate)

import Data.SrcFile
import Data.Expr
import Printer.Printer
import Data.QualName (QualName)
import qualified Data.QualName as QualName (fromQualName)


printNameM :: QualName -> PrinterM ()
printNameM name =
  putPrinter (QualName.fromQualName name)


printPatM pat =
  do case patDefns pat of
       [] -> return ()
       (arg, _):_ -> putPrinter arg
     putPrinter "@("
     printExprM (patPred pat)
     putPrinter ")"


printPatsM :: [Pat] -> PrinterM ()
printPatsM [] = return ()
printPatsM [pat] = printPatM pat

printPatsM (pat:pats) =
    do printPatM pat
       putPrinter " "
       printPatsM pats


printExprM :: Expr -> PrinterM ()
printExprM (IdE name) = printNameM name
printExprM (CharE c) = putPrinter (show c)
printExprM (IntE i) = putPrinter (show i)
printExprM (RealE d) = putPrinter (show d)

printExprM (SeqE exprs) | not (null exprs) && all isCharE exprs =
    putPrinter $ show $ map (\(CharE c) -> c) exprs

printExprM (SeqE exprs) =
    do putPrinter "["
       printExprs exprs
       putPrinter "]"
    where printExprs [] = return ()
          printExprs [expr] = printExprM expr
          printExprs (expr:exprs) =
              do printExprM expr
                 putPrinter ","
                 printExprs exprs

printExprM (AppE expr1 expr2) =
    do printApp expr1
       putPrinter " "
       printApp expr2
    where printApp expr
              | isValueE expr && not (isLambdaE expr) = printExprM expr
              | otherwise =
                  do putPrinter "("
                     printExprM expr
                     putPrinter ")"

printExprM (CondMacro ms blame) =
  do putMatches ms
     nlPrinter
     putPrinter ("_ -> blame " ++ show blame)
  where putMatches [] = return ()
        putMatches [(pats, expr)] =
          do printPatsM pats
             putPrinter " ("
             printExprM expr
             putPrinter ")"
        putMatches ((pats, expr):ms) =
          do printPatsM pats
             putPrinter " "
             printExprM expr
             putPrinter ")"
             nlPrinter
             putMatches ms

printExprM (CondE ms blame) =
    do putPrinter "cond "
       withPrinterCol $ do
         nlPrinter
         putMatches (ms ++ [(idE "_", appE "signal" (stringE blame))])
    where putMatches [] = return ()

          putMatches [(pred, val)] =
              do printExprM pred
                 putPrinter " -> "
                 printExprM val

          putMatches ((pred, val):ms) =
              do putMatches [(pred, val)]
                 nlPrinter
                 putMatches ms

printExprM (FnDecl ann kw name body) =
    do putPrinter $ showKw kw ++ " " ++ name
       putAnn ann
       withPrinterCol $ do
         nlPrinter
         printExprM body
    where showKw Def = "def"
          showKw NrDef = "nrdef"

          putAnn Nothing = return ()
          putAnn (Just t) = putPrinter $ " : " ++ show t

printExprM (LambdaMacro pats body) =
    do printPatsM pats
       putPrinter " = "
       printExprM body

printExprM (LambdaE str t body) =
    do putPrinter $ "\\" ++ str ++ putT t ++ " -> "
       withPrinterCol $ printExprM body
    where putT Nothing = ""
          putT (Just t) = " : " ++ t

printExprM (MergeE vals) =
    do putPrinter "{"
       loop vals
       putPrinter "}"
    where loop [] = return ()
          loop [(name, expr)] =
              do printNameM name
                 putPrinter " = "
                 printExprM expr
          loop ((name, expr):vals) =
              do printNameM name
                 putPrinter " = "
                 printExprM expr
                 putPrinter ", "
                 loop vals

printExprM (WhereE expr exprs) =
    do printExprM expr
       nlPrinter
       putPrinter "where"
       withPrinterCol $ do
         nlPrinter
         printExprs exprs
    where printExprs [] = return ()
          printExprs [expr] = printExprM expr
          printExprs (expr:exprs) =
              do printExprM expr
                 nlPrinter
                 printExprs exprs


initialPrinterState = PrinterState 0


prettyPrint :: Expr -> IO ()
prettyPrint expr =
  void $ runStateT (printExprM expr) initialPrinterState


prettyPrintSrcFile :: SrcFile -> IO ()
prettyPrintSrcFile SrcFile { t = CoreT, name } =
  void $ runStateT (do putPrinter $ "me " ++ name
                       nlPrinter) initialPrinterState

prettyPrintSrcFile SrcFile { name, decls = exprs } =
  void $ runStateT (printSrcFileM name exprs) initialPrinterState
  where printExprs [] = return ()
        printExprs [expr] = printExprM expr >> nlPrinter
        printExprs (expr:exprs) =
          do printExprM expr
             nlPrinter
             nlPrinter
             printExprs exprs

        printSrcFileM name exprs =
          do putPrinter $ "me " ++ name
             nlPrinter
             nlPrinter
             printExprs exprs
