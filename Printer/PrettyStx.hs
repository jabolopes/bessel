{-# LANGUAGE NamedFieldPuns #-}
module Printer.PrettyStx where

import Control.Monad.State
import Data.Functor ((<$>))
import Data.List (intercalate)

import Data.SrcFile
import Data.Stx
import Printer.Printer


printPatM pat =
    do case patDefns pat of
         [] -> return ()
         (arg, _):_ -> putPrinter arg
       putPrinter "@("
       printStxM (patPred pat)
       putPrinter ")"


printPatsM :: [Pat String] -> PrinterM ()
printPatsM [] = return ()
printPatsM [pat] = printPatM pat

printPatsM (pat:pats) =
    do printPatM pat
       putPrinter " "
       printPatsM pats


printStxM :: Stx String -> PrinterM ()
printStxM (CharStx c) = putPrinter (show c)
printStxM (IntStx i) = putPrinter (show i)
printStxM (DoubleStx d) = putPrinter (show d)

printStxM (SeqStx stxs) | not (null stxs) && all isCharStx stxs =
    putPrinter $ show $ map (\(CharStx c) -> c) stxs

printStxM (SeqStx stxs) =
    do putPrinter "["
       printStxs stxs
       putPrinter "]"
    where printStxs [] = return ()
          printStxs [stx] = printStxM stx
          printStxs (stx:stxs) =
              do printStxM stx
                 putPrinter ","
                 printStxs stxs

printStxM (IdStx str) =
    putPrinter str

printStxM (AppStx stx1 stx2) =
    do printApp stx1
       putPrinter " "
       printApp stx2
    where printApp stx
              | isValueStx stx && not (isLambdaStx stx) = printStxM stx
              | otherwise =
                  do putPrinter "("
                     printStxM stx
                     putPrinter ")"

printStxM (CondMacro ms blame) =
  withPrinterCol $ do
    nlPrinter
    putMatches ms
    nlPrinter
    putPrinter ("_ -> blame " ++ show blame)
    where putMatches [] = return ()
          putMatches [(pats, stx)] =
              do printPatsM pats
                 putPrinter " ("
                 printStxM stx
                 putPrinter ")"
          putMatches ((pats, stx):ms) =
              do printPatsM pats
                 putPrinter " "
                 printStxM stx
                 putPrinter ")"
                 nlPrinter
                 putMatches ms

printStxM (CondStx ms blame) =
    do putPrinter "cond "
       withPrinterCol $ do
         nlPrinter
         putMatches (ms ++ [(IdStx "_", appStx "signal" (stringStx blame))])
    where putMatches [] = return ()

          putMatches [(pred, val)] =
              do printStxM pred
                 putPrinter " -> "
                 printStxM val

          putMatches ((pred, val):ms) =
              do putMatches [(pred, val)]
                 nlPrinter
                 putMatches ms

printStxM (DefnStx ann kw name body) =
    do putAnn ann
       putPrinter $ kw' kw ++ " " ++ name ++ " = "
       printStxM body
    where kw' Def = "def"
          kw' NrDef = "nrdef"

          putAnn Nothing = return ()
          putAnn (Just t) =
              do putPrinter $ "sig " ++ name ++ " : " ++ show t
                 nlPrinter

printStxM (LambdaMacro pats body) =
    do printPatsM pats
       putPrinter " = "
       printStxM body

printStxM (LambdaStx str t body) =
    do putPrinter $ "\\" ++ str ++ putT t ++ " -> "
       withPrinterCol $ printStxM body
    where putT Nothing = ""
          putT (Just t) = " : " ++ t

printStxM (MergeStx vals) =
    do putPrinter "{"
       loop vals
       putPrinter "}"
    where loop [] = return ()
          loop [(name, stx)] =
              do putPrinter $ name ++ " = "
                 printStxM stx
          loop ((name, stx):vals) =
              do putPrinter $ name ++ " = "
                 printStxM stx
                 putPrinter ", "
                 loop vals

printStxM (WhereStx stx stxs) =
    do printStxM stx
       nlPrinter
       putPrinter "where"
       withPrinterCol $ do
         nlPrinter
         printStxs stxs
    where printStxs [] = return ()
          printStxs [stx] = printStxM stx
          printStxs (stx:stxs) =
              do printStxM stx
                 nlPrinter
                 printStxs stxs


initialPrinterState = PrinterState 0


prettyPrint :: Stx String -> IO ()
prettyPrint stx =
  void $ runStateT (printStxM stx) initialPrinterState


prettyPrintSrcFile :: SrcFile -> IO ()
prettyPrintSrcFile SrcFile { t = CoreT, name } =
  void $ runStateT (do putPrinter $ "me " ++ name
                       nlPrinter) initialPrinterState

prettyPrintSrcFile SrcFile { name, decls = stxs } =
  void $ runStateT (printSrcFileM name stxs) initialPrinterState
  where printStxs [] = return ()
        printStxs [stx] = printStxM stx >> nlPrinter
        printStxs (stx:stxs) =
          do printStxM stx
             nlPrinter
             nlPrinter
             printStxs stxs

        printSrcFileM name stxs =
          do putPrinter $ "me " ++ name
             nlPrinter
             nlPrinter
             printStxs stxs