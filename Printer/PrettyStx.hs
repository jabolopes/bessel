module Printer.PrettyStx where

import Control.Monad.State

import Data.Stx
import Printer.Printer


printStx :: Stx String -> PrinterM ()
printStx (CharStx c) = putPrinter $ show c
printStx (IntStx i) = putPrinter $ show i
printStx (DoubleStx d) = putPrinter $ show d

printStx (SeqStx stxs) | not (null stxs) && all isCharStx stxs =
    putPrinter $ show $ map (\(CharStx c) -> c) stxs

printStx (SeqStx stxs) =
    do putPrinter "<"
       printStxs stxs
       putPrinter ">"
    where printStxs [] = return ()
          printStxs [stx] = printStx stx
          printStxs (stx:stxs) =
              do printStx stx
                 putPrinter ","
                 printStxs stxs

printStx (IdStx str) = putPrinter str

printStx (AppStx stx1 stx2) =
    do printApp stx1
       putPrinter ":"
       printApp stx2
    where printApp stx@(CharStx _) = printStx stx
          printApp stx@(IntStx _) = printStx stx
          printApp stx@(DoubleStx _) = printStx stx
          printApp stx@(SeqStx _) = printStx stx
          printApp stx@(IdStx _) = printStx stx
          printApp stx =
              do putPrinter "("
                 printStx stx
                 putPrinter ")"

printStx (DefnStx kw str body) =
    do putPrinter $ kw' kw ++ " " ++ str ++ " = "
       printStx body
    where kw' Def = "def"
          kw' NrDef = "nrdef"

printStx (LambdaStx str body) =
    do putPrinter $ "λ " ++ str ++ " -> "
       withPrinterCol $ printStx body

printStx (ModuleStx str (_, stxs)) =
    do putPrinter $ "module " ++ str
       nlPrinter
       printStxs stxs
    where printStxs [] = return ()
          printStxs [stx] = printStx stx
          printStxs (stx:stxs) =
              do printStx stx
                 nlPrinter
                 nlPrinter
                 printStxs stxs

printStx (TypeStx name stxs) =
    do putPrinter $ "type " ++ name
       withPrinterCol $ do
         nlPrinter
         printStxs stxs
    where printStxs [] = return ()
          printStxs [stx] = printStx stx
          printStxs (stx:stxs) =
              do printStx stx
                 nlPrinter
                 printStxs stxs

printStx (TypeMkStx name arg) =
    putPrinter $ "mk:<" ++ show name ++ "," ++ arg ++ ">"

printStx (TypeUnStx name arg) =
    putPrinter $ "un:<" ++ show name ++ "," ++ arg ++ ">"

printStx (TypeIsStx name arg) =
    putPrinter $ "is:<" ++ show name ++ "," ++ arg ++ ">"

printStx (WhereStx stx (_, stxs)) =
    do printStx stx
       nlPrinter
       putPrinter "where"
       withPrinterCol $ do
         nlPrinter
         printStxs stxs
    where printStxs [] = return ()
          printStxs [stx] = printStx stx
          printStxs (stx:stxs) =
              do printStx stx
                 nlPrinter
                 printStxs stxs


prettyPrint :: Stx String -> IO ()
prettyPrint stx =
    runStateT (printStx stx) (PrinterState 0) >> return ()