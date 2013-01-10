module Printer.PrettyStx where

import Control.Monad.State

import Data.Stx
import Printer.Printer


printNamespaceM :: Namespace String -> PrinterM ()
printNamespaceM (Namespace uses stxs) =
    mapM_ (putPrinter . ("use " ++)) uses
    where printStxs [] = return ()
          printStxs (stx:stxs) =
              do printStxM stx
                 nlPrinter
                 nlPrinter
                 printStxs stxs


printStxM :: Stx String -> PrinterM ()
printStxM (CharStx c) = putPrinter $ show c
printStxM (IntStx i) = putPrinter $ show i
printStxM (DoubleStx d) = putPrinter $ show d

printStxM (SeqStx stxs) | not (null stxs) && all isCharStx stxs =
    putPrinter $ show $ map (\(CharStx c) -> c) stxs

printStxM (SeqStx stxs) =
    do putPrinter "<"
       printStxs stxs
       putPrinter ">"
    where printStxs [] = return ()
          printStxs [stx] = printStxM stx
          printStxs (stx:stxs) =
              do printStxM stx
                 putPrinter ","
                 printStxs stxs

printStxM (IdStx str) = putPrinter str

printStxM (AppStx stx1 stx2) =
    do printApp stx1
       putPrinter ":"
       printApp stx2
    where printApp stx@(CharStx _) = printStxM stx
          printApp stx@(IntStx _) = printStxM stx
          printApp stx@(DoubleStx _) = printStxM stx
          printApp stx@(SeqStx _) = printStxM stx
          printApp stx@(IdStx _) = printStxM stx
          printApp stx =
              do putPrinter "("
                 printStxM stx
                 putPrinter ")"

printStxM (DefnStx kw str body) =
    do putPrinter $ kw' kw ++ " " ++ str ++ " = "
       printStxM body
    where kw' Def = "def"
          kw' NrDef = "nrdef"

printStxM (LambdaStx str body) =
    do putPrinter $ "λ " ++ str ++ " -> "
       withPrinterCol $ printStxM body

printStxM (ModuleStx str ns) =
    do let str' | str == "" = str
                | otherwise = str ++ " "
       putPrinter $ "module " ++ str' ++ "where {"
       withPrinterCol $ do
         nlPrinter
         printNamespaceM ns
       putPrinter "}"

printStxM (TypeStx name stxs) =
    do putPrinter $ "type " ++ name
       withPrinterCol $ do
         nlPrinter
         printStxs stxs
    where printStxs [] = return ()
          printStxs [stx] = printStxM stx
          printStxs (stx:stxs) =
              do printStxM stx
                 nlPrinter
                 printStxs stxs

printStxM (TypeMkStx name arg) =
    putPrinter $ "mk:<" ++ show name ++ "," ++ arg ++ ">"

printStxM (TypeUnStx name arg) =
    putPrinter $ "un:<" ++ show name ++ "," ++ arg ++ ">"

printStxM (TypeIsStx name arg) =
    putPrinter $ "is:<" ++ show name ++ "," ++ arg ++ ">"

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


prettyPrint :: Stx String -> IO ()
prettyPrint stx =
    runStateT (printStxM stx) (PrinterState 0) >> return ()