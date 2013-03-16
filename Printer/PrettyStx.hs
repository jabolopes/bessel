{-# LANGUAGE NamedFieldPuns #-}
module Printer.PrettyStx where

import Control.Monad.State
import Data.Functor ((<$>))
import Data.List (intercalate)

import Data.SrcFile
import Data.Stx
import Printer.Printer


printNamespaceM :: Namespace String -> PrinterM ()
printNamespaceM (Namespace uses stxs) =
    do forM_ uses $ \use -> do
         case use of
           (ns, "") -> putPrinter $ "use " ++ ns
           (ns, prefix) -> putPrinter $ "use " ++ ns ++ " as " ++ prefix
         nlPrinter
       unless (null uses) nlPrinter
       printStxs stxs
    where printStxs [] = return ()
          printStxs [stx] = printStxM stx >> nlPrinter
          printStxs (stx:stxs) =
              do printStxM stx
                 nlPrinter
                 nlPrinter
                 printStxs stxs


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

printStxM (CondStx ms blame) =
    do putPrinter "cond "
       withPrinterCol $ do
         nlPrinter
         putMatches (ms ++ [(IdStx "_", appStx "signal" (stringStx blame))])
    where putMatches [] = return ()

          putMatches [(pred, expr)] =
              do printStxM pred
                 putPrinter " -> "
                 printStxM expr

          putMatches ((pred, expr):ms) =
              do printStxM pred
                 putPrinter " -> "
                 printStxM expr
                 nlPrinter
                 putMatches ms

printStxM (DefnStx kw str body) =
    do putPrinter $ kw' kw ++ " " ++ str ++ " = "
       printStxM body
    where kw' Def = "def"
          kw' NrDef = "nrdef"

printStxM (LambdaStx str body) =
    do putPrinter $ "\\" ++ str ++ " -> "
       withPrinterCol $ printStxM body

printStxM (ModuleStx prefix ns) =
    do let prefix' | null prefix = ""
                   | otherwise = intercalate "." prefix ++ " "
       putPrinter $ "module " ++ prefix' ++ "where {"
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

printStxM (TypeMkStx name) =
    putPrinter $ "mk' " ++ name

printStxM (TypeUnStx) =
    putPrinter $ "un"

printStxM (TypeIsStx name) =
    putPrinter $ "is' " ++ name

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
    runStateT (printStxM stx) initialPrinterState >> return ()


prettyPrintNamespace :: Namespace String -> IO ()
prettyPrintNamespace ns =
    runStateT (printNamespaceM ns) initialPrinterState >> return ()


prettyPrintSrcFile :: SrcFile -> IO ()
prettyPrintSrcFile SrcFile { t = CoreT, name } =
    runStateT (do putPrinter $ "me " ++ name
                  nlPrinter) initialPrinterState >> return ()

prettyPrintSrcFile SrcFile { name, renNs = Just ns } =
    runStateT (printSrcFileM name ns) initialPrinterState >> return ()
    where printSrcFileM name ns =
              do putPrinter $ "me " ++ name
                 nlPrinter
                 nlPrinter
                 printNamespaceM ns