module Printer.Printer where

import Control.Monad.State


data PrinterState = PrinterState Int
type PrinterM a = StateT PrinterState IO a


getPrinterCol :: PrinterM Int
getPrinterCol =
    do PrinterState col <- get
       return col


putPrinterCol :: Int -> PrinterM ()
putPrinterCol col = put $ PrinterState col


putPrinter :: String -> PrinterM ()
putPrinter str = lift $ putStr str


nlPrinter :: PrinterM ()
nlPrinter =
    do col <- getPrinterCol
       putPrinter "\n"
       putPrinter $ replicate col ' '


withPrinterCol :: PrinterM a -> PrinterM ()
withPrinterCol m =
    do col <- getPrinterCol
       putPrinterCol $ col + 2
       m
       putPrinterCol col