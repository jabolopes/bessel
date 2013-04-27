module Printer.Printer where

import Control.Monad.State


data PrinterState = PrinterState Int
type PrinterM a = StateT PrinterState IO a


getPrinterCol :: PrinterM Int
getPrinterCol =
    do PrinterState col <- get
       return col


putPrinterCol :: Int -> PrinterM ()
putPrinterCol = put . PrinterState


putPrinter :: String -> PrinterM ()
putPrinter = lift . putStr


nlPrinter :: PrinterM ()
nlPrinter =
    do col <- getPrinterCol
       putPrinter "\n"
       putPrinter (replicate col ' ')


withPrinterCol :: PrinterM a -> PrinterM ()
withPrinterCol m =
    do col <- getPrinterCol
       putPrinterCol (col + 2)
       m
       putPrinterCol col