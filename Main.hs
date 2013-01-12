module Main where

import System.Environment

import qualified Core (srcfile)
import qualified CoreTypechecker (srcfile)
import qualified Core.Environment (srcfile)
import qualified Core.IO (srcfile)
import qualified Core.Shell (srcfile)
import Data.SrcFile
import Repl


corefiles :: [SrcFile]
corefiles =
    let corefile | doTypecheck = CoreTypechecker.srcfile
                 | otherwise = Core.srcfile
    in
      [corefile,
       Core.Environment.srcfile,
       Core.IO.srcfile,
       Core.Shell.srcfile]


preludeName :: String
preludeName =
    if doTypecheck then
        "PreludeType"
    else
        "Prelude"


main :: IO ()
main =
    do args <- getArgs
       case args of
         [] -> importFile corefiles preludeName >>= repl
         filenames -> importFiles corefiles filenames >>= batch "main"