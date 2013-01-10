module Main where

import qualified Core (srcfile)
import qualified Core.IO (srcfile)
import qualified Core.Shell (srcfile)
import Data.SrcFile
import Repl


corefiles :: [SrcFile]
corefiles = [Core.srcfile,
             Core.IO.srcfile,
             Core.Shell.srcfile]


main :: IO ()
main = importFile corefiles "Prelude" >>= repl