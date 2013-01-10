module Main where

import qualified Core (srcfile)
import qualified Core.IO (srcfile)
import Repl


main :: IO ()
main = importFile [Core.srcfile, Core.IO.srcfile] "Prelude" >>= repl