module Main where

import qualified Core (srcfile)
import qualified CoreTypechecker (srcfile)
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
    importFile corefiles preludeName >>= repl