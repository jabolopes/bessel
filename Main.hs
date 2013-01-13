module Main where

import System.Environment

import qualified Core (srcfile)
import qualified CoreTypechecker (srcfile)
import qualified Core.Environment (srcfile)
import qualified Core.IO (srcfile)
import qualified Core.Shell (srcfile)

import qualified Core.Happstack (srcfile)

import Data.SrcFile
import Repl


corefiles :: [SrcFile]
corefiles =
    let
        corefile | doTypecheck = CoreTypechecker.srcfile
                 | otherwise = Core.srcfile
    in
      [corefile,
       Core.Environment.srcfile,
       Core.IO.srcfile,
       Core.Shell.srcfile,

       Core.Happstack.srcfile]


preludeName :: String
preludeName | doTypecheck = "PreludeType"
            | otherwise = "Prelude"


main :: IO ()
main =
    do args <- getArgs
       case args of
         [] -> importFile corefiles preludeName >>= repl
         filename:_ -> importFile corefiles filename >>= batch "main:<>"