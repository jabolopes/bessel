module Main where

import System.Environment

import qualified Core (srcfile)
import qualified CoreTypechecker (srcfile)
import qualified Core.Environment (link)
import qualified Core.IO (link)
import qualified Core.Shell (link)

import qualified Core.Happstack (srcfile)

import Data.SrcFile
import Repl


corefiles :: [SrcFile]
corefiles =
    let
        corefile | doTypecheck = CoreTypechecker.srcfile
                 | otherwise = Core.srcfile
    in
      [corefile, Core.Happstack.srcfile]


linkCorefiles :: [Int] -> [[Int] -> (SrcFile, [Int])] -> [SrcFile]
linkCorefiles _ [] = []
linkCorefiles ids (fn:fns) =
    let (srcfile, ids') = fn ids in
    srcfile:linkCorefiles ids' fns


links :: [[Int] -> (SrcFile, [Int])]
links = [Core.Environment.link,
         Core.IO.link,
         Core.Shell.link]


corefiles' :: [SrcFile]
corefiles' = corefiles ++ linkCorefiles (map (* (-1)) [1..]) links


preludeName :: String
preludeName | doTypecheck = "PreludeType"
            | otherwise = "Prelude"


main :: IO ()
main =
    do args <- getArgs
       case args of
         [] -> importFile corefiles' preludeName >>= repl
         filename:_ -> importFile corefiles' filename >>= batch "main:<>"