module Main where

import System.Environment

import qualified Core (srcfile)
import qualified CoreTypechecker (srcfile)
import qualified Core.Environment (link)
import qualified Core.IO (link)
import qualified Core.Shell (link)

import qualified Core.Happstack (link)

import Data.SrcFile
import Repl


corefiles :: SrcFile
corefiles | doTypecheck = CoreTypechecker.srcfile
          | otherwise = Core.srcfile


linkCorefiles :: [Int] -> [[Int] -> (SrcFile, [Int])] -> [SrcFile]
linkCorefiles _ [] = []
linkCorefiles ids (fn:fns) =
    let (srcfile, ids') = fn ids in
    srcfile:linkCorefiles ids' fns


links :: [[Int] -> (SrcFile, [Int])]
links = [Core.Environment.link,
         Core.IO.link,
         Core.Shell.link,

         Core.Happstack.link]


corefiles' :: [SrcFile]
corefiles' = corefiles:linkCorefiles (map (* (-1)) [1..]) links


preludeName :: String
preludeName | doTypecheck = "PreludeTypechecker"
            | otherwise = "Prelude"


main :: IO ()
main =
    do args <- getArgs
       case args of
         [] -> importFile corefiles' preludeName >>= repl
         filename:_ -> importFile corefiles' filename >>= batch "main:<>"