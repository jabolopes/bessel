module Main where

import Data.Foldable (forM_)
import Data.Functor ((<$>))

import Config
import Data.Exception
import qualified Data.FileSystem as FileSystem (initial)
import Data.SrcFile
import Repl hiding (initialFs)


links :: [[Int] -> (SrcFile, [Int])]
links = []
    -- Core.Environment.link,
    -- Core.IO.link
    -- Core.Math.link,
    -- Core.Shell.link,
    -- Core.Happstack.link


linkCorefiles :: [Int] -> [[Int] -> (SrcFile, [Int])] -> [SrcFile]
linkCorefiles _ [] = []
linkCorefiles ids (fn:fns) =
    let (srcfile, ids') = fn ids in
    srcfile:linkCorefiles ids' fns


corefiles :: [SrcFile]
corefiles = corefile:linkCorefiles (map (* (-1)) [1..]) links


mainException :: UserException -> IO (Maybe a)
mainException e =
    do putUserException e
       return Nothing


main :: IO ()
main =
    do -- args <- getArgs
       mstate <- (Just <$> importFile (FileSystem.initial corefiles) preludeName)
                  `catchUserException` mainException
       forM_ mstate repl
